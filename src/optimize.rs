use crate::engine::KEYWORD_DUMP_AST;
use crate::parser::{Expr, Stmt};

fn optimize_stmt(stmt: Stmt, changed: &mut bool, preserve_result: bool) -> Stmt {
    match stmt {
        Stmt::IfElse(expr, stmt1, None) => match *expr {
            Expr::False(pos) => {
                *changed = true;
                Stmt::Noop(pos)
            }
            Expr::True(_) => optimize_stmt(*stmt1, changed, true),
            expr => Stmt::IfElse(
                Box::new(optimize_expr(expr, changed)),
                Box::new(optimize_stmt(*stmt1, changed, true)),
                None,
            ),
        },

        Stmt::IfElse(expr, stmt1, Some(stmt2)) => match *expr {
            Expr::False(_) => optimize_stmt(*stmt2, changed, true),
            Expr::True(_) => optimize_stmt(*stmt1, changed, true),
            expr => Stmt::IfElse(
                Box::new(optimize_expr(expr, changed)),
                Box::new(optimize_stmt(*stmt1, changed, true)),
                Some(Box::new(optimize_stmt(*stmt2, changed, true))),
            ),
        },

        Stmt::While(expr, stmt) => match *expr {
            Expr::False(pos) => {
                *changed = true;
                Stmt::Noop(pos)
            }
            Expr::True(_) => Stmt::Loop(Box::new(optimize_stmt(*stmt, changed, false))),
            expr => Stmt::While(
                Box::new(optimize_expr(expr, changed)),
                Box::new(optimize_stmt(*stmt, changed, false)),
            ),
        },

        Stmt::Loop(stmt) => Stmt::Loop(Box::new(optimize_stmt(*stmt, changed, false))),
        Stmt::For(id, expr, stmt) => Stmt::For(
            id,
            Box::new(optimize_expr(*expr, changed)),
            Box::new(optimize_stmt(*stmt, changed, false)),
        ),
        Stmt::Let(id, Some(expr), pos) => {
            Stmt::Let(id, Some(Box::new(optimize_expr(*expr, changed))), pos)
        }
        Stmt::Let(_, None, _) => stmt,

        Stmt::Block(statements, pos) => {
            let original_len = statements.len();

            let mut result: Vec<_> = statements
                .into_iter() // For each statement
                .rev() // Scan in reverse
                .map(|s| optimize_stmt(s, changed, preserve_result)) // Optimize the statement
                .enumerate()
                .filter(|(i, s)| s.is_op() || (preserve_result && *i == 0)) // Remove no-op's but leave the last one if we need the result
                .map(|(_, s)| s)
                .rev()
                .collect();

            // Remove all raw expression statements that are pure except for the very last statement
            let last_stmt = if preserve_result { result.pop() } else { None };

            result.retain(|stmt| match stmt {
                Stmt::Expr(expr) if expr.is_pure() => false,
                _ => true,
            });

            if let Some(stmt) = last_stmt {
                result.push(stmt);
            }

            // Remove all let statements at the end of a block - the new variables will go away anyway.
            // But be careful only remove ones that have no initial values or have values that are pure expressions,
            // otherwise there may be side effects.
            let mut removed = false;

            while let Some(expr) = result.pop() {
                match expr {
                    Stmt::Let(_, None, _) => removed = true,
                    Stmt::Let(_, Some(val_expr), _) if val_expr.is_pure() => removed = true,

                    _ => {
                        result.push(expr);
                        break;
                    }
                }
            }

            if preserve_result {
                if removed {
                    result.push(Stmt::Noop(pos))
                }

                result = result
                    .into_iter()
                    .rev()
                    .enumerate()
                    .map(|(i, s)| optimize_stmt(s, changed, i == 0)) // Optimize all other statements again
                    .rev()
                    .collect();
            }

            *changed = *changed || original_len != result.len();

            match result[..] {
                // No statements in block - change to No-op
                [] => {
                    *changed = true;
                    Stmt::Noop(pos)
                }
                // Only one statement - promote
                [_] => {
                    *changed = true;
                    result.remove(0)
                }
                _ => Stmt::Block(result, pos),
            }
        }

        Stmt::Expr(expr) => Stmt::Expr(Box::new(optimize_expr(*expr, changed))),

        Stmt::ReturnWithVal(Some(expr), is_return, pos) => Stmt::ReturnWithVal(
            Some(Box::new(optimize_expr(*expr, changed))),
            is_return,
            pos,
        ),

        stmt => stmt,
    }
}

fn optimize_expr(expr: Expr, changed: &mut bool) -> Expr {
    match expr {
        Expr::Stmt(stmt, pos) => match optimize_stmt(*stmt, changed, true) {
            Stmt::Noop(_) => {
                *changed = true;
                Expr::Unit(pos)
            }
            Stmt::Expr(expr) => {
                *changed = true;
                *expr
            }
            stmt => Expr::Stmt(Box::new(stmt), pos),
        },
        Expr::Assignment(id, expr, pos) => {
            Expr::Assignment(id, Box::new(optimize_expr(*expr, changed)), pos)
        }
        Expr::Dot(lhs, rhs, pos) => Expr::Dot(
            Box::new(optimize_expr(*lhs, changed)),
            Box::new(optimize_expr(*rhs, changed)),
            pos,
        ),

        #[cfg(not(feature = "no_index"))]
        Expr::Index(lhs, rhs, pos) => match (*lhs, *rhs) {
            (Expr::Array(mut items, _), Expr::IntegerConstant(i, _))
                if i >= 0 && (i as usize) < items.len() && items.iter().all(|x| x.is_pure()) =>
            {
                // Array where everything is a pure - promote the indexed item.
                // All other items can be thrown away.
                *changed = true;
                items.remove(i as usize)
            }
            (lhs, rhs) => Expr::Index(
                Box::new(optimize_expr(lhs, changed)),
                Box::new(optimize_expr(rhs, changed)),
                pos,
            ),
        },
        #[cfg(feature = "no_index")]
        Expr::Index(_, _, _) => panic!("encountered an index expression during no_index!"),

        #[cfg(not(feature = "no_index"))]
        Expr::Array(items, pos) => {
            let original_len = items.len();

            let items: Vec<_> = items
                .into_iter()
                .map(|expr| optimize_expr(expr, changed))
                .collect();

            *changed = *changed || original_len != items.len();

            Expr::Array(items, pos)
        }

        #[cfg(feature = "no_index")]
        Expr::Array(_, _) => panic!("encountered an array during no_index!"),

        Expr::And(lhs, rhs) => match (*lhs, *rhs) {
            (Expr::True(_), rhs) => {
                *changed = true;
                rhs
            }
            (Expr::False(pos), _) => {
                *changed = true;
                Expr::False(pos)
            }
            (lhs, Expr::True(_)) => {
                *changed = true;
                lhs
            }
            (lhs, rhs) => Expr::And(
                Box::new(optimize_expr(lhs, changed)),
                Box::new(optimize_expr(rhs, changed)),
            ),
        },
        Expr::Or(lhs, rhs) => match (*lhs, *rhs) {
            (Expr::False(_), rhs) => {
                *changed = true;
                rhs
            }
            (Expr::True(pos), _) => {
                *changed = true;
                Expr::True(pos)
            }
            (lhs, Expr::False(_)) => {
                *changed = true;
                lhs
            }
            (lhs, rhs) => Expr::Or(
                Box::new(optimize_expr(lhs, changed)),
                Box::new(optimize_expr(rhs, changed)),
            ),
        },

        // Expr::FunctionCall(id, args, def_value, pos) if id == KEYWORD_DUMP_AST => {
        //     Expr::FunctionCall(id, args, def_value, pos)
        // }
        Expr::FunctionCall(id, args, def_value, pos) => {
            let original_len = args.len();

            let args: Vec<_> = args
                .into_iter()
                .map(|a| optimize_expr(a, changed))
                .collect();

            *changed = *changed || original_len != args.len();

            Expr::FunctionCall(id, args, def_value, pos)
        }

        expr => expr,
    }
}

pub(crate) fn optimize(statements: Vec<Stmt>) -> Vec<Stmt> {
    let mut result = statements;

    loop {
        let mut changed = false;

        result = result
            .into_iter()
            .rev() // Scan in reverse
            .enumerate()
            .map(|(i, stmt)| {
                // Keep all variable declarations at this level
                let keep = stmt.is_var();

                // Always keep the last return value
                optimize_stmt(stmt, &mut changed, keep || i == 0)
            })
            .rev()
            .collect();

        if !changed {
            break;
        }
    }

    // Eliminate No-op's but always keep the last statement
    let last_stmt = result.pop();

    result.retain(Stmt::is_op); // Remove all No-op's

    if let Some(stmt) = last_stmt {
        result.push(stmt); // Add back the last statement
    }

    result
}
