use crate::parser::{Expr, Stmt};

fn optimize_stmt(stmt: Stmt, changed: &mut bool) -> Stmt {
    match stmt {
        Stmt::IfElse(expr, stmt1, None) => match *expr {
            Expr::False(pos) => {
                *changed = true;
                Stmt::Noop(pos)
            }
            Expr::True(_) => optimize_stmt(*stmt1, changed),
            expr => Stmt::IfElse(
                Box::new(optimize_expr(expr, changed)),
                Box::new(optimize_stmt(*stmt1, changed)),
                None,
            ),
        },

        Stmt::IfElse(expr, stmt1, Some(stmt2)) => match *expr {
            Expr::False(_) => optimize_stmt(*stmt2, changed),
            Expr::True(_) => optimize_stmt(*stmt1, changed),
            expr => Stmt::IfElse(
                Box::new(optimize_expr(expr, changed)),
                Box::new(optimize_stmt(*stmt1, changed)),
                Some(Box::new(optimize_stmt(*stmt2, changed))),
            ),
        },

        Stmt::While(expr, stmt) => match *expr {
            Expr::False(pos) => {
                *changed = true;
                Stmt::Noop(pos)
            }
            Expr::True(_) => Stmt::Loop(Box::new(optimize_stmt(*stmt, changed))),
            expr => Stmt::While(
                Box::new(optimize_expr(expr, changed)),
                Box::new(optimize_stmt(*stmt, changed)),
            ),
        },

        Stmt::Loop(stmt) => Stmt::Loop(Box::new(optimize_stmt(*stmt, changed))),
        Stmt::For(id, expr, stmt) => Stmt::For(
            id,
            Box::new(optimize_expr(*expr, changed)),
            Box::new(optimize_stmt(*stmt, changed)),
        ),
        Stmt::Let(id, Some(expr), pos) => {
            Stmt::Let(id, Some(Box::new(optimize_expr(*expr, changed))), pos)
        }
        Stmt::Let(_, None, _) => stmt,

        Stmt::Block(statements, pos) => {
            let original_len = statements.len();

            let mut result: Vec<_> = statements
                .into_iter() // For each statement
                .map(|s| optimize_stmt(s, changed)) // Optimize the statement
                .filter(Stmt::is_op) // Remove no-op's
                .collect();

            if let Some(last_stmt) = result.pop() {
                // Remove all raw expression statements that evaluate to constants
                // except for the very last statement
                result.retain(|stmt| match stmt {
                    Stmt::Expr(expr) if expr.is_constant() => false,
                    _ => true,
                });

                result.push(last_stmt);
            }

            *changed = *changed || original_len != result.len();

            match result[..] {
                [] => {
                    // No statements in block - change to No-op
                    *changed = true;
                    Stmt::Noop(pos)
                }
                [Stmt::Let(_, None, _)] => {
                    // Only one empty variable declaration - change to No-op
                    *changed = true;
                    Stmt::Noop(pos)
                }
                [Stmt::Let(_, Some(_), _)] => {
                    // Only one let statement, but cannot promote
                    // (otherwise the variable gets declared in the scope above)
                    // and still need to run just in case there are side effects
                    Stmt::Block(result, pos)
                }
                [_] => {
                    // Only one statement - promote
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
        stmt @ Stmt::ReturnWithVal(None, _, _) => stmt,

        stmt @ Stmt::Noop(_) | stmt @ Stmt::Break(_) => stmt,
    }
}

fn optimize_expr(expr: Expr, changed: &mut bool) -> Expr {
    match expr {
        Expr::IntegerConstant(_, _)
        | Expr::FloatConstant(_, _)
        | Expr::Identifier(_, _)
        | Expr::CharConstant(_, _)
        | Expr::StringConstant(_, _)
        | Expr::True(_)
        | Expr::False(_)
        | Expr::Unit(_) => expr,

        Expr::Stmt(stmt, pos) => match optimize_stmt(*stmt, changed) {
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
        Expr::Index(lhs, rhs, pos) => Expr::Index(
            Box::new(optimize_expr(*lhs, changed)),
            Box::new(optimize_expr(*rhs, changed)),
            pos,
        ),
        Expr::Array(items, pos) => {
            let original_len = items.len();

            let items: Vec<_> = items
                .into_iter()
                .map(|expr| optimize_expr(expr, changed))
                .collect();

            *changed = *changed || original_len != items.len();

            Expr::Array(items, pos)
        }

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

        Expr::FunctionCall(id, args, def_value, pos) => {
            let original_len = args.len();

            let args: Vec<_> = args
                .into_iter()
                .map(|a| optimize_expr(a, changed))
                .collect();

            *changed = *changed || original_len != args.len();

            Expr::FunctionCall(id, args, def_value, pos)
        }
    }
}

pub(crate) fn optimize(mut statements: Vec<Stmt>) -> Vec<Stmt> {
    loop {
        let mut changed = false;

        statements = statements
            .into_iter()
            .map(|stmt| optimize_stmt(stmt, &mut changed))
            .filter(Stmt::is_op)
            .collect();

        if !changed {
            break;
        }
    }

    statements
}
