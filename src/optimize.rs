#![cfg(not(feature = "no_optimize"))]

use crate::any::Dynamic;
use crate::engine::{Engine, FnCallArgs, KEYWORD_DEBUG, KEYWORD_DUMP_AST, KEYWORD_PRINT};
use crate::parser::{map_dynamic_to_expr, Expr, FnDef, Stmt, AST};
use crate::scope::{Scope, ScopeEntry, VariableType};

use crate::stdlib::{
    sync::Arc,
    vec::Vec, string::{String, ToString},
    boxed::Box, vec,
};

/// Level of optimization performed
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum OptimizationLevel {
    /// No optimization performed
    None,
    /// Only perform simple optimizations without evaluating functions
    Simple,
    /// Full optimizations performed, including evaluating functions.
    /// Take care that this may cause side effects.
    Full,
}

struct State<'a> {
    changed: bool,
    constants: Vec<(String, Expr)>,
    engine: Option<&'a Engine<'a>>,
}

impl State<'_> {
    pub fn new() -> Self {
        State {
            changed: false,
            constants: vec![],
            engine: None,
        }
    }
    pub fn reset(&mut self) {
        self.changed = false;
    }
    pub fn set_dirty(&mut self) {
        self.changed = true;
    }
    pub fn is_dirty(&self) -> bool {
        self.changed
    }
    pub fn contains_constant(&self, name: &str) -> bool {
        self.constants.iter().any(|(n, _)| n == name)
    }
    pub fn restore_constants(&mut self, len: usize) {
        self.constants.truncate(len)
    }
    pub fn push_constant(&mut self, name: &str, value: Expr) {
        self.constants.push((name.to_string(), value))
    }
    pub fn find_constant(&self, name: &str) -> Option<&Expr> {
        for (n, expr) in self.constants.iter().rev() {
            if n == name {
                return Some(expr);
            }
        }

        None
    }
}

fn optimize_stmt<'a>(stmt: Stmt, state: &mut State<'a>, preserve_result: bool) -> Stmt {
    match stmt {
        Stmt::IfElse(expr, stmt1, None) if stmt1.is_noop() => {
            state.set_dirty();

            let pos = expr.position();
            let expr = optimize_expr(*expr, state);

            if matches!(expr, Expr::False(_) | Expr::True(_)) {
                Stmt::Noop(stmt1.position())
            } else {
                let stmt = Stmt::Expr(Box::new(expr));

                if preserve_result {
                    Stmt::Block(vec![stmt, *stmt1], pos)
                } else {
                    stmt
                }
            }
        }

        Stmt::IfElse(expr, stmt1, None) => match *expr {
            Expr::False(pos) => {
                state.set_dirty();
                Stmt::Noop(pos)
            }
            Expr::True(_) => optimize_stmt(*stmt1, state, true),
            expr => Stmt::IfElse(
                Box::new(optimize_expr(expr, state)),
                Box::new(optimize_stmt(*stmt1, state, true)),
                None,
            ),
        },

        Stmt::IfElse(expr, stmt1, Some(stmt2)) => match *expr {
            Expr::False(_) => optimize_stmt(*stmt2, state, true),
            Expr::True(_) => optimize_stmt(*stmt1, state, true),
            expr => Stmt::IfElse(
                Box::new(optimize_expr(expr, state)),
                Box::new(optimize_stmt(*stmt1, state, true)),
                match optimize_stmt(*stmt2, state, true) {
                    stmt if stmt.is_noop() => None,
                    stmt => Some(Box::new(stmt)),
                },
            ),
        },

        Stmt::While(expr, stmt) => match *expr {
            Expr::False(pos) => {
                state.set_dirty();
                Stmt::Noop(pos)
            }
            Expr::True(_) => Stmt::Loop(Box::new(optimize_stmt(*stmt, state, false))),
            expr => Stmt::While(
                Box::new(optimize_expr(expr, state)),
                Box::new(optimize_stmt(*stmt, state, false)),
            ),
        },

        Stmt::Loop(stmt) => Stmt::Loop(Box::new(optimize_stmt(*stmt, state, false))),
        Stmt::For(id, expr, stmt) => Stmt::For(
            id,
            Box::new(optimize_expr(*expr, state)),
            Box::new(optimize_stmt(*stmt, state, false)),
        ),
        Stmt::Let(id, Some(expr), pos) => {
            Stmt::Let(id, Some(Box::new(optimize_expr(*expr, state))), pos)
        }
        Stmt::Let(_, None, _) => stmt,

        Stmt::Block(statements, pos) => {
            let orig_len = statements.len();
            let orig_constants_len = state.constants.len();

            let mut result: Vec<_> = statements
                .into_iter() // For each statement
                .map(|stmt| {
                    if let Stmt::Const(name, value, pos) = stmt {
                        state.push_constant(&name, *value);
                        state.set_dirty();
                        Stmt::Noop(pos) // No need to keep constants
                    } else {
                        optimize_stmt(stmt, state, preserve_result) // Optimize the statement
                    }
                })
                .enumerate()
                .filter(|(i, stmt)| stmt.is_op() || (preserve_result && *i == orig_len - 1)) // Remove no-op's but leave the last one if we need the result
                .map(|(_, stmt)| stmt)
                .collect();

            // Remove all raw expression statements that are pure except for the very last statement
            let last_stmt = if preserve_result { result.pop() } else { None };

            result.retain(|stmt| !matches!(stmt, Stmt::Expr(expr) if expr.is_pure()));

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
                    .map(|(i, s)| optimize_stmt(s, state, i == 0)) // Optimize all other statements again
                    .rev()
                    .collect();
            }

            if orig_len != result.len() {
                state.set_dirty();
            }

            state.restore_constants(orig_constants_len);

            match result[..] {
                // No statements in block - change to No-op
                [] => {
                    state.set_dirty();
                    Stmt::Noop(pos)
                }
                // Only one statement - promote
                [_] => {
                    state.set_dirty();
                    result.remove(0)
                }
                _ => Stmt::Block(result, pos),
            }
        }

        Stmt::Expr(expr) => Stmt::Expr(Box::new(optimize_expr(*expr, state))),

        Stmt::ReturnWithVal(Some(expr), is_return, pos) => {
            Stmt::ReturnWithVal(Some(Box::new(optimize_expr(*expr, state))), is_return, pos)
        }

        stmt => stmt,
    }
}

fn optimize_expr<'a>(expr: Expr, state: &mut State<'a>) -> Expr {
    match expr {
        Expr::Stmt(stmt, pos) => match optimize_stmt(*stmt, state, true) {
            Stmt::Noop(_) => {
                state.set_dirty();
                Expr::Unit(pos)
            }
            Stmt::Expr(expr) => {
                state.set_dirty();
                *expr
            }
            stmt => Expr::Stmt(Box::new(stmt), pos),
        },
        Expr::Assignment(id1, expr1, pos1) => match *expr1 {
            Expr::Assignment(id2, expr2, pos2) => match (*id1, *id2) {
                (Expr::Variable(var1, _), Expr::Variable(var2, _)) if var1 == var2 => {
                    // Assignment to the same variable - fold
                    state.set_dirty();

                    Expr::Assignment(
                        Box::new(Expr::Variable(var1, pos1)),
                        Box::new(optimize_expr(*expr2, state)),
                        pos1,
                    )
                }
                (id1, id2) => Expr::Assignment(
                    Box::new(id1),
                    Box::new(Expr::Assignment(
                        Box::new(id2),
                        Box::new(optimize_expr(*expr2, state)),
                        pos2,
                    )),
                    pos1,
                ),
            },
            expr => Expr::Assignment(id1, Box::new(optimize_expr(expr, state)), pos1),
        },
        Expr::Dot(lhs, rhs, pos) => Expr::Dot(
            Box::new(optimize_expr(*lhs, state)),
            Box::new(optimize_expr(*rhs, state)),
            pos,
        ),

        #[cfg(not(feature = "no_index"))]
        Expr::Index(lhs, rhs, pos) => match (*lhs, *rhs) {
            (Expr::Array(mut items, _), Expr::IntegerConstant(i, _))
                if i >= 0 && (i as usize) < items.len() && items.iter().all(|x| x.is_pure()) =>
            {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                items.remove(i as usize)
            }
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, _)) 
                if i >= 0 && (i as usize) < s.chars().count() =>
            {
                // String literal indexing - get the character
                state.set_dirty();
                Expr::CharConstant(s.chars().nth(i as usize).expect("should get char"), pos)
            }

            (lhs, rhs) => Expr::Index(
                Box::new(optimize_expr(lhs, state)),
                Box::new(optimize_expr(rhs, state)),
                pos,
            ),
        },

        #[cfg(not(feature = "no_index"))]
        Expr::Array(items, pos) => {
            let orig_len = items.len();

            let items: Vec<_> = items
                .into_iter()
                .map(|expr| optimize_expr(expr, state))
                .collect();

            if orig_len != items.len() {
                state.set_dirty();
            }

            Expr::Array(items, pos)
        }

        Expr::And(lhs, rhs) => match (*lhs, *rhs) {
            (Expr::True(_), rhs) => {
                state.set_dirty();
                rhs
            }
            (Expr::False(pos), _) => {
                state.set_dirty();
                Expr::False(pos)
            }
            (lhs, Expr::True(_)) => {
                state.set_dirty();
                lhs
            }
            (lhs, rhs) => Expr::And(
                Box::new(optimize_expr(lhs, state)),
                Box::new(optimize_expr(rhs, state)),
            ),
        },
        Expr::Or(lhs, rhs) => match (*lhs, *rhs) {
            (Expr::False(_), rhs) => {
                state.set_dirty();
                rhs
            }
            (Expr::True(pos), _) => {
                state.set_dirty();
                Expr::True(pos)
            }
            (lhs, Expr::False(_)) => {
                state.set_dirty();
                lhs
            }
            (lhs, rhs) => Expr::Or(
                Box::new(optimize_expr(lhs, state)),
                Box::new(optimize_expr(rhs, state)),
            ),
        },

        // Do not optimize anything within `dump_ast`
        Expr::FunctionCall(id, args, def_value, pos) if id == KEYWORD_DUMP_AST => {
            Expr::FunctionCall(id, args, def_value, pos)
        }
        // Actually call function to optimize it
        Expr::FunctionCall(id, args, def_value, pos)
            if id != KEYWORD_DEBUG // not debug
                && id != KEYWORD_PRINT // not print
                && state.engine.map(|eng| eng.optimization_level == OptimizationLevel::Full).unwrap_or(false) // full optimizations
                && args.iter().all(|expr| expr.is_constant()) // all arguments are constants
            =>
        {
            let engine = state.engine.expect("engine should be Some");
            let mut arg_values: Vec<_> = args.iter().map(Expr::get_constant_value).collect();
            let call_args: FnCallArgs = arg_values.iter_mut().map(Dynamic::as_mut).collect();
            engine.call_ext_fn_raw(&id, call_args, pos).ok().map(|r| 
                r.or(def_value.clone()).and_then(|result| map_dynamic_to_expr(result, pos).0)
                    .map(|expr| {
                        state.set_dirty();
                        expr
                    })).flatten()
                    .unwrap_or_else(|| Expr::FunctionCall(id, args, def_value, pos))
        }
        // Optimize the function call arguments
        Expr::FunctionCall(id, args, def_value, pos) => {
            let orig_len = args.len();

            let args: Vec<_> = args.into_iter().map(|a| optimize_expr(a, state)).collect();

            if orig_len != args.len() {
                state.set_dirty();
            }

            Expr::FunctionCall(id, args, def_value, pos)
        }

        Expr::Variable(ref name, _) if state.contains_constant(name) => {
            state.set_dirty();

            // Replace constant with value
            state
                .find_constant(name)
                .expect("should find constant in scope!")
                .clone()
        }

        expr => expr,
    }
}

pub(crate) fn optimize<'a>(
    statements: Vec<Stmt>,
    engine: Option<&Engine<'a>>,
    scope: &Scope,
) -> Vec<Stmt> {
    // If optimization level is None then skip optimizing
    if engine
        .map(|eng| eng.optimization_level == OptimizationLevel::None)
        .unwrap_or(false)
    {
        return statements;
    }

    // Set up the state
    let mut state = State::new();
    state.engine = engine;

    scope
        .iter()
        .filter(|ScopeEntry { var_type, expr, .. }| {
            // Get all the constants with definite constant expressions
            *var_type == VariableType::Constant
                && expr.as_ref().map(Expr::is_constant).unwrap_or(false)
        })
        .for_each(|ScopeEntry { name, expr, .. }| {
            state.push_constant(
                name.as_ref(),
                expr.as_ref().expect("should be Some(expr)").clone(),
            )
        });

    let orig_constants_len = state.constants.len();

    // Optimization loop
    let mut result = statements;

    loop {
        state.reset();
        state.restore_constants(orig_constants_len);

        let num_statements = result.len();

        result = result
            .into_iter()
            .enumerate()
            .map(|(i, stmt)| {
                if let Stmt::Const(name, value, _) = &stmt {
                    // Load constants
                    state.push_constant(name, value.as_ref().clone());
                    stmt // Keep it in the top scope
                } else {
                    // Keep all variable declarations at this level
                    // and always keep the last return value
                    let keep = stmt.is_var() || i == num_statements - 1;

                    optimize_stmt(stmt, &mut state, keep)
                }
            })
            .collect();

        if !state.is_dirty() {
            break;
        }
    }

    // Eliminate code that is pure but always keep the last statement
    let last_stmt = result.pop();

    // Remove all pure statements at top level
    result.retain(|stmt| !matches!(stmt, Stmt::Expr(expr) if expr.is_pure()));

    if let Some(stmt) = last_stmt {
        result.push(stmt); // Add back the last statement
    }

    result
}

pub fn optimize_ast(
    engine: &Engine,
    scope: &Scope,
    statements: Vec<Stmt>,
    functions: Vec<FnDef>,
) -> AST {
    AST(
        match engine.optimization_level {
            OptimizationLevel::None => statements,
            OptimizationLevel::Simple => optimize(statements, None, &scope),
            OptimizationLevel::Full => optimize(statements, Some(engine), &scope),
        },
        functions
            .into_iter()
            .map(|mut fn_def| {
                match engine.optimization_level {
                    OptimizationLevel::None => (),
                    OptimizationLevel::Simple | OptimizationLevel::Full => {
                        let pos = fn_def.body.position();
                        let mut body = optimize(vec![fn_def.body], None, &Scope::new());
                        fn_def.body = body.pop().unwrap_or_else(|| Stmt::Noop(pos));
                    }
                }
                Arc::new(fn_def)
            })
            .collect(),
    )
}
