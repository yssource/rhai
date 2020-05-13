use crate::any::Dynamic;
use crate::calc_fn_hash;
use crate::engine::{
    Engine, FunctionsLib, KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_PRINT, KEYWORD_TYPE_OF,
};
use crate::fn_native::FnCallArgs;
use crate::packages::{PackageStore, PackagesCollection};
use crate::parser::{map_dynamic_to_expr, Expr, FnDef, ReturnType, Stmt, AST};
use crate::result::EvalAltResult;
use crate::scope::{Entry as ScopeEntry, EntryType as ScopeEntryType, Scope};
use crate::token::Position;

use crate::stdlib::{
    boxed::Box,
    collections::HashMap,
    iter::empty,
    string::{String, ToString},
    vec,
    vec::Vec,
};

/// Level of optimization performed.
///
/// Not available under the `no_optimize` feature.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum OptimizationLevel {
    /// No optimization performed.
    None,
    /// Only perform simple optimizations without evaluating functions.
    Simple,
    /// Full optimizations performed, including evaluating functions.
    /// Take care that this may cause side effects as it essentially assumes that all functions are pure.
    Full,
}

impl OptimizationLevel {
    /// Is the `OptimizationLevel` None.
    pub fn is_none(self) -> bool {
        self == Self::None
    }
    /// Is the `OptimizationLevel` Full.
    pub fn is_full(self) -> bool {
        self == Self::Full
    }
}

/// Mutable state throughout an optimization pass.
struct State<'a> {
    /// Has the AST been changed during this pass?
    changed: bool,
    /// Collection of constants to use for eager function evaluations.
    constants: Vec<(String, Expr)>,
    /// An `Engine` instance for eager function evaluation.
    engine: &'a Engine,
    /// Library of script-defined functions.
    fn_lib: &'a [(&'a str, usize)],
    /// Optimization level.
    optimization_level: OptimizationLevel,
}

impl<'a> State<'a> {
    /// Create a new State.
    pub fn new(
        engine: &'a Engine,
        fn_lib: &'a [(&'a str, usize)],
        level: OptimizationLevel,
    ) -> Self {
        Self {
            changed: false,
            constants: vec![],
            engine,
            fn_lib,
            optimization_level: level,
        }
    }
    /// Reset the state from dirty to clean.
    pub fn reset(&mut self) {
        self.changed = false;
    }
    /// Set the AST state to be dirty (i.e. changed).
    pub fn set_dirty(&mut self) {
        self.changed = true;
    }
    /// Is the AST dirty (i.e. changed)?
    pub fn is_dirty(&self) -> bool {
        self.changed
    }
    /// Does a constant exist?
    pub fn contains_constant(&self, name: &str) -> bool {
        self.constants.iter().any(|(n, _)| n == name)
    }
    /// Prune the list of constants back to a specified size.
    pub fn restore_constants(&mut self, len: usize) {
        self.constants.truncate(len)
    }
    /// Add a new constant to the list.
    pub fn push_constant(&mut self, name: &str, value: Expr) {
        self.constants.push((name.into(), value))
    }
    /// Look up a constant from the list.
    pub fn find_constant(&self, name: &str) -> Option<&Expr> {
        for (n, expr) in self.constants.iter().rev() {
            if n == name {
                return Some(expr);
            }
        }

        None
    }
}

/// Call a registered function
fn call_fn(
    packages: &PackagesCollection,
    base_package: &PackageStore,
    fn_name: &str,
    args: &mut FnCallArgs,
    pos: Position,
) -> Result<Option<Dynamic>, Box<EvalAltResult>> {
    // Search built-in's and external functions
    let hash = calc_fn_hash(empty(), fn_name, args.iter().map(|a| a.type_id()));

    base_package
        .get_function(hash)
        .or_else(|| packages.get_function(hash))
        .map(|func| func.call(args, pos))
        .transpose()
}

/// Optimize a statement.
fn optimize_stmt<'a>(stmt: Stmt, state: &mut State<'a>, preserve_result: bool) -> Stmt {
    match stmt {
        // if expr { Noop }
        Stmt::IfThenElse(x) if matches!(x.1, Stmt::Noop(_)) => {
            state.set_dirty();

            let pos = x.0.position();
            let expr = optimize_expr(x.0, state);

            if preserve_result {
                // -> { expr, Noop }
                Stmt::Block(Box::new((vec![Stmt::Expr(Box::new(expr)), x.1], pos)))
            } else {
                // -> expr
                Stmt::Expr(Box::new(expr))
            }
        }
        // if expr { if_block }
        Stmt::IfThenElse(x) if x.2.is_none() => match x.0 {
            // if false { if_block } -> Noop
            Expr::False(pos) => {
                state.set_dirty();
                Stmt::Noop(pos)
            }
            // if true { if_block } -> if_block
            Expr::True(_) => optimize_stmt(x.1, state, true),
            // if expr { if_block }
            expr => Stmt::IfThenElse(Box::new((
                optimize_expr(expr, state),
                optimize_stmt(x.1, state, true),
                None,
            ))),
        },
        // if expr { if_block } else { else_block }
        Stmt::IfThenElse(x) if x.2.is_some() => match x.0 {
            // if false { if_block } else { else_block } -> else_block
            Expr::False(_) => optimize_stmt(x.2.unwrap(), state, true),
            // if true { if_block } else { else_block } -> if_block
            Expr::True(_) => optimize_stmt(x.1, state, true),
            // if expr { if_block } else { else_block }
            expr => Stmt::IfThenElse(Box::new((
                optimize_expr(expr, state),
                optimize_stmt(x.1, state, true),
                match optimize_stmt(x.2.unwrap(), state, true) {
                    Stmt::Noop(_) => None, // Noop -> no else block
                    stmt => Some(stmt),
                },
            ))),
        },
        // while expr { block }
        Stmt::While(x) => match x.0 {
            // while false { block } -> Noop
            Expr::False(pos) => {
                state.set_dirty();
                Stmt::Noop(pos)
            }
            // while true { block } -> loop { block }
            Expr::True(_) => Stmt::Loop(Box::new(optimize_stmt(x.1, state, false))),
            // while expr { block }
            expr => match optimize_stmt(x.1, state, false) {
                // while expr { break; } -> { expr; }
                Stmt::Break(pos) => {
                    // Only a single break statement - turn into running the guard expression once
                    state.set_dirty();
                    let mut statements = vec![Stmt::Expr(Box::new(optimize_expr(expr, state)))];
                    if preserve_result {
                        statements.push(Stmt::Noop(pos))
                    }
                    Stmt::Block(Box::new((statements, pos)))
                }
                // while expr { block }
                stmt => Stmt::While(Box::new((optimize_expr(expr, state), stmt))),
            },
        },
        // loop { block }
        Stmt::Loop(block) => match optimize_stmt(*block, state, false) {
            // loop { break; } -> Noop
            Stmt::Break(pos) => {
                // Only a single break statement
                state.set_dirty();
                Stmt::Noop(pos)
            }
            // loop { block }
            stmt => Stmt::Loop(Box::new(stmt)),
        },
        // for id in expr { block }
        Stmt::For(x) => Stmt::For(Box::new((
            x.0,
            optimize_expr(x.1, state),
            optimize_stmt(x.2, state, false),
        ))),
        // let id = expr;
        Stmt::Let(x) if x.1.is_some() => {
            Stmt::Let(Box::new((x.0, Some(optimize_expr(x.1.unwrap(), state)))))
        }
        // let id;
        stmt @ Stmt::Let(_) => stmt,
        // import expr as id;
        Stmt::Import(x) => Stmt::Import(Box::new((optimize_expr(x.0, state), x.1))),
        // { block }
        Stmt::Block(x) => {
            let orig_len = x.0.len(); // Original number of statements in the block, for change detection
            let orig_constants_len = state.constants.len(); // Original number of constants in the state, for restore later
            let pos = x.1;

            // Optimize each statement in the block
            let mut result: Vec<_> =
                x.0.into_iter()
                    .map(|stmt| match stmt {
                        // Add constant into the state
                        Stmt::Const(v) => {
                            let ((name, pos), expr) = *v;
                            state.push_constant(&name, expr);
                            state.set_dirty();
                            Stmt::Noop(pos) // No need to keep constants
                        }
                        // Optimize the statement
                        _ => optimize_stmt(stmt, state, preserve_result),
                    })
                    .collect();

            // Remove all raw expression statements that are pure except for the very last statement
            let last_stmt = if preserve_result { result.pop() } else { None };

            result.retain(|stmt| !stmt.is_pure());

            if let Some(stmt) = last_stmt {
                result.push(stmt);
            }

            // Remove all let/import statements at the end of a block - the new variables will go away anyway.
            // But be careful only remove ones that have no initial values or have values that are pure expressions,
            // otherwise there may be side effects.
            let mut removed = false;

            while let Some(expr) = result.pop() {
                match expr {
                    Stmt::Let(x) if x.1.is_none() => removed = true,
                    Stmt::Let(x) if x.1.is_some() => removed = x.1.unwrap().is_pure(),
                    Stmt::Import(x) => removed = x.0.is_pure(),
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

                // Optimize all the statements again
                result = result
                    .into_iter()
                    .rev()
                    .enumerate()
                    .map(|(i, s)| optimize_stmt(s, state, i == 0))
                    .rev()
                    .collect();
            }

            // Remove everything following the the first return/throw
            let mut dead_code = false;

            result.retain(|stmt| {
                if dead_code {
                    return false;
                }

                match stmt {
                    Stmt::ReturnWithVal(_) | Stmt::Break(_) => {
                        dead_code = true;
                    }
                    _ => (),
                }

                true
            });

            // Change detection
            if orig_len != result.len() {
                state.set_dirty();
            }

            // Pop the stack and remove all the local constants
            state.restore_constants(orig_constants_len);

            match result[..] {
                // No statements in block - change to No-op
                [] => {
                    state.set_dirty();
                    Stmt::Noop(pos)
                }
                // Only one let/import statement - leave it alone
                [Stmt::Let(_)] | [Stmt::Import(_)] => Stmt::Block(Box::new((result, pos))),
                // Only one statement - promote
                [_] => {
                    state.set_dirty();
                    result.remove(0)
                }
                _ => Stmt::Block(Box::new((result, pos))),
            }
        }
        // expr;
        Stmt::Expr(expr) => Stmt::Expr(Box::new(optimize_expr(*expr, state))),
        // return expr;
        Stmt::ReturnWithVal(x) if x.1.is_some() => {
            Stmt::ReturnWithVal(Box::new((x.0, Some(optimize_expr(x.1.unwrap(), state)))))
        }
        // All other statements - skip
        stmt => stmt,
    }
}

/// Optimize an expression.
fn optimize_expr<'a>(expr: Expr, state: &mut State<'a>) -> Expr {
    // These keywords are handled specially
    const DONT_EVAL_KEYWORDS: [&str; 3] = [KEYWORD_PRINT, KEYWORD_DEBUG, KEYWORD_EVAL];

    match expr {
        // ( stmt )
        Expr::Stmt(x) => match optimize_stmt(x.0, state, true) {
            // ( Noop ) -> ()
            Stmt::Noop(_) => {
                state.set_dirty();
                Expr::Unit(x.1)
            }
            // ( expr ) -> expr
            Stmt::Expr(expr) => {
                state.set_dirty();
                *expr
            }
            // ( stmt )
            stmt => Expr::Stmt(Box::new((stmt, x.1))),
        },
        // id = expr
        Expr::Assignment(x) => match x.1 {
            //id = id2 = expr2
            Expr::Assignment(x2) => match (x.0, x2.0) {
                // var = var = expr2 -> var = expr2
                (Expr::Variable(a), Expr::Variable(b))
                    if a.1.is_none() && b.1.is_none() && a.0 == b.0 && a.3 == b.3 =>
                {
                    // Assignment to the same variable - fold
                    state.set_dirty();
                    Expr::Assignment(Box::new((Expr::Variable(a), optimize_expr(x2.1, state), x.2)))
                }
                // id1 = id2 = expr2
                (id1, id2) => {
                    Expr::Assignment(Box::new((
                        id1, Expr::Assignment(Box::new((id2, optimize_expr(x2.1, state), x2.2))), x.2,
                    )))
                }
            },
            // id = expr
            expr => Expr::Assignment(Box::new((x.0, optimize_expr(expr, state), x.2))),
        },

        // lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x) => match (x.0, x.1) {
            // map.string
            (Expr::Map(m), Expr::Property(p)) if m.0.iter().all(|(_, x)| x.is_pure()) => {
                let ((prop, _, _), _) = p.as_ref();
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let pos = m.1;
                m.0.into_iter().find(|((name, _), _)| name == prop)
                    .map(|(_, expr)| expr.set_position(pos))
                    .unwrap_or_else(|| Expr::Unit(pos))
            }
            // lhs.rhs
            (lhs, rhs) => Expr::Dot(Box::new((optimize_expr(lhs, state), optimize_expr(rhs, state), x.2)))
        }

        // lhs[rhs]
        #[cfg(not(feature = "no_index"))]
        Expr::Index(x) => match (x.0, x.1) {
            // array[int]
            (Expr::Array(mut a), Expr::IntegerConstant(i))
                if i.0 >= 0 && (i.0 as usize) < a.0.len() && a.0.iter().all(Expr::is_pure) =>
            {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                a.0.remove(i.0 as usize).set_position(a.1)
            }
            // map[string]
            (Expr::Map(m), Expr::StringConstant(s)) if m.0.iter().all(|(_, x)| x.is_pure()) => {
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let pos = m.1;
                m.0.into_iter().find(|((name, _), _)| name == &s.0)
                    .map(|(_, expr)| expr.set_position(pos))
                    .unwrap_or_else(|| Expr::Unit(pos))
            }
            // string[int]
            (Expr::StringConstant(s), Expr::IntegerConstant(i)) if i.0 >= 0 && (i.0 as usize) < s.0.chars().count() => {
                // String literal indexing - get the character
                state.set_dirty();
                Expr::CharConstant(Box::new((s.0.chars().nth(i.0 as usize).expect("should get char"), s.1)))
            }
            // lhs[rhs]
            (lhs, rhs) => Expr::Index(Box::new((optimize_expr(lhs, state), optimize_expr(rhs, state), x.2))),
        },
        // [ items .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(a) => Expr::Array(Box::new((a.0
                            .into_iter()
                            .map(|expr| optimize_expr(expr, state))
                            .collect(), a.1))),
        // [ items .. ]
        #[cfg(not(feature = "no_object"))]
        Expr::Map(m) => Expr::Map(Box::new((m.0
                            .into_iter()
                            .map(|((key, pos), expr)| ((key, pos), optimize_expr(expr, state)))
                            .collect(), m.1))),
        // lhs in rhs
        Expr::In(x) => match (x.0, x.1) {
            // "xxx" in "xxxxx"
            (Expr::StringConstant(a), Expr::StringConstant(b)) => {
                state.set_dirty();
                if b.0.contains(&a.0) { Expr::True(a.1) } else { Expr::False(a.1) }
            }
            // 'x' in "xxxxx"
            (Expr::CharConstant(a), Expr::StringConstant(b)) => {
                state.set_dirty();
                if b.0.contains(a.0) { Expr::True(a.1) } else { Expr::False(a.1) }
            }
            // "xxx" in #{...}
            (Expr::StringConstant(a), Expr::Map(b)) => {
                state.set_dirty();
                if b.0.iter().find(|((name, _), _)| name == &a.0).is_some() {
                    Expr::True(a.1)
                } else {
                    Expr::False(a.1)
                }
            }
            // 'x' in #{...}
            (Expr::CharConstant(a), Expr::Map(b)) => {
                state.set_dirty();
                let ch = a.0.to_string();

                if b.0.iter().find(|((name, _), _)| name == &ch).is_some() {
                    Expr::True(a.1)
                } else {
                    Expr::False(a.1)
                }
            }
            // lhs in rhs
            (lhs, rhs) => Expr::In(Box::new((optimize_expr(lhs, state), optimize_expr(rhs, state), x.2))),
        },
        // lhs && rhs
        Expr::And(x) => match (x.0, x.1) {
            // true && rhs -> rhs
            (Expr::True(_), rhs) => {
                state.set_dirty();
                rhs
            }
            // false && rhs -> false
            (Expr::False(pos), _) => {
                state.set_dirty();
                Expr::False(pos)
            }
            // lhs && true -> lhs
            (lhs, Expr::True(_)) => {
                state.set_dirty();
                optimize_expr(lhs, state)
            }
            // lhs && rhs
            (lhs, rhs) => Expr::And(Box::new((optimize_expr(lhs, state), optimize_expr(rhs, state), x.2))),
        },
        // lhs || rhs
        Expr::Or(x) => match (x.0, x.1) {
            // false || rhs -> rhs
            (Expr::False(_), rhs) => {
                state.set_dirty();
                rhs
            }
            // true || rhs -> true
            (Expr::True(pos), _) => {
                state.set_dirty();
                Expr::True(pos)
            }
            // lhs || false
            (lhs, Expr::False(_)) => {
                state.set_dirty();
                optimize_expr(lhs, state)
            }
            // lhs || rhs
            (lhs, rhs) => Expr::Or(Box::new((optimize_expr(lhs, state), optimize_expr(rhs, state), x.2))),
        },

        // Do not call some special keywords
        Expr::FnCall(mut x) if DONT_EVAL_KEYWORDS.contains(&(x.0).0.as_ref())=> {
            x.3 = x.3.into_iter().map(|a| optimize_expr(a, state)).collect();
            Expr::FnCall(x)
        }

        // Eagerly call functions
        Expr::FnCall(mut x)
                if x.1.is_none() // Non-qualified
                && state.optimization_level == OptimizationLevel::Full // full optimizations
                && x.3.iter().all(|expr| expr.is_constant()) // all arguments are constants
        => {
            let ((name, pos), _, _, args, def_value) = x.as_mut();

            // First search in script-defined functions (can override built-in)
            if state.fn_lib.iter().find(|(id, len)| *id == name && *len == args.len()).is_some() {
                // A script-defined function overrides the built-in function - do not make the call
                x.3 = x.3.into_iter().map(|a| optimize_expr(a, state)).collect();
                return Expr::FnCall(x);
            }

            let mut arg_values: Vec<_> = args.iter().map(Expr::get_constant_value).collect();
            let mut call_args: Vec<_> = arg_values.iter_mut().collect();

            // Save the typename of the first argument if it is `type_of()`
            // This is to avoid `call_args` being passed into the closure
            let arg_for_type_of = if name == KEYWORD_TYPE_OF && call_args.len() == 1 {
                state.engine.map_type_name(call_args[0].type_name())
            } else {
                ""
            };

            call_fn(&state.engine.packages, &state.engine.base_package, name, &mut call_args, *pos).ok()
                .and_then(|result|
                    result.or_else(|| {
                        if !arg_for_type_of.is_empty() {
                            // Handle `type_of()`
                            Some(arg_for_type_of.to_string().into())
                        } else {
                            // Otherwise use the default value, if any
                            def_value.clone()
                        }
                    }).and_then(|result| map_dynamic_to_expr(result, *pos))
                    .map(|expr| {
                        state.set_dirty();
                        expr
                    })
                ).unwrap_or_else(|| {
                    // Optimize function call arguments
                    x.3 = x.3.into_iter().map(|a| optimize_expr(a, state)).collect();
                    Expr::FnCall(x)
                })
        }

        // id(args ..) -> optimize function call arguments
        Expr::FnCall(mut x) => {
            x.3 = x.3.into_iter().map(|a| optimize_expr(a, state)).collect();
            Expr::FnCall(x)
        }

        // constant-name
        Expr::Variable(x) if x.1.is_none() && state.contains_constant(&(x.0).0) => {
            let (name, pos) = x.0;
            state.set_dirty();

            // Replace constant with value
            state.find_constant(&name).expect("should find constant in scope!").clone().set_position(pos)
        }

        // All other expressions - skip
        expr => expr,
    }
}

fn optimize<'a>(
    statements: Vec<Stmt>,
    engine: &Engine,
    scope: &Scope,
    fn_lib: &'a [(&'a str, usize)],
    level: OptimizationLevel,
) -> Vec<Stmt> {
    // If optimization level is None then skip optimizing
    if level == OptimizationLevel::None {
        return statements;
    }

    // Set up the state
    let mut state = State::new(engine, fn_lib, level);

    // Add constants from the scope into the state
    scope
        .iter()
        .filter(|ScopeEntry { typ, expr, .. }| {
            // Get all the constants with definite constant expressions
            *typ == ScopeEntryType::Constant
                && expr.as_ref().map(|v| v.is_constant()).unwrap_or(false)
        })
        .for_each(|ScopeEntry { name, expr, .. }| {
            state.push_constant(
                name.as_ref(),
                (**expr.as_ref().expect("should be Some(expr)")).clone(),
            )
        });

    let orig_constants_len = state.constants.len();

    let mut result = statements;

    // Optimization loop
    loop {
        state.reset();
        state.restore_constants(orig_constants_len);

        let num_statements = result.len();

        result = result
            .into_iter()
            .enumerate()
            .map(|(i, stmt)| {
                match &stmt {
                    Stmt::Const(v) => {
                        // Load constants
                        let ((name, _), expr) = v.as_ref();
                        state.push_constant(&name, expr.clone());
                        stmt // Keep it in the global scope
                    }
                    _ => {
                        // Keep all variable declarations at this level
                        // and always keep the last return value
                        let keep = match stmt {
                            Stmt::Let(_) | Stmt::Import(_) => true,
                            _ => i == num_statements - 1,
                        };
                        optimize_stmt(stmt, &mut state, keep)
                    }
                }
            })
            .collect();

        if !state.is_dirty() {
            break;
        }
    }

    // Eliminate code that is pure but always keep the last statement
    let last_stmt = result.pop();

    // Remove all pure statements at global level
    result.retain(|stmt| !stmt.is_pure());

    // Add back the last statement unless it is a lone No-op
    if let Some(stmt) = last_stmt {
        if !result.is_empty() || !matches!(stmt, Stmt::Noop(_)) {
            result.push(stmt);
        }
    }

    result
}

/// Optimize an AST.
pub fn optimize_into_ast(
    engine: &Engine,
    scope: &Scope,
    statements: Vec<Stmt>,
    functions: Vec<FnDef>,
    level: OptimizationLevel,
) -> AST {
    #[cfg(feature = "no_optimize")]
    const level: OptimizationLevel = OptimizationLevel::None;

    #[cfg(not(feature = "no_function"))]
    let fn_lib: Vec<_> = functions
        .iter()
        .map(|fn_def| (fn_def.name.as_str(), fn_def.params.len()))
        .collect();

    #[cfg(feature = "no_function")]
    const fn_lib: &[(&str, usize)] = &[];

    #[cfg(not(feature = "no_function"))]
    let lib = FunctionsLib::from_iter(functions.iter().cloned().map(|mut fn_def| {
        if !level.is_none() {
            let pos = fn_def.body.position();

            // Optimize the function body
            let mut body = optimize(vec![fn_def.body], engine, &Scope::new(), &fn_lib, level);

            // {} -> Noop
            fn_def.body = match body.pop().unwrap_or_else(|| Stmt::Noop(pos)) {
                // { return val; } -> val
                Stmt::ReturnWithVal(x) if x.1.is_some() && (x.0).0 == ReturnType::Return => {
                    Stmt::Expr(Box::new(x.1.unwrap()))
                }
                // { return; } -> ()
                Stmt::ReturnWithVal(x) if x.1.is_none() && (x.0).0 == ReturnType::Return => {
                    Stmt::Expr(Box::new(Expr::Unit((x.0).1)))
                }
                // All others
                stmt => stmt,
            };
        }
        fn_def
    }));

    #[cfg(feature = "no_function")]
    let lib: FunctionsLib = Default::default();

    AST::new(
        match level {
            OptimizationLevel::None => statements,
            OptimizationLevel::Simple | OptimizationLevel::Full => {
                optimize(statements, engine, &scope, &fn_lib, level)
            }
        },
        lib,
    )
}
