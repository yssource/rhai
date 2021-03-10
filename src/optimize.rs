//! Module implementing the [`AST`] optimizer.

use crate::ast::{Expr, Ident, Stmt};
use crate::dynamic::AccessMode;
use crate::engine::{KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_PRINT, KEYWORD_TYPE_OF};
use crate::fn_builtin::get_builtin_binary_op_fn;
use crate::parser::map_dynamic_to_expr;
use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    hash::{Hash, Hasher},
    iter::empty,
    mem,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use crate::utils::get_hasher;
use crate::{
    calc_fn_hash, calc_fn_params_hash, combine_hashes, Dynamic, Engine, Module, Position, Scope,
    StaticVec, AST,
};

/// Level of optimization performed.
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
    /// Is the `OptimizationLevel` [`None`][OptimizationLevel::None]?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is_none(self) -> bool {
        self == Self::None
    }
    /// Is the `OptimizationLevel` [`Simple`][OptimizationLevel::Simple]?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is_simple(self) -> bool {
        self == Self::Simple
    }
    /// Is the `OptimizationLevel` [`Full`][OptimizationLevel::Full]?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is_full(self) -> bool {
        self == Self::Full
    }
}

/// Mutable state throughout an optimization pass.
#[derive(Debug, Clone)]
struct State<'a> {
    /// Has the [`AST`] been changed during this pass?
    changed: bool,
    /// Collection of constants to use for eager function evaluations.
    variables: Vec<(String, AccessMode, Expr)>,
    /// Activate constants propagation?
    propagate_constants: bool,
    /// An [`Engine`] instance for eager function evaluation.
    engine: &'a Engine,
    /// [Module] containing script-defined functions.
    lib: &'a [&'a Module],
    /// Optimization level.
    optimization_level: OptimizationLevel,
}

impl<'a> State<'a> {
    /// Create a new State.
    #[inline(always)]
    pub fn new(engine: &'a Engine, lib: &'a [&'a Module], level: OptimizationLevel) -> Self {
        Self {
            changed: false,
            variables: vec![],
            propagate_constants: true,
            engine,
            lib,
            optimization_level: level,
        }
    }
    /// Reset the state from dirty to clean.
    #[inline(always)]
    pub fn reset(&mut self) {
        self.changed = false;
    }
    /// Set the [`AST`] state to be dirty (i.e. changed).
    #[inline(always)]
    pub fn set_dirty(&mut self) {
        self.changed = true;
    }
    /// Is the [`AST`] dirty (i.e. changed)?
    #[inline(always)]
    pub fn is_dirty(&self) -> bool {
        self.changed
    }
    /// Prune the list of constants back to a specified size.
    #[inline(always)]
    pub fn restore_var(&mut self, len: usize) {
        self.variables.truncate(len)
    }
    /// Add a new constant to the list.
    #[inline(always)]
    pub fn push_var(&mut self, name: &str, access: AccessMode, value: Expr) {
        self.variables.push((name.into(), access, value))
    }
    /// Look up a constant from the list.
    #[inline]
    pub fn find_constant(&self, name: &str) -> Option<&Expr> {
        if !self.propagate_constants {
            return None;
        }

        self.variables.iter().rev().find_map(|(n, access, expr)| {
            if n == name {
                match access {
                    AccessMode::ReadWrite => None,
                    AccessMode::ReadOnly => Some(expr),
                }
            } else {
                None
            }
        })
    }
}

// Has a system function a Rust-native override?
fn has_native_fn(state: &State, hash_script: u64, arg_types: &[TypeId]) -> bool {
    let hash_params = calc_fn_params_hash(arg_types.iter().cloned());
    let hash = combine_hashes(hash_script, hash_params);

    // First check registered functions
    state.engine.global_namespace.contains_fn(hash, false)
            // Then check packages
            || state.engine.global_modules.iter().any(|m| m.contains_fn(hash, false))
            // Then check sub-modules
            || state.engine.global_sub_modules.values().any(|m| m.contains_qualified_fn(hash))
}

/// Call a registered function
fn call_fn_with_constant_arguments(
    state: &State,
    fn_name: &str,
    arg_values: &mut [Dynamic],
) -> Option<Dynamic> {
    state
        .engine
        .call_native_fn(
            &mut Default::default(),
            &mut Default::default(),
            state.lib,
            fn_name,
            calc_fn_hash(empty(), fn_name, arg_values.len()),
            arg_values.iter_mut().collect::<StaticVec<_>>().as_mut(),
            false,
            false,
            Position::NONE,
        )
        .ok()
        .map(|(v, _)| v)
}

/// Optimize a block of [statements][Stmt].
fn optimize_stmt_block(
    mut statements: Vec<Stmt>,
    pos: Position,
    state: &mut State,
    preserve_result: bool,
) -> Vec<Stmt> {
    if statements.is_empty() {
        return statements;
    }

    let orig_len = statements.len(); // Original number of statements in the block, for change detection
    let orig_constants_len = state.variables.len(); // Original number of constants in the state, for restore later
    let orig_propagate_constants = state.propagate_constants;

    // Optimize each statement in the block
    statements.iter_mut().for_each(|stmt| {
        match stmt {
            // Add constant literals into the state
            Stmt::Const(value_expr, Ident { name, .. }, _, _) => {
                optimize_expr(value_expr, state);

                if value_expr.is_constant() {
                    state.push_var(name, AccessMode::ReadOnly, value_expr.clone());
                }
            }
            // Add variables into the state
            Stmt::Let(value_expr, Ident { name, pos, .. }, _, _) => {
                optimize_expr(value_expr, state);
                state.push_var(name, AccessMode::ReadWrite, Expr::Unit(*pos));
            }
            // Optimize the statement
            _ => optimize_stmt(stmt, state, preserve_result),
        }
    });

    // Remove all raw expression statements that are pure except for the very last statement
    let last_stmt = if preserve_result {
        statements.pop()
    } else {
        None
    };

    statements.retain(|stmt| !stmt.is_pure());

    if let Some(stmt) = last_stmt {
        statements.push(stmt);
    }

    // Remove all let/import statements at the end of a block - the new variables will go away anyway.
    // But be careful only remove ones that have no initial values or have values that are pure expressions,
    // otherwise there may be side effects.
    let mut removed = false;

    while let Some(expr) = statements.pop() {
        match expr {
            Stmt::Let(expr, _, _, _) | Stmt::Const(expr, _, _, _) => removed = expr.is_pure(),
            #[cfg(not(feature = "no_module"))]
            Stmt::Import(expr, _, _) => removed = expr.is_pure(),
            _ => {
                statements.push(expr);
                break;
            }
        }
    }

    if preserve_result {
        if removed {
            statements.push(Stmt::Noop(pos))
        }

        // Optimize all the statements again
        let num_statements = statements.len();
        statements
            .iter_mut()
            .enumerate()
            .for_each(|(i, stmt)| optimize_stmt(stmt, state, i >= num_statements - 1));
    }

    // Remove everything following the the first return/throw
    let mut dead_code = false;

    statements.retain(|stmt| {
        if dead_code {
            return false;
        }

        match stmt {
            Stmt::Return(_, _, _) | Stmt::Break(_) => dead_code = true,
            _ => (),
        }

        true
    });

    // Change detection
    if orig_len != statements.len() {
        state.set_dirty();
    }

    // Pop the stack and remove all the local constants
    state.restore_var(orig_constants_len);

    state.propagate_constants = orig_propagate_constants;

    statements
}

/// Optimize a [statement][Stmt].
fn optimize_stmt(stmt: &mut Stmt, state: &mut State, preserve_result: bool) {
    match stmt {
        // expr op= expr
        Stmt::Assignment(x, _) => match x.0 {
            Expr::Variable(_) => optimize_expr(&mut x.1, state),
            _ => {
                optimize_expr(&mut x.0, state);
                optimize_expr(&mut x.1, state);
            }
        },

        // if expr {}
        Stmt::If(condition, x, _) if x.0.is_empty() && x.1.is_empty() => {
            state.set_dirty();

            let pos = condition.position();
            let mut expr = mem::take(condition);
            optimize_expr(&mut expr, state);

            *stmt = if preserve_result {
                // -> { expr, Noop }
                Stmt::Block(vec![Stmt::Expr(expr), Stmt::Noop(pos)], pos)
            } else {
                // -> expr
                Stmt::Expr(expr)
            };
        }
        // if false { if_block } -> Noop
        Stmt::If(Expr::BoolConstant(false, pos), x, _) if x.1.is_empty() => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos);
        }
        // if false { if_block } else { else_block } -> else_block
        Stmt::If(Expr::BoolConstant(false, _), x, _) => {
            state.set_dirty();
            *stmt = match optimize_stmt_block(
                mem::take(&mut x.1.statements).into_vec(),
                x.1.pos,
                state,
                preserve_result,
            ) {
                statements if statements.is_empty() => Stmt::Noop(x.1.pos),
                statements => Stmt::Block(statements, x.1.pos),
            }
        }
        // if true { if_block } else { else_block } -> if_block
        Stmt::If(Expr::BoolConstant(true, _), x, _) => {
            state.set_dirty();
            *stmt = match optimize_stmt_block(
                mem::take(&mut x.0.statements).into_vec(),
                x.0.pos,
                state,
                preserve_result,
            ) {
                statements if statements.is_empty() => Stmt::Noop(x.0.pos),
                statements => Stmt::Block(statements, x.0.pos),
            }
        }
        // if expr { if_block } else { else_block }
        Stmt::If(condition, x, _) => {
            optimize_expr(condition, state);
            x.0.statements = optimize_stmt_block(
                mem::take(&mut x.0.statements).into_vec(),
                x.0.pos,
                state,
                preserve_result,
            )
            .into();
            x.1.statements = optimize_stmt_block(
                mem::take(&mut x.1.statements).into_vec(),
                x.1.pos,
                state,
                preserve_result,
            )
            .into();
        }

        // switch const { ... }
        Stmt::Switch(expr, x, pos) if expr.is_constant() => {
            let value = expr.get_constant_value().unwrap();
            let hasher = &mut get_hasher();
            value.hash(hasher);
            let hash = hasher.finish();

            state.set_dirty();

            let table = &mut x.0;

            if let Some(stmt) = table.get_mut(&hash) {
                optimize_stmt(stmt, state, true);
                *expr = Expr::Stmt(Box::new(vec![mem::take(stmt)].into()), *pos);
            } else if let Some(def_stmt) = x.1.as_mut() {
                optimize_stmt(def_stmt, state, true);
                *expr = Expr::Stmt(Box::new(vec![mem::take(def_stmt)].into()), *pos);
            } else {
                *expr = Expr::Unit(*pos);
            }
        }
        // switch
        Stmt::Switch(expr, x, _) => {
            optimize_expr(expr, state);
            x.0.values_mut()
                .for_each(|stmt| optimize_stmt(stmt, state, preserve_result));
            if let Some(def_stmt) = x.1.as_mut() {
                optimize_stmt(def_stmt, state, preserve_result);

                match def_stmt {
                    Stmt::Noop(_) | Stmt::Expr(Expr::Unit(_)) => x.1 = None,
                    _ => (),
                }
            }
        }

        // while false { block } -> Noop
        Stmt::While(Expr::BoolConstant(false, pos), _, _) => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos)
        }
        // while expr { block }
        Stmt::While(condition, block, _) => {
            optimize_expr(condition, state);

            block.statements = optimize_stmt_block(
                mem::take(&mut block.statements).into_vec(),
                block.pos,
                state,
                false,
            )
            .into();

            if block.len() == 1 {
                match block.statements[0] {
                    // while expr { break; } -> { expr; }
                    Stmt::Break(pos) => {
                        // Only a single break statement - turn into running the guard expression once
                        state.set_dirty();
                        if !condition.is_unit() {
                            let mut statements = vec![Stmt::Expr(mem::take(condition))];
                            if preserve_result {
                                statements.push(Stmt::Noop(pos))
                            }
                            *stmt = Stmt::Block(statements, pos);
                        } else {
                            *stmt = Stmt::Noop(pos);
                        };
                    }
                    _ => (),
                }
            }
        }
        // do { block } while false | do { block } until true -> { block }
        Stmt::Do(block, Expr::BoolConstant(true, _), false, _)
        | Stmt::Do(block, Expr::BoolConstant(false, _), true, _) => {
            state.set_dirty();
            *stmt = Stmt::Block(
                optimize_stmt_block(
                    mem::take(&mut block.statements).into_vec(),
                    block.pos,
                    state,
                    false,
                ),
                block.pos,
            );
        }
        // do { block } while|until expr
        Stmt::Do(block, condition, _, _) => {
            optimize_expr(condition, state);
            block.statements = optimize_stmt_block(
                mem::take(&mut block.statements).into_vec(),
                block.pos,
                state,
                false,
            )
            .into();
        }
        // for id in expr { block }
        Stmt::For(iterable, x, _) => {
            optimize_expr(iterable, state);
            x.1.statements = optimize_stmt_block(
                mem::take(&mut x.1.statements).into_vec(),
                x.1.pos,
                state,
                false,
            )
            .into();
        }
        // let id = expr;
        Stmt::Let(expr, _, _, _) => optimize_expr(expr, state),
        // import expr as var;
        #[cfg(not(feature = "no_module"))]
        Stmt::Import(expr, _, _) => optimize_expr(expr, state),
        // { block }
        Stmt::Block(statements, pos) => {
            *stmt = match optimize_stmt_block(mem::take(statements), *pos, state, preserve_result) {
                statements if statements.is_empty() => {
                    state.set_dirty();
                    Stmt::Noop(*pos)
                }
                // Only one statement - promote
                mut statements if statements.len() == 1 => {
                    state.set_dirty();
                    statements.pop().unwrap()
                }
                statements => Stmt::Block(statements, *pos),
            };
        }
        // try { pure block } catch ( var ) { block }
        Stmt::TryCatch(x, _, _) if x.0.statements.iter().all(Stmt::is_pure) => {
            // If try block is pure, there will never be any exceptions
            state.set_dirty();
            *stmt = Stmt::Block(
                optimize_stmt_block(
                    mem::take(&mut x.0.statements).into_vec(),
                    x.0.pos,
                    state,
                    false,
                ),
                x.0.pos,
            );
        }
        // try { block } catch ( var ) { block }
        Stmt::TryCatch(x, _, _) => {
            x.0.statements = optimize_stmt_block(
                mem::take(&mut x.0.statements).into_vec(),
                x.0.pos,
                state,
                false,
            )
            .into();
            x.2.statements = optimize_stmt_block(
                mem::take(&mut x.2.statements).into_vec(),
                x.2.pos,
                state,
                false,
            )
            .into();
        }
        // {}
        Stmt::Expr(Expr::Stmt(x, pos)) if x.is_empty() => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos);
        }
        // {...};
        Stmt::Expr(Expr::Stmt(x, pos)) => {
            state.set_dirty();
            *stmt = Stmt::Block(mem::take(x).into_vec(), *pos);
        }
        // expr;
        Stmt::Expr(expr) => optimize_expr(expr, state),
        // return expr;
        Stmt::Return(_, Some(ref mut expr), _) => optimize_expr(expr, state),

        // All other statements - skip
        _ => (),
    }
}

/// Optimize an [expression][Expr].
fn optimize_expr(expr: &mut Expr, state: &mut State) {
    // These keywords are handled specially
    const DONT_EVAL_KEYWORDS: &[&str] = &[
        KEYWORD_PRINT, // side effects
        KEYWORD_DEBUG, // side effects
        KEYWORD_EVAL,  // arbitrary scripts
    ];

    match expr {
        // {}
        Expr::Stmt(x, pos) if x.is_empty() => { state.set_dirty(); *expr = Expr::Unit(*pos) }
        // { stmt; ... } - do not count promotion as dirty because it gets turned back into an array
        Expr::Stmt(x, pos) =>  {
            let statements = optimize_stmt_block(mem::take(x).into_vec(), *pos, state, true);
            *expr = Expr::Stmt(Box::new(statements.into()), *pos);
        }
        // lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // map.string
            (Expr::Map(m, pos), Expr::Property(p)) if m.iter().all(|(_, x)| x.is_pure()) => {
                let prop = &p.4.name;
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(m).into_iter().find(|(x, _)| &x.name == prop)
                            .map(|(_, mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // var.rhs
            (Expr::Variable(_), rhs) => optimize_expr(rhs, state),
            // lhs.rhs
            (lhs, rhs) => { optimize_expr(lhs, state); optimize_expr(rhs, state); }
        }

        // lhs[rhs]
        #[cfg(not(feature = "no_index"))]
        Expr::Index(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // array[int]
            (Expr::Array(a, pos), Expr::IntegerConstant(i, _))
                if *i >= 0 && (*i as usize) < a.len() && a.iter().all(Expr::is_pure) =>
            {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let mut result = a.remove(*i as usize);
                result.set_position(*pos);
                *expr = result;
            }
            // map[string]
            (Expr::Map(m, pos), Expr::StringConstant(s, _)) if m.iter().all(|(_, x)| x.is_pure()) => {
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(m).into_iter().find(|(x, _)| x.name == *s)
                            .map(|(_, mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // string[int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, _)) if *i >= 0 && (*i as usize) < s.chars().count() => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().nth(*i as usize).unwrap(), *pos);
            }
            // var[rhs]
            (Expr::Variable(_), rhs) => optimize_expr(rhs, state),
            // lhs[rhs]
            (lhs, rhs) => { optimize_expr(lhs, state); optimize_expr(rhs, state); }
        },
        // [ constant .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(_, _) if expr.is_constant() => {
            state.set_dirty();
            *expr = Expr::DynamicConstant(Box::new(expr.get_constant_value().unwrap()), expr.position());
        }
        // [ items .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(x, _) => x.iter_mut().for_each(|expr| optimize_expr(expr, state)),
        // #{ key:constant, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(_, _) if expr.is_constant() => {
            state.set_dirty();
            *expr = Expr::DynamicConstant(Box::new(expr.get_constant_value().unwrap()), expr.position());
        }
        // #{ key:value, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(x, _) => x.iter_mut().for_each(|(_, expr)| optimize_expr(expr, state)),
        // lhs && rhs
        Expr::And(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // true && rhs -> rhs
            (Expr::BoolConstant(true, _), rhs) => {
                state.set_dirty();
                optimize_expr(rhs, state);
                *expr = mem::take(rhs);
            }
            // false && rhs -> false
            (Expr::BoolConstant(false, pos), _) => {
                state.set_dirty();
                *expr = Expr::BoolConstant(false, *pos);
            }
            // lhs && true -> lhs
            (lhs, Expr::BoolConstant(true, _)) => {
                state.set_dirty();
                optimize_expr(lhs, state);
                *expr = mem::take(lhs);
            }
            // lhs && rhs
            (lhs, rhs) => { optimize_expr(lhs, state); optimize_expr(rhs, state); }
        },
        // lhs || rhs
        Expr::Or(ref mut x, _) => match (&mut x.lhs, &mut x.rhs) {
            // false || rhs -> rhs
            (Expr::BoolConstant(false, _), rhs) => {
                state.set_dirty();
                optimize_expr(rhs, state);
                *expr = mem::take(rhs);
            }
            // true || rhs -> true
            (Expr::BoolConstant(true, pos), _) => {
                state.set_dirty();
                *expr = Expr::BoolConstant(true, *pos);
            }
            // lhs || false
            (lhs, Expr::BoolConstant(false, _)) => {
                state.set_dirty();
                optimize_expr(lhs, state);
                *expr = mem::take(lhs);
            }
            // lhs || rhs
            (lhs, rhs) => { optimize_expr(lhs, state); optimize_expr(rhs, state); }
        },

        // eval!
        Expr::FnCall(x, _) if x.name == KEYWORD_EVAL => {
            state.propagate_constants = false;
        }
        // Do not call some special keywords
        Expr::FnCall(x, _) if DONT_EVAL_KEYWORDS.contains(&x.name.as_ref()) => {
            x.args.iter_mut().for_each(|a| optimize_expr(a, state));
        }

        // Call built-in operators
        Expr::FnCall(x, pos)
                if x.namespace.is_none() // Non-qualified
                && state.optimization_level == OptimizationLevel::Simple // simple optimizations
                && x.args.len() == 2 // binary call
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
                //&& !is_valid_identifier(x.name.chars()) // cannot be scripted
        => {
            let mut arg_values: StaticVec<_> = x.args.iter().map(|e| e.get_constant_value().unwrap()).collect();
            let arg_types: StaticVec<_> = arg_values.iter().map(Dynamic::type_id).collect();

            // Search for overloaded operators (can override built-in).
            if !has_native_fn(state, x.hash.native_hash(), arg_types.as_ref()) {
                if let Some(result) = get_builtin_binary_op_fn(x.name.as_ref(), &arg_values[0], &arg_values[1])
                                        .and_then(|f| {
                                            let ctx = (state.engine, x.name.as_ref(), state.lib).into();
                                            let (first, second) = arg_values.split_first_mut().unwrap();
                                            (f)(ctx, &mut [ first, &mut second[0] ]).ok()
                                        })
                                        .and_then(|result| map_dynamic_to_expr(result, *pos))
                {
                    state.set_dirty();
                    *expr = result;
                    return;
                }
            }

            x.args.iter_mut().for_each(|a| optimize_expr(a, state));
        }

        // Eagerly call functions
        Expr::FnCall(x, pos)
                if x.namespace.is_none() // Non-qualified
                && state.optimization_level == OptimizationLevel::Full // full optimizations
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
        => {
            // First search for script-defined functions (can override built-in)
            #[cfg(not(feature = "no_function"))]
            let has_script_fn = state.lib.iter().any(|&m| m.get_script_fn(x.name.as_ref(), x.args.len(), false).is_some());
            #[cfg(feature = "no_function")]
            let has_script_fn = false;

            if !has_script_fn {
                let mut arg_values: StaticVec<_> = x.args.iter().map(|e| e.get_constant_value().unwrap()).collect();

                // Save the typename of the first argument if it is `type_of()`
                // This is to avoid `call_args` being passed into the closure
                let arg_for_type_of = if x.name == KEYWORD_TYPE_OF && arg_values.len() == 1 {
                    state.engine.map_type_name(arg_values[0].type_name())
                } else {
                    ""
                };

                if let Some(result) = call_fn_with_constant_arguments(&state, x.name.as_ref(), arg_values.as_mut())
                                        .or_else(|| {
                                            if !arg_for_type_of.is_empty() {
                                                // Handle `type_of()`
                                                Some(arg_for_type_of.to_string().into())
                                            } else {
                                                None
                                            }
                                        })
                                        .and_then(|result| map_dynamic_to_expr(result, *pos))
                {
                    state.set_dirty();
                    *expr = result;
                    return;
                }
            }

            x.args.iter_mut().for_each(|a| optimize_expr(a, state));
        }

        // id(args ..) -> optimize function call arguments
        Expr::FnCall(x, _) => x.args.iter_mut().for_each(|a| optimize_expr(a, state)),

        // constant-name
        Expr::Variable(x) if x.1.is_none() && state.find_constant(&x.2.name).is_some() => {
            state.set_dirty();

            // Replace constant with value
            let mut result = state.find_constant(&x.2.name).unwrap().clone();
            result.set_position(x.2.pos);
            *expr = result;
        }

        // Custom syntax
        Expr::Custom(x, _) => {
            if x.scope_delta != 0 {
                state.propagate_constants = false;
            }
            x.keywords.iter_mut().for_each(|expr| optimize_expr(expr, state));
        }

        // All other expressions - skip
        _ => (),
    }
}

/// Optimize a block of [statements][Stmt] at top level.
fn optimize_top_level(
    mut statements: Vec<Stmt>,
    engine: &Engine,
    scope: &Scope,
    lib: &[&Module],
    level: OptimizationLevel,
) -> Vec<Stmt> {
    // If optimization level is None then skip optimizing
    if level == OptimizationLevel::None {
        statements.shrink_to_fit();
        return statements;
    }

    // Set up the state
    let mut state = State::new(engine, lib, level);

    // Add constants and variables from the scope
    scope.iter().for_each(|(name, constant, value)| {
        if !constant {
            state.push_var(name, AccessMode::ReadWrite, Expr::Unit(Position::NONE));
        } else if let Some(val) = map_dynamic_to_expr(value, Position::NONE) {
            state.push_var(name, AccessMode::ReadOnly, val);
        } else {
            state.push_var(name, AccessMode::ReadOnly, Expr::Unit(Position::NONE));
        }
    });

    let orig_constants_len = state.variables.len();

    // Optimization loop
    loop {
        state.reset();
        state.restore_var(orig_constants_len);

        let num_statements = statements.len();

        statements.iter_mut().enumerate().for_each(|(i, stmt)| {
            match stmt {
                Stmt::Const(value_expr, Ident { name, .. }, _, _) => {
                    // Load constants
                    optimize_expr(value_expr, &mut state);

                    if value_expr.is_constant() {
                        state.push_var(name, AccessMode::ReadOnly, value_expr.clone());
                    }
                }
                Stmt::Let(value_expr, Ident { name, pos, .. }, _, _) => {
                    optimize_expr(value_expr, &mut state);
                    state.push_var(name, AccessMode::ReadWrite, Expr::Unit(*pos));
                }
                _ => {
                    // Keep all variable declarations at this level
                    // and always keep the last return value
                    let keep = match stmt {
                        Stmt::Let(_, _, _, _) | Stmt::Const(_, _, _, _) => true,
                        #[cfg(not(feature = "no_module"))]
                        Stmt::Import(_, _, _) => true,
                        _ => i >= num_statements - 1,
                    };
                    optimize_stmt(stmt, &mut state, keep);
                }
            }
        });

        if !state.is_dirty() {
            break;
        }
    }

    // Eliminate code that is pure but always keep the last statement
    let last_stmt = statements.pop();

    // Remove all pure statements at global level
    statements.retain(|stmt| !stmt.is_pure());

    // Add back the last statement unless it is a lone No-op
    if let Some(stmt) = last_stmt {
        if !statements.is_empty() || !stmt.is_noop() {
            statements.push(stmt);
        }
    }

    statements.shrink_to_fit();
    statements
}

/// Optimize an [`AST`].
pub fn optimize_into_ast(
    engine: &Engine,
    scope: &Scope,
    mut statements: Vec<Stmt>,
    _functions: Vec<crate::ast::ScriptFnDef>,
    level: OptimizationLevel,
) -> AST {
    let level = if cfg!(feature = "no_optimize") {
        OptimizationLevel::None
    } else {
        level
    };

    #[cfg(not(feature = "no_function"))]
    let lib = {
        let mut module = Module::new();

        if !level.is_none() {
            // We only need the script library's signatures for optimization purposes
            let mut lib2 = Module::new();

            _functions
                .iter()
                .map(|fn_def| crate::ast::ScriptFnDef {
                    name: fn_def.name.clone(),
                    access: fn_def.access,
                    body: Default::default(),
                    params: fn_def.params.clone(),
                    #[cfg(not(feature = "no_closure"))]
                    externals: fn_def.externals.clone(),
                    lib: None,
                    #[cfg(not(feature = "no_module"))]
                    mods: Default::default(),
                    comments: Default::default(),
                })
                .for_each(|fn_def| {
                    lib2.set_script_fn(fn_def);
                });

            _functions
                .into_iter()
                .map(|mut fn_def| {
                    let pos = fn_def.body.position();

                    // Optimize the function body
                    let mut body = optimize_top_level(
                        vec![fn_def.body],
                        engine,
                        &Scope::new(),
                        &[&lib2],
                        level,
                    );

                    // {} -> Noop
                    fn_def.body = match body.pop().unwrap_or_else(|| Stmt::Noop(pos)) {
                        // { return val; } -> val
                        Stmt::Return(crate::ast::ReturnType::Return, Some(expr), _) => {
                            Stmt::Expr(expr)
                        }
                        // { return; } -> ()
                        Stmt::Return(crate::ast::ReturnType::Return, None, pos) => {
                            Stmt::Expr(Expr::Unit(pos))
                        }
                        // All others
                        stmt => stmt,
                    };
                    fn_def
                })
                .for_each(|fn_def| {
                    module.set_script_fn(fn_def);
                });
        } else {
            _functions.into_iter().for_each(|fn_def| {
                module.set_script_fn(fn_def);
            });
        }

        module
    };

    #[cfg(feature = "no_function")]
    let lib = Default::default();

    statements.shrink_to_fit();

    AST::new(
        match level {
            OptimizationLevel::None => statements,
            OptimizationLevel::Simple | OptimizationLevel::Full => {
                optimize_top_level(statements, engine, &scope, &[&lib], level)
            }
        },
        lib,
    )
}
