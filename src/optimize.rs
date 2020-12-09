//! Module implementing the [`AST`] optimizer.

use crate::ast::{Expr, ScriptFnDef, Stmt};
use crate::dynamic::AccessMode;
use crate::engine::{KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_PRINT, KEYWORD_TYPE_OF};
use crate::fn_call::run_builtin_binary_op;
use crate::parser::map_dynamic_to_expr;
use crate::stdlib::{
    boxed::Box,
    hash::{Hash, Hasher},
    iter::empty,
    mem,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use crate::token::is_valid_identifier;
use crate::utils::get_hasher;
use crate::{calc_native_fn_hash, Dynamic, Engine, Module, Position, Scope, StaticVec, AST};

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
        for (n, access, expr) in self.variables.iter().rev() {
            if n == name {
                return if access.is_read_only() {
                    Some(expr)
                } else {
                    None
                };
            }
        }

        None
    }
}

/// Call a registered function
fn call_fn_with_constant_arguments(
    state: &State,
    fn_name: &str,
    arg_values: &mut [Dynamic],
) -> Option<Dynamic> {
    // Search built-in's and external functions
    let hash_fn = calc_native_fn_hash(empty(), fn_name, arg_values.iter().map(|a| a.type_id()));

    state
        .engine
        .call_native_fn(
            &mut Default::default(),
            &mut Default::default(),
            state.lib,
            fn_name,
            hash_fn,
            arg_values.iter_mut().collect::<StaticVec<_>>().as_mut(),
            false,
            true,
            None,
        )
        .ok()
        .map(|(v, _)| v)
}

/// Optimize a block of [statements][crate::ast::Stmt].
fn optimize_stmt_block(
    mut statements: Vec<Stmt>,
    pos: Position,
    state: &mut State,
    preserve_result: bool,
    count_promote_as_dirty: bool,
) -> Stmt {
    let orig_len = statements.len(); // Original number of statements in the block, for change detection
    let orig_constants_len = state.variables.len(); // Original number of constants in the state, for restore later

    // Optimize each statement in the block
    statements.iter_mut().for_each(|stmt| match stmt {
        // Add constant literals into the state
        Stmt::Const(var_def, Some(expr), _, _) if expr.is_constant() => {
            state.push_var(&var_def.name, AccessMode::ReadOnly, mem::take(expr));
        }
        Stmt::Const(var_def, None, _, _) => {
            state.push_var(&var_def.name, AccessMode::ReadOnly, Expr::Unit(var_def.pos));
        }
        // Add variables into the state
        Stmt::Let(var_def, _, _, _) => {
            state.push_var(
                &var_def.name,
                AccessMode::ReadWrite,
                Expr::Unit(var_def.pos),
            );
        }
        // Optimize the statement
        _ => optimize_stmt(stmt, state, preserve_result),
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
            Stmt::Let(_, expr, _, _) | Stmt::Const(_, expr, _, _) => {
                removed = expr.as_ref().map(Expr::is_pure).unwrap_or(true)
            }
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
            .for_each(|(i, stmt)| optimize_stmt(stmt, state, i == num_statements));
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

    match &statements[..] {
        // No statements in block - change to No-op
        [] => {
            state.set_dirty();
            Stmt::Noop(pos)
        }
        // Only one let statement - leave it alone
        [x] if matches!(x, Stmt::Let(_, _, _, _)) => Stmt::Block(statements, pos),
        // Only one const statement - leave it alone
        [x] if matches!(x, Stmt::Const(_, _, _, _)) => Stmt::Block(statements, pos),
        // Only one import statement - leave it alone
        #[cfg(not(feature = "no_module"))]
        [x] if matches!(x, Stmt::Import(_, _, _)) => Stmt::Block(statements, pos),
        // Only one statement - promote
        [_] => {
            if count_promote_as_dirty {
                state.set_dirty();
            }
            statements.remove(0)
        }
        _ => Stmt::Block(statements, pos),
    }
}

/// Optimize a [statement][crate::ast::Stmt].
fn optimize_stmt(stmt: &mut Stmt, state: &mut State, preserve_result: bool) {
    match stmt {
        // expr op= expr
        Stmt::Assignment(x, _) => match x.0 {
            Expr::Variable(_) => optimize_expr(&mut x.2, state),
            _ => {
                optimize_expr(&mut x.0, state);
                optimize_expr(&mut x.2, state);
            }
        },

        // if false { if_block } -> Noop
        Stmt::If(Expr::BoolConstant(false, pos), x, _) if x.1.is_none() => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos);
        }
        // if true { if_block } -> if_block
        Stmt::If(Expr::BoolConstant(true, _), x, _) if x.1.is_none() => {
            *stmt = mem::take(&mut x.0);
            optimize_stmt(stmt, state, true);
        }
        // if expr { Noop }
        Stmt::If(condition, x, _) if x.1.is_none() && matches!(x.0, Stmt::Noop(_)) => {
            state.set_dirty();

            let pos = condition.position();
            let mut expr = mem::take(condition);
            optimize_expr(&mut expr, state);

            *stmt = if preserve_result {
                // -> { expr, Noop }
                let mut statements = Vec::new();
                statements.push(Stmt::Expr(expr));
                statements.push(mem::take(&mut x.0));
                Stmt::Block(statements, pos)
            } else {
                // -> expr
                Stmt::Expr(expr)
            };
        }
        // if expr { if_block }
        Stmt::If(condition, x, _) if x.1.is_none() => {
            optimize_expr(condition, state);
            optimize_stmt(&mut x.0, state, true);
        }
        // if false { if_block } else { else_block } -> else_block
        Stmt::If(Expr::BoolConstant(false, _), x, _) if x.1.is_some() => {
            *stmt = mem::take(x.1.as_mut().unwrap());
            optimize_stmt(stmt, state, true);
        }
        // if true { if_block } else { else_block } -> if_block
        Stmt::If(Expr::BoolConstant(true, _), x, _) => {
            *stmt = mem::take(&mut x.0);
            optimize_stmt(stmt, state, true);
        }
        // if expr { if_block } else { else_block }
        Stmt::If(condition, x, _) => {
            optimize_expr(condition, state);
            optimize_stmt(&mut x.0, state, true);
            if let Some(else_block) = x.1.as_mut() {
                optimize_stmt(else_block, state, true);
                match else_block {
                    Stmt::Noop(_) => x.1 = None, // Noop -> no else block
                    _ => (),
                }
            }
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
                .for_each(|stmt| optimize_stmt(stmt, state, true));
            if let Some(def_stmt) = x.1.as_mut() {
                optimize_stmt(def_stmt, state, true);

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
            optimize_stmt(block, state, false);
            optimize_expr(condition, state);

            match **block {
                // while expr { break; } -> { expr; }
                Stmt::Break(pos) => {
                    // Only a single break statement - turn into running the guard expression once
                    state.set_dirty();
                    let mut statements = Vec::new();
                    statements.push(Stmt::Expr(mem::take(condition)));
                    if preserve_result {
                        statements.push(Stmt::Noop(pos))
                    }
                    *stmt = Stmt::Block(statements, pos);
                }
                _ => (),
            }
        }
        // do { block } while false | do { block } until true -> { block }
        Stmt::Do(block, Expr::BoolConstant(true, _), false, _)
        | Stmt::Do(block, Expr::BoolConstant(false, _), true, _) => {
            state.set_dirty();
            optimize_stmt(block.as_mut(), state, false);
            *stmt = mem::take(block.as_mut());
        }
        // do { block } while|until expr
        Stmt::Do(block, condition, _, _) => {
            optimize_stmt(block.as_mut(), state, false);
            optimize_expr(condition, state);
        }
        // for id in expr { block }
        Stmt::For(iterable, x, _) => {
            optimize_expr(iterable, state);
            optimize_stmt(&mut x.1, state, false);
        }
        // let id = expr;
        Stmt::Let(_, Some(expr), _, _) => optimize_expr(expr, state),
        // let id;
        Stmt::Let(_, None, _, _) => (),
        // import expr as var;
        #[cfg(not(feature = "no_module"))]
        Stmt::Import(expr, _, _) => optimize_expr(expr, state),
        // { block }
        Stmt::Block(statements, pos) => {
            *stmt = optimize_stmt_block(mem::take(statements), *pos, state, preserve_result, true);
        }
        // try { block } catch ( var ) { block }
        Stmt::TryCatch(x, _, _) if x.0.is_pure() => {
            // If try block is pure, there will never be any exceptions
            state.set_dirty();
            let pos = x.0.position();
            optimize_stmt(&mut x.0, state, preserve_result);
            let mut statements = match mem::take(&mut x.0) {
                Stmt::Block(statements, _) => statements,
                stmt => vec![stmt],
            };
            statements.push(Stmt::Noop(pos));
            *stmt = Stmt::Block(statements, pos);
        }
        // try { block } catch ( var ) { block }
        Stmt::TryCatch(x, _, _) => {
            optimize_stmt(&mut x.0, state, false);
            optimize_stmt(&mut x.2, state, false);
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

/// Optimize an [expression][crate::ast::Expr].
fn optimize_expr(expr: &mut Expr, state: &mut State) {
    // These keywords are handled specially
    const DONT_EVAL_KEYWORDS: &[&str] = &[
        KEYWORD_PRINT, // side effects
        KEYWORD_DEBUG, // side effects
        KEYWORD_EVAL,  // arbitrary scripts
    ];

    match expr {
        // expr - do not promote because there is a reason it is wrapped in an `Expr::Expr`
        Expr::Expr(x) => optimize_expr(x, state),
        // {}
        Expr::Stmt(x, pos) if x.is_empty() => { state.set_dirty(); *expr = Expr::Unit(*pos) }
        // { stmt; ... } - do not count promotion as dirty because it gets turned back into an array
        Expr::Stmt(x, pos) => match optimize_stmt_block(mem::take(x).into_vec(), *pos, state, true, false) {
            // {}
            Stmt::Noop(_) => { state.set_dirty(); *expr = Expr::Unit(*pos); }
            // { stmt, .. }
            Stmt::Block(statements, _) => *x = Box::new(statements.into()),
            // { expr }
            Stmt::Expr(inner) => { state.set_dirty(); *expr = inner; }
            // { stmt }
            stmt => x.push(stmt),
        }

        // lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // map.string
            (Expr::Map(m, pos), Expr::Property(p)) if m.iter().all(|(_, x)| x.is_pure()) => {
                let prop = &p.1.name;
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
        Expr::Map(_, _) if expr.is_constant()=> {
            state.set_dirty();
            *expr = Expr::DynamicConstant(Box::new(expr.get_constant_value().unwrap()), expr.position());
        }
        // #{ key:value, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(x, _) => x.iter_mut().for_each(|(_, expr)| optimize_expr(expr, state)),
        // lhs in rhs
        Expr::In(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // "xxx" in "xxxxx"
            (Expr::StringConstant(a, pos), Expr::StringConstant(b, _)) => {
                state.set_dirty();
                *expr = Expr::BoolConstant( b.contains(a.as_str()), *pos);
            }
            // 'x' in "xxxxx"
            (Expr::CharConstant(a, pos), Expr::StringConstant(b, _)) => {
                state.set_dirty();
                *expr = Expr::BoolConstant(b.contains(*a), *pos);
            }
            // "xxx" in #{...}
            (Expr::StringConstant(a, pos), Expr::Map(b, _)) => {
                state.set_dirty();
                *expr = Expr::BoolConstant(b.iter().find(|(x, _)| x.name == *a).is_some(), *pos);
            }
            // 'x' in #{...}
            (Expr::CharConstant(a, pos), Expr::Map(b, _)) => {
                state.set_dirty();
                let ch = a.to_string();
                *expr = Expr::BoolConstant(b.iter().find(|(x, _)| x.name == &ch).is_some(), *pos);
            }
            // lhs in rhs
            (lhs, rhs) => { optimize_expr(lhs, state); optimize_expr(rhs, state); }
        },
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
                && !is_valid_identifier(x.name.chars()) // cannot be scripted
        => {
            let arg_values: StaticVec<_> = x.args.iter().map(|e| e.get_constant_value().unwrap()).collect();
            let arg_types: StaticVec<_> = arg_values.iter().map(Dynamic::type_id).collect();

            // Search for overloaded operators (can override built-in).
            if !state.engine.has_override_by_name_and_arguments(Some(&state.engine.global_sub_modules), state.lib, x.name.as_ref(), arg_types.as_ref(), false) {
                if let Some(result) = run_builtin_binary_op(x.name.as_ref(), &arg_values[0], &arg_values[1])
                                        .ok().flatten()
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
                                                // Otherwise use the default value, if any
                                                x.def_value.clone()
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
        Expr::Variable(x) if x.1.is_none() && state.find_constant(&x.3.name).is_some() => {
            state.set_dirty();

            // Replace constant with value
            let mut result = state.find_constant(&x.3.name).unwrap().clone();
            result.set_position(x.3.pos);
            *expr = result;
        }

        // Custom syntax
        Expr::Custom(x, _) => x.keywords.iter_mut().for_each(|expr| optimize_expr(expr, state)),

        // All other expressions - skip
        _ => (),
    }
}

/// Optimize a block of [statements][crate::ast::Stmt] at top level.
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
                Stmt::Const(var_def, expr, _, _) if expr.is_some() => {
                    // Load constants
                    let value_expr = expr.as_mut().unwrap();
                    optimize_expr(value_expr, &mut state);

                    if value_expr.is_constant() {
                        state.push_var(&var_def.name, AccessMode::ReadOnly, value_expr.clone());
                    }

                    // Keep it in the global scope
                    if value_expr.is_unit() {
                        state.set_dirty();
                        *expr = None;
                    }
                }
                Stmt::Const(var_def, None, _, _) => {
                    state.push_var(&var_def.name, AccessMode::ReadOnly, Expr::Unit(var_def.pos));
                }
                Stmt::Let(var_def, _, _, _) => {
                    state.push_var(
                        &var_def.name,
                        AccessMode::ReadWrite,
                        Expr::Unit(var_def.pos),
                    );
                }
                _ => {
                    // Keep all variable declarations at this level
                    // and always keep the last return value
                    let keep = match stmt {
                        Stmt::Let(_, _, _, _) | Stmt::Const(_, _, _, _) => true,
                        #[cfg(not(feature = "no_module"))]
                        Stmt::Import(_, _, _) => true,
                        _ => i == num_statements - 1,
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
    _functions: Vec<ScriptFnDef>,
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
                .map(|fn_def| ScriptFnDef {
                    name: fn_def.name.clone(),
                    access: fn_def.access,
                    body: Default::default(),
                    params: fn_def.params.clone(),
                    #[cfg(not(feature = "no_closure"))]
                    externals: fn_def.externals.clone(),
                    lib: None,
                    #[cfg(not(feature = "no_module"))]
                    mods: Default::default(),
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
                        Stmt::Return((crate::ast::ReturnType::Return, _), Some(expr), _) => {
                            Stmt::Expr(expr)
                        }
                        // { return; } -> ()
                        Stmt::Return((crate::ast::ReturnType::Return, pos), None, _) => {
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
