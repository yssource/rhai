//! Module implementing the [`AST`] optimizer.

use crate::ast::{Expr, OpAssignment, Stmt};
use crate::dynamic::AccessMode;
use crate::engine::{KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_FN_PTR, KEYWORD_PRINT, KEYWORD_TYPE_OF};
use crate::fn_builtin::get_builtin_binary_op_fn;
use crate::parser::map_dynamic_to_expr;
use crate::token::Token;
use crate::utils::get_hasher;
use crate::{
    calc_fn_hash, calc_fn_params_hash, combine_hashes, Dynamic, Engine, FnPtr, ImmutableString,
    Module, Position, Scope, StaticVec, AST,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::TypeId,
    hash::{Hash, Hasher},
    iter::empty,
    mem,
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
    pub fn new(
        engine: &'a Engine,
        lib: &'a [&'a Module],
        optimization_level: OptimizationLevel,
    ) -> Self {
        Self {
            changed: false,
            variables: vec![],
            propagate_constants: true,
            engine,
            lib,
            optimization_level,
        }
    }
    /// Set the [`AST`] state to be dirty (i.e. changed).
    #[inline(always)]
    pub fn set_dirty(&mut self) {
        self.changed = true;
    }
    /// Set the [`AST`] state to be not dirty (i.e. unchanged).
    #[inline(always)]
    pub fn clear_dirty(&mut self) {
        self.changed = false;
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
    state.engine.global_namespace.contains_fn(hash)
            // Then check packages
            || state.engine.global_modules.iter().any(|m| m.contains_fn(hash))
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
    state: &mut State,
    preserve_result: bool,
    is_internal: bool,
    reduce_return: bool,
) -> Vec<Stmt> {
    if statements.is_empty() {
        return statements;
    }

    let mut is_dirty = state.is_dirty();

    let is_pure = if is_internal {
        Stmt::is_internally_pure
    } else {
        Stmt::is_pure
    };

    loop {
        state.clear_dirty();

        let orig_constants_len = state.variables.len(); // Original number of constants in the state, for restore later
        let orig_propagate_constants = state.propagate_constants;

        // Remove everything following control flow breaking statements
        let mut dead_code = false;

        statements.retain(|stmt| {
            if dead_code {
                state.set_dirty();
                false
            } else if stmt.is_control_flow_break() {
                dead_code = true;
                true
            } else {
                true
            }
        });

        // Optimize each statement in the block
        statements.iter_mut().for_each(|stmt| {
            match stmt {
                // Add constant literals into the state
                Stmt::Const(value_expr, x, _, _) => {
                    optimize_expr(value_expr, state);

                    if value_expr.is_constant() {
                        state.push_var(&x.name, AccessMode::ReadOnly, value_expr.clone());
                    }
                }
                // Add variables into the state
                Stmt::Let(value_expr, x, _, _) => {
                    optimize_expr(value_expr, state);
                    state.push_var(&x.name, AccessMode::ReadWrite, Expr::Unit(x.pos));
                }
                // Optimize the statement
                _ => optimize_stmt(stmt, state, preserve_result),
            }
        });

        // Remove all pure statements except the last one
        let mut index = 0;
        let mut first_non_constant = statements
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, stmt)| match stmt {
                stmt if !is_pure(stmt) => Some(i),

                Stmt::Let(e, _, _, _) | Stmt::Const(e, _, _, _) | Stmt::Expr(e)
                    if !e.is_constant() =>
                {
                    Some(i)
                }

                #[cfg(not(feature = "no_module"))]
                Stmt::Import(e, _, _) if !e.is_constant() => Some(i),

                _ => None,
            })
            .map_or(0, |n| statements.len() - n - 1);

        while index < statements.len() {
            if preserve_result && index >= statements.len() - 1 {
                break;
            } else {
                match &statements[index] {
                    stmt if is_pure(stmt) && index >= first_non_constant => {
                        state.set_dirty();
                        statements.remove(index);
                    }
                    stmt if stmt.is_pure() => {
                        state.set_dirty();
                        if index < first_non_constant {
                            first_non_constant -= 1;
                        }
                        statements.remove(index);
                    }
                    _ => index += 1,
                }
            }
        }

        // Remove all pure statements that do not return values at the end of a block.
        // We cannot remove anything for non-pure statements due to potential side-effects.
        if preserve_result {
            loop {
                match &mut statements[..] {
                    // { return; } -> {}
                    [Stmt::Return(crate::ast::ReturnType::Return, None, _)] if reduce_return => {
                        state.set_dirty();
                        statements.clear();
                    }
                    [stmt] if !stmt.returns_value() && is_pure(stmt) => {
                        state.set_dirty();
                        statements.clear();
                    }
                    // { ...; return; } -> { ... }
                    [.., last_stmt, Stmt::Return(crate::ast::ReturnType::Return, None, _)]
                        if reduce_return && !last_stmt.returns_value() =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    // { ...; return val; } -> { ...; val }
                    [.., Stmt::Return(crate::ast::ReturnType::Return, expr, pos)]
                        if reduce_return =>
                    {
                        state.set_dirty();
                        *statements.last_mut().unwrap() = if let Some(expr) = expr {
                            Stmt::Expr(mem::take(expr))
                        } else {
                            Stmt::Noop(*pos)
                        };
                    }
                    [.., second_last_stmt, Stmt::Noop(_)] if second_last_stmt.returns_value() => {}
                    [.., second_last_stmt, last_stmt]
                        if !last_stmt.returns_value() && is_pure(last_stmt) =>
                    {
                        state.set_dirty();
                        if second_last_stmt.returns_value() {
                            *statements.last_mut().unwrap() = Stmt::Noop(last_stmt.position());
                        } else {
                            statements.pop().unwrap();
                        }
                    }
                    _ => break,
                }
            }
        } else {
            loop {
                match &statements[..] {
                    [stmt] if is_pure(stmt) => {
                        state.set_dirty();
                        statements.clear();
                    }
                    // { ...; return; } -> { ... }
                    [.., Stmt::Return(crate::ast::ReturnType::Return, None, _)]
                        if reduce_return =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    // { ...; return pure_val; } -> { ... }
                    [.., Stmt::Return(crate::ast::ReturnType::Return, Some(expr), _)]
                        if reduce_return && expr.is_pure() =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    [.., last_stmt] if is_pure(last_stmt) => {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    _ => break,
                }
            }
        }

        // Pop the stack and remove all the local constants
        state.restore_var(orig_constants_len);
        state.propagate_constants = orig_propagate_constants;

        if !state.is_dirty() {
            break;
        }

        is_dirty = true;
    }

    if is_dirty {
        state.set_dirty();
    }

    statements.shrink_to_fit();
    statements
}

/// Optimize a [statement][Stmt].
fn optimize_stmt(stmt: &mut Stmt, state: &mut State, preserve_result: bool) {
    match stmt {
        // var = var op expr => var op= expr
        Stmt::Assignment(x, _)
            if x.1.is_none()
                && x.0.is_variable_access(true)
                && matches!(&x.2, Expr::FnCall(x2, _)
                        if Token::lookup_from_syntax(&x2.name).map(|t| t.has_op_assignment()).unwrap_or(false)
                        && x2.args_count() == 2 && x2.args.len() >= 1
                        && x2.args[0].get_variable_name(true) == x.0.get_variable_name(true)
                ) =>
        {
            match &mut x.2 {
                Expr::FnCall(x2, _) => {
                    state.set_dirty();
                    let op = Token::lookup_from_syntax(&x2.name).unwrap();
                    let op_assignment = op.make_op_assignment().unwrap();
                    x.1 = Some(OpAssignment::new(op_assignment));
                    x.2 = if x2.args.len() > 1 {
                        mem::take(&mut x2.args[1])
                    } else {
                        let (value, pos) = mem::take(&mut x2.literal_args[0]);
                        Expr::DynamicConstant(Box::new(value), pos)
                    };
                }
                _ => unreachable!(),
            }
        }

        // expr op= expr
        Stmt::Assignment(x, _) => match x.0 {
            Expr::Variable(_, _, _) => optimize_expr(&mut x.2, state),
            _ => {
                optimize_expr(&mut x.0, state);
                optimize_expr(&mut x.2, state);
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
                Stmt::Block(Box::new([Stmt::Expr(expr), Stmt::Noop(pos)]), pos)
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
            let else_block = mem::take(&mut *x.1).into_vec();
            *stmt = match optimize_stmt_block(else_block, state, preserve_result, true, false) {
                statements if statements.is_empty() => Stmt::Noop(x.1.position()),
                statements => Stmt::Block(statements.into_boxed_slice(), x.1.position()),
            }
        }
        // if true { if_block } else { else_block } -> if_block
        Stmt::If(Expr::BoolConstant(true, _), x, _) => {
            state.set_dirty();
            let if_block = mem::take(&mut *x.0).into_vec();
            *stmt = match optimize_stmt_block(if_block, state, preserve_result, true, false) {
                statements if statements.is_empty() => Stmt::Noop(x.0.position()),
                statements => Stmt::Block(statements.into_boxed_slice(), x.0.position()),
            }
        }
        // if expr { if_block } else { else_block }
        Stmt::If(condition, x, _) => {
            optimize_expr(condition, state);
            let if_block = mem::take(x.0.statements()).into_vec();
            *x.0.statements() =
                optimize_stmt_block(if_block, state, preserve_result, true, false).into();
            let else_block = mem::take(x.1.statements()).into_vec();
            *x.1.statements() =
                optimize_stmt_block(else_block, state, preserve_result, true, false).into();
        }

        // switch const { ... }
        Stmt::Switch(match_expr, x, pos) if match_expr.is_constant() => {
            let value = match_expr.get_constant_value().unwrap();
            let hasher = &mut get_hasher();
            value.hash(hasher);
            let hash = hasher.finish();

            state.set_dirty();
            let table = &mut x.0;

            if let Some(block) = table.get_mut(&hash) {
                if let Some(mut condition) = mem::take(&mut block.0) {
                    // switch const { case if condition => stmt, _ => def } => if condition { stmt } else { def }
                    optimize_expr(&mut condition, state);

                    let def_block = mem::take(&mut *x.1).into_vec();
                    let def_stmt = optimize_stmt_block(def_block, state, true, true, false);
                    let def_pos = if x.1.position().is_none() {
                        *pos
                    } else {
                        x.1.position()
                    };

                    *stmt = Stmt::If(
                        condition,
                        Box::new((
                            mem::take(&mut block.1),
                            Stmt::Block(def_stmt.into_boxed_slice(), def_pos).into(),
                        )),
                        match_expr.position(),
                    );
                } else {
                    // Promote the matched case
                    let new_pos = block.1.position();
                    let statements = mem::take(&mut *block.1);
                    let statements =
                        optimize_stmt_block(statements.into_vec(), state, true, true, false);
                    *stmt = Stmt::Block(statements.into_boxed_slice(), new_pos);
                }
            } else {
                // Promote the default case
                let def_block = mem::take(&mut *x.1).into_vec();
                let def_stmt = optimize_stmt_block(def_block, state, true, true, false);
                let def_pos = if x.1.position().is_none() {
                    *pos
                } else {
                    x.1.position()
                };
                *stmt = Stmt::Block(def_stmt.into_boxed_slice(), def_pos);
            }
        }
        // switch
        Stmt::Switch(match_expr, x, _) => {
            optimize_expr(match_expr, state);
            x.0.values_mut().for_each(|block| {
                let condition = if let Some(mut condition) = mem::take(&mut block.0) {
                    optimize_expr(&mut condition, state);
                    condition
                } else {
                    Expr::Unit(Position::NONE)
                };

                match condition {
                    Expr::Unit(_) | Expr::BoolConstant(true, _) => (),
                    _ => {
                        block.0 = Some(condition);

                        *block.1.statements() = optimize_stmt_block(
                            mem::take(block.1.statements()).into_vec(),
                            state,
                            preserve_result,
                            true,
                            false,
                        )
                        .into();
                    }
                }
            });

            // Remove false cases
            while let Some((&key, _)) = x.0.iter().find(|(_, block)| match block.0 {
                Some(Expr::BoolConstant(false, _)) => true,
                _ => false,
            }) {
                state.set_dirty();
                x.0.remove(&key);
            }

            let def_block = mem::take(x.1.statements()).into_vec();
            *x.1.statements() =
                optimize_stmt_block(def_block, state, preserve_result, true, false).into();
        }

        // while false { block } -> Noop
        Stmt::While(Expr::BoolConstant(false, pos), _, _) => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos)
        }
        // while expr { block }
        Stmt::While(condition, body, _) => {
            optimize_expr(condition, state);
            let block = mem::take(body.statements()).into_vec();
            *body.statements() = optimize_stmt_block(block, state, false, true, false).into();

            if body.len() == 1 {
                match body[0] {
                    // while expr { break; } -> { expr; }
                    Stmt::Break(pos) => {
                        // Only a single break statement - turn into running the guard expression once
                        state.set_dirty();
                        if !condition.is_unit() {
                            let mut statements = vec![Stmt::Expr(mem::take(condition))];
                            if preserve_result {
                                statements.push(Stmt::Noop(pos))
                            }
                            *stmt = Stmt::Block(statements.into_boxed_slice(), pos);
                        } else {
                            *stmt = Stmt::Noop(pos);
                        };
                    }
                    _ => (),
                }
            }
        }
        // do { block } while false | do { block } until true -> { block }
        Stmt::Do(body, Expr::BoolConstant(true, _), false, _)
        | Stmt::Do(body, Expr::BoolConstant(false, _), true, _) => {
            state.set_dirty();
            let block_pos = body.position();
            let block = mem::take(body.statements()).into_vec();
            *stmt = Stmt::Block(
                optimize_stmt_block(block, state, false, true, false).into_boxed_slice(),
                block_pos,
            );
        }
        // do { block } while|until expr
        Stmt::Do(body, condition, _, _) => {
            optimize_expr(condition, state);
            let block = mem::take(body.statements()).into_vec();
            *body.statements() = optimize_stmt_block(block, state, false, true, false).into();
        }
        // for id in expr { block }
        Stmt::For(iterable, x, _) => {
            optimize_expr(iterable, state);
            let body = mem::take(x.1.statements()).into_vec();
            *x.1.statements() = optimize_stmt_block(body, state, false, true, false).into();
        }
        // let id = expr;
        Stmt::Let(expr, _, _, _) => optimize_expr(expr, state),
        // import expr as var;
        #[cfg(not(feature = "no_module"))]
        Stmt::Import(expr, _, _) => optimize_expr(expr, state),
        // { block }
        Stmt::Block(statements, pos) => {
            let statements = mem::take(statements).into_vec();
            let mut block = optimize_stmt_block(statements, state, preserve_result, true, false);

            match block.as_mut_slice() {
                [] => {
                    state.set_dirty();
                    *stmt = Stmt::Noop(*pos);
                }
                // Only one statement - promote
                [s] => {
                    state.set_dirty();
                    *stmt = mem::take(s);
                }
                _ => *stmt = Stmt::Block(block.into_boxed_slice(), *pos),
            }
        }
        // try { pure try_block } catch ( var ) { catch_block } -> try_block
        Stmt::TryCatch(x, _, _) if x.0.iter().all(Stmt::is_pure) => {
            // If try block is pure, there will never be any exceptions
            state.set_dirty();
            let try_pos = x.0.position();
            let try_block = mem::take(&mut *x.0).into_vec();
            *stmt = Stmt::Block(
                optimize_stmt_block(try_block, state, false, true, false).into_boxed_slice(),
                try_pos,
            );
        }
        // try { try_block } catch ( var ) { catch_block }
        Stmt::TryCatch(x, _, _) => {
            let try_block = mem::take(x.0.statements()).into_vec();
            *x.0.statements() = optimize_stmt_block(try_block, state, false, true, false).into();
            let catch_block = mem::take(x.2.statements()).into_vec();
            *x.2.statements() = optimize_stmt_block(catch_block, state, false, true, false).into();
        }
        // func(...)
        Stmt::Expr(expr @ Expr::FnCall(_, _)) => {
            optimize_expr(expr, state);
            match expr {
                Expr::FnCall(x, pos) => {
                    state.set_dirty();
                    *stmt = Stmt::FnCall(mem::take(x), *pos);
                }
                _ => (),
            }
        }
        // {}
        Stmt::Expr(Expr::Stmt(x)) if x.is_empty() => {
            state.set_dirty();
            *stmt = Stmt::Noop(x.position());
        }
        // {...};
        Stmt::Expr(Expr::Stmt(x)) => {
            state.set_dirty();
            *stmt = mem::take(x.as_mut()).into();
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
        Expr::Stmt(x) if x.is_empty() => { state.set_dirty(); *expr = Expr::Unit(x.position()) }
        // { stmt; ... } - do not count promotion as dirty because it gets turned back into an array
        Expr::Stmt(x) => {
            *x.statements() = optimize_stmt_block(mem::take(x.statements()).into_vec(), state, true, true, false).into();

            // { Stmt(Expr) } - promote
            match x.as_mut().as_mut() {
                [ Stmt::Expr(e) ] => { state.set_dirty(); *expr = mem::take(e); }
                _ => ()
            }
        }
        // lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // map.string
            (Expr::Map(m, pos), Expr::Property(p)) if m.0.iter().all(|(_, x)| x.is_pure()) => {
                let prop = &p.2.name;
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(&mut m.0).into_iter().find(|(x, _)| &x.name == prop)
                            .map(|(_, mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // var.rhs
            (Expr::Variable(_, _, _), rhs) => optimize_expr(rhs, state),
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
            // array[-int]
            (Expr::Array(a, pos), Expr::IntegerConstant(i, _))
                if *i < 0 && i.checked_abs().map(|n| n as usize <= a.len()).unwrap_or(false) && a.iter().all(Expr::is_pure) =>
            {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let mut result = a.remove(a.len() - i.abs() as usize);
                result.set_position(*pos);
                *expr = result;
            }
            // map[string]
            (Expr::Map(m, pos), Expr::StringConstant(s, _)) if m.0.iter().all(|(_, x)| x.is_pure()) => {
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(&mut m.0).into_iter().find(|(x, _)| x.name.as_str() == s.as_str())
                            .map(|(_, mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // string[int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, _)) if *i >= 0 && (*i as usize) < s.chars().count() => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().nth(*i as usize).unwrap(), *pos);
            }
            // string[-int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, _)) if *i < 0 && i.checked_abs().map(|n| n as usize <= s.chars().count()).unwrap_or(false) => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().rev().nth(i.abs() as usize - 1).unwrap(), *pos);
            }
            // var[rhs]
            (Expr::Variable(_, _, _), rhs) => optimize_expr(rhs, state),
            // lhs[rhs]
            (lhs, rhs) => { optimize_expr(lhs, state); optimize_expr(rhs, state); }
        },
        // ``
        Expr::InterpolatedString(x) if x.is_empty() => {
            state.set_dirty();
            *expr = Expr::StringConstant(state.engine.empty_string.clone(), Position::NONE);
        }
        // `...`
        Expr::InterpolatedString(x) if x.len() == 1 && matches!(x[0], Expr::StringConstant(_, _)) => {
            state.set_dirty();
            *expr = mem::take(&mut x[0]);
        }
        // `... ${ ... } ...`
        Expr::InterpolatedString(x) => {
            x.iter_mut().for_each(|expr| optimize_expr(expr, state));

            let mut n= 0;

            // Merge consecutive strings
            while n < x.len()-1 {
                match (mem::take(&mut x[n]), mem::take(&mut x[n+1])) {
                    (Expr::StringConstant(mut s1, pos), Expr::StringConstant(s2, _)) => {
                        s1 += s2;
                        x[n] = Expr::StringConstant(s1, pos);
                        x.remove(n+1);
                        state.set_dirty();
                    }
                    (expr1, Expr::Unit(_))  => {
                        x[n] = expr1;
                        x.remove(n+1);
                        state.set_dirty();
                    }
                    (Expr::Unit(_), expr2) => {
                        x[n+1] = expr2;
                        x.remove(n);
                        state.set_dirty();
                    }
                    (expr1, Expr::StringConstant(s, _)) if s.is_empty() => {
                        x[n] = expr1;
                        x.remove(n+1);
                        state.set_dirty();
                    }
                    (Expr::StringConstant(s, _), expr2) if s.is_empty()=> {
                        x[n+1] = expr2;
                        x.remove(n);
                        state.set_dirty();
                    }
                    (expr1, expr2) => {
                        x[n] = expr1;
                        x[n+1] = expr2;
                        n += 1;
                    }
                }
            }

            x.shrink_to_fit();
        }
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
        Expr::Map(x, _) => x.0.iter_mut().for_each(|(_, expr)| optimize_expr(expr, state)),
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
        // Fn
        Expr::FnCall(x, pos)
            if !x.is_qualified() // Non-qualified
            && state.optimization_level == OptimizationLevel::Simple // simple optimizations
            && x.args_count() == 1
            && x.literal_args.len() == 1
            && x.literal_args[0].0.is::<ImmutableString>()
            && x.name == KEYWORD_FN_PTR
        => {
            state.set_dirty();
            let fn_ptr = FnPtr::new_unchecked(mem::take(&mut x.literal_args[0].0).as_str_ref().unwrap().into(), Default::default());
            *expr = Expr::DynamicConstant(Box::new(fn_ptr.into()), *pos);
        }

        // Do not call some special keywords
        Expr::FnCall(x, _) if DONT_EVAL_KEYWORDS.contains(&x.name.as_ref()) => {
            x.args.iter_mut().for_each(|a| optimize_expr(a, state));
        }

        // Call built-in operators
        Expr::FnCall(x, pos)
                if !x.is_qualified() // Non-qualified
                && state.optimization_level == OptimizationLevel::Simple // simple optimizations
                && x.args_count() == 2 // binary call
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
                //&& !is_valid_identifier(x.name.chars()) // cannot be scripted
        => {
            let mut arg_values: StaticVec<_> = x.args.iter().map(|e| e.get_constant_value().unwrap())
                                                .chain(x.literal_args.iter().map(|(v, _)| v).cloned())
                                                .collect();

            let arg_types: StaticVec<_> = arg_values.iter().map(Dynamic::type_id).collect();

            // Search for overloaded operators (can override built-in).
            if !has_native_fn(state, x.hashes.native_hash(), arg_types.as_ref()) {
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

            // Move constant arguments to the right
            while x.args.last().map(Expr::is_constant).unwrap_or(false) {
                let arg = x.args.pop().unwrap();
                let arg_pos = arg.position();
                x.literal_args.insert(0, (arg.get_constant_value().unwrap(), arg_pos));
            }

            x.args.shrink_to_fit();
            x.literal_args.shrink_to_fit();
        }

        // Eagerly call functions
        Expr::FnCall(x, pos)
                if !x.is_qualified() // Non-qualified
                && state.optimization_level == OptimizationLevel::Full // full optimizations
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
        => {
            // First search for script-defined functions (can override built-in)
            #[cfg(not(feature = "no_function"))]
            let has_script_fn = state.lib.iter().any(|&m| m.get_script_fn(x.name.as_ref(), x.args_count()).is_some());
            #[cfg(feature = "no_function")]
            let has_script_fn = false;

            if !has_script_fn {
                let mut arg_values: StaticVec<_> = x.args.iter().map(|e| e.get_constant_value().unwrap())
                                                    .chain(x.literal_args.iter().map(|(v, _)| v).cloned())
                                                    .collect();

                // Save the typename of the first argument if it is `type_of()`
                // This is to avoid `call_args` being passed into the closure
                let arg_for_type_of = if x.name == KEYWORD_TYPE_OF && arg_values.len() == 1 {
                    state.engine.map_type_name(arg_values[0].type_name())
                } else {
                    ""
                };

                if let Some(result) = call_fn_with_constant_arguments(&state, x.name.as_ref(), &mut arg_values)
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
        Expr::FnCall(x, _) => {
            x.args.iter_mut().for_each(|a| optimize_expr(a, state));

            // Move constant arguments to the right
            while x.args.last().map(Expr::is_constant).unwrap_or(false) {
                let arg = x.args.pop().unwrap();
                let arg_pos = arg.position();
                x.literal_args.insert(0, (arg.get_constant_value().unwrap(), arg_pos));
            }

            x.args.shrink_to_fit();
            x.literal_args.shrink_to_fit();
        }

        // constant-name
        Expr::Variable(_, pos, x) if x.1.is_none() && state.find_constant(&x.2).is_some() => {
            state.set_dirty();

            // Replace constant with value
            let mut result = state.find_constant(&x.2).unwrap().clone();
            result.set_position(*pos);
            *expr = result;
        }

        // Custom syntax
        Expr::Custom(x, _) => {
            if x.scope_changed {
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
    optimization_level: OptimizationLevel,
) -> Vec<Stmt> {
    // If optimization level is None then skip optimizing
    if optimization_level == OptimizationLevel::None {
        statements.shrink_to_fit();
        return statements;
    }

    // Set up the state
    let mut state = State::new(engine, lib, optimization_level);

    // Add constants and variables from the scope
    scope.iter().for_each(|(name, constant, value)| {
        if !constant {
            state.push_var(name, AccessMode::ReadWrite, Expr::Unit(Position::NONE));
        } else {
            state.push_var(
                name,
                AccessMode::ReadOnly,
                Expr::DynamicConstant(Box::new(value), Position::NONE),
            );
        }
    });

    statements = optimize_stmt_block(statements, &mut state, true, false, true);
    statements
}

/// Optimize an [`AST`].
pub fn optimize_into_ast(
    engine: &Engine,
    scope: &Scope,
    mut statements: Vec<Stmt>,
    _functions: Vec<crate::Shared<crate::ast::ScriptFnDef>>,
    optimization_level: OptimizationLevel,
) -> AST {
    let level = if cfg!(feature = "no_optimize") {
        OptimizationLevel::None
    } else {
        optimization_level
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
                    #[cfg(not(feature = "no_function"))]
                    #[cfg(feature = "metadata")]
                    comments: Default::default(),
                })
                .for_each(|fn_def| {
                    lib2.set_script_fn(fn_def);
                });

            let lib2 = &[&lib2];

            _functions
                .into_iter()
                .map(|fn_def| {
                    let mut fn_def = crate::fn_native::shared_take_or_clone(fn_def);

                    // Optimize the function body
                    let state = &mut State::new(engine, lib2, level);

                    let body = mem::take(fn_def.body.statements()).into_vec();

                    *fn_def.body.statements() =
                        optimize_stmt_block(body, state, true, true, true).into();

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
