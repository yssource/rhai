//! Module defining functions for evaluating a statement.

use super::{EvalState, GlobalRuntimeState, Target};
use crate::ast::{Expr, Ident, OpAssignment, Stmt, AST_OPTION_FLAGS::*};
use crate::func::get_hasher;
use crate::r#unsafe::unsafe_cast_var_name_to_lifetime;
use crate::types::dynamic::{AccessMode, Union};
use crate::{Dynamic, Engine, Module, Position, RhaiResult, RhaiResultOf, Scope, ERR, INT};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    borrow::Cow,
    hash::{Hash, Hasher},
};

impl Engine {
    /// Evaluate a statements block.
    pub(crate) fn eval_stmt_block(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        statements: &[Stmt],
        restore_orig_state: bool,
        level: usize,
    ) -> RhaiResult {
        if statements.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        let orig_always_search_scope = state.always_search_scope;
        let orig_scope_len = scope.len();
        let orig_mods_len = global.num_imported_modules();
        let orig_fn_resolution_caches_len = state.fn_resolution_caches_len();

        if restore_orig_state {
            state.scope_level += 1;
        }

        let mut result = Dynamic::UNIT;

        for stmt in statements {
            let _mods_len = global.num_imported_modules();

            result = self.eval_stmt(
                scope,
                global,
                state,
                lib,
                this_ptr,
                stmt,
                restore_orig_state,
                level,
            )?;

            #[cfg(not(feature = "no_module"))]
            if matches!(stmt, Stmt::Import(_, _, _)) {
                // Get the extra modules - see if any functions are marked global.
                // Without global functions, the extra modules never affect function resolution.
                if global
                    .scan_modules_raw()
                    .skip(_mods_len)
                    .any(|(_, m)| m.contains_indexed_global_functions())
                {
                    if state.fn_resolution_caches_len() > orig_fn_resolution_caches_len {
                        // When new module is imported with global functions and there is already
                        // a new cache, clear it - notice that this is expensive as all function
                        // resolutions must start again
                        state.fn_resolution_cache_mut().clear();
                    } else if restore_orig_state {
                        // When new module is imported with global functions, push a new cache
                        state.push_fn_resolution_cache();
                    } else {
                        // When the block is to be evaluated in-place, just clear the current cache
                        state.fn_resolution_cache_mut().clear();
                    }
                }
            }
        }

        // If imports list is modified, pop the functions lookup cache
        state.rewind_fn_resolution_caches(orig_fn_resolution_caches_len);

        if restore_orig_state {
            scope.rewind(orig_scope_len);
            state.scope_level -= 1;
            global.truncate_modules(orig_mods_len);

            // The impact of new local variables goes away at the end of a block
            // because any new variables introduced will go out of scope
            state.always_search_scope = orig_always_search_scope;
        }

        Ok(result)
    }

    /// Evaluate an op-assignment statement.
    /// [`Position`] in [`EvalAltResult`] is [`NONE`][Position::NONE] and should be set afterwards.
    pub(crate) fn eval_op_assignment(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        op_info: Option<OpAssignment>,
        op_pos: Position,
        target: &mut Target,
        root: (&str, Position),
        new_val: Dynamic,
    ) -> RhaiResultOf<()> {
        if target.is_read_only() {
            // Assignment to constant variable
            return Err(ERR::ErrorAssignmentToConstant(root.0.to_string(), root.1).into());
        }

        let mut new_val = new_val;

        if let Some(OpAssignment {
            hash_op_assign,
            hash_op,
            op,
        }) = op_info
        {
            let mut lock_guard;
            let lhs_ptr_inner;

            #[cfg(not(feature = "no_closure"))]
            let target_is_shared = target.is_shared();
            #[cfg(feature = "no_closure")]
            let target_is_shared = false;

            if target_is_shared {
                lock_guard = target.write_lock::<Dynamic>().expect("`Dynamic`");
                lhs_ptr_inner = &mut *lock_guard;
            } else {
                lhs_ptr_inner = &mut *target;
            }

            let hash = hash_op_assign;
            let args = &mut [lhs_ptr_inner, &mut new_val];

            match self.call_native_fn(global, state, lib, op, hash, args, true, true, op_pos) {
                Ok(_) => {
                    #[cfg(not(feature = "unchecked"))]
                    self.check_data_size(&mut args[0], root.1)?;
                }
                Err(err) if matches!(*err, ERR::ErrorFunctionNotFound(ref f, _) if f.starts_with(op)) =>
                {
                    // Expand to `var = var op rhs`
                    let op = &op[..op.len() - 1]; // extract operator without =

                    // Run function
                    let (value, _) = self.call_native_fn(
                        global, state, lib, op, hash_op, args, true, false, op_pos,
                    )?;

                    *args[0] = value.flatten();
                }
                Err(err) => return Err(err),
            }
        } else {
            // Normal assignment
            *target.as_mut() = new_val;
        }

        target.propagate_changed_value()
    }

    /// Evaluate a statement.
    ///
    /// # Safety
    ///
    /// This method uses some unsafe code, mainly for avoiding cloning of local variable names via
    /// direct lifetime casting.
    pub(crate) fn eval_stmt(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        stmt: &Stmt,
        rewind_scope: bool,
        level: usize,
    ) -> RhaiResult {
        // Coded this way for better branch prediction.
        // Popular branches are lifted out of the `match` statement into their own branches.

        // Function calls should account for a relatively larger portion of statements.
        if let Stmt::FnCall(x, pos) = stmt {
            #[cfg(not(feature = "unchecked"))]
            self.inc_operations(&mut global.num_operations, stmt.position())?;

            return self.eval_fn_call_expr(scope, global, state, lib, this_ptr, x, *pos, level);
        }

        // Then assignments.
        // We shouldn't do this for too many variants because, soon or later, the added comparisons
        // will cost more than the mis-predicted `match` branch.
        if let Stmt::Assignment(x, op_pos) = stmt {
            #[cfg(not(feature = "unchecked"))]
            self.inc_operations(&mut global.num_operations, stmt.position())?;

            return if x.0.is_variable_access(false) {
                let (lhs_expr, op_info, rhs_expr) = x.as_ref();
                let rhs_val = self
                    .eval_expr(scope, global, state, lib, this_ptr, rhs_expr, level)?
                    .flatten();
                let (mut lhs_ptr, pos) =
                    self.search_namespace(scope, global, state, lib, this_ptr, lhs_expr)?;

                let var_name = lhs_expr.get_variable_name(false).expect("`Expr::Variable`");

                if !lhs_ptr.is_ref() {
                    return Err(ERR::ErrorAssignmentToConstant(var_name.to_string(), pos).into());
                }

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, pos)?;

                self.eval_op_assignment(
                    global,
                    state,
                    lib,
                    *op_info,
                    *op_pos,
                    &mut lhs_ptr,
                    (var_name, pos),
                    rhs_val,
                )
                .map_err(|err| err.fill_position(rhs_expr.position()))?;

                Ok(Dynamic::UNIT)
            } else {
                let (lhs_expr, op_info, rhs_expr) = x.as_ref();
                let rhs_val = self
                    .eval_expr(scope, global, state, lib, this_ptr, rhs_expr, level)?
                    .flatten();
                let _new_val = Some(((rhs_val, rhs_expr.position()), (*op_info, *op_pos)));

                // Must be either `var[index] op= val` or `var.prop op= val`
                match lhs_expr {
                    // name op= rhs (handled above)
                    Expr::Variable(_, _, _) => {
                        unreachable!("Expr::Variable case is already handled")
                    }
                    // idx_lhs[idx_expr] op= rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(_, _, _) => {
                        self.eval_dot_index_chain(
                            scope, global, state, lib, this_ptr, lhs_expr, level, _new_val,
                        )?;
                        Ok(Dynamic::UNIT)
                    }
                    // dot_lhs.dot_rhs op= rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(_, _, _) => {
                        self.eval_dot_index_chain(
                            scope, global, state, lib, this_ptr, lhs_expr, level, _new_val,
                        )?;
                        Ok(Dynamic::UNIT)
                    }
                    _ => unreachable!("cannot assign to expression: {:?}", lhs_expr),
                }
            };
        }

        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, stmt.position())?;

        match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Dynamic::UNIT),

            // Expression as statement
            Stmt::Expr(expr) => Ok(self
                .eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                .flatten()),

            // Block scope
            Stmt::Block(statements, _) if statements.is_empty() => Ok(Dynamic::UNIT),
            Stmt::Block(statements, _) => {
                self.eval_stmt_block(scope, global, state, lib, this_ptr, statements, true, level)
            }

            // If statement
            Stmt::If(expr, x, _) => {
                let guard_val = self
                    .eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, expr.position()))?;

                if guard_val {
                    if !x.0.is_empty() {
                        self.eval_stmt_block(scope, global, state, lib, this_ptr, &x.0, true, level)
                    } else {
                        Ok(Dynamic::UNIT)
                    }
                } else {
                    if !x.1.is_empty() {
                        self.eval_stmt_block(scope, global, state, lib, this_ptr, &x.1, true, level)
                    } else {
                        Ok(Dynamic::UNIT)
                    }
                }
            }

            // Switch statement
            Stmt::Switch(match_expr, x, _) => {
                let (table, def_stmt, ranges) = x.as_ref();

                let value =
                    self.eval_expr(scope, global, state, lib, this_ptr, match_expr, level)?;

                let stmt_block = if value.is_hashable() {
                    let hasher = &mut get_hasher();
                    value.hash(hasher);
                    let hash = hasher.finish();

                    // First check hashes
                    if let Some(t) = table.get(&hash) {
                        if let Some(ref c) = t.0 {
                            if self
                                .eval_expr(scope, global, state, lib, this_ptr, &c, level)?
                                .as_bool()
                                .map_err(|typ| {
                                    self.make_type_mismatch_err::<bool>(typ, c.position())
                                })?
                            {
                                Some(&t.1)
                            } else {
                                None
                            }
                        } else {
                            Some(&t.1)
                        }
                    } else if value.is::<INT>() && !ranges.is_empty() {
                        // Then check integer ranges
                        let value = value.as_int().expect("`INT`");
                        let mut result = None;

                        for (_, _, _, condition, stmt_block) in
                            ranges.iter().filter(|&&(start, end, inclusive, _, _)| {
                                (!inclusive && (start..end).contains(&value))
                                    || (inclusive && (start..=end).contains(&value))
                            })
                        {
                            if let Some(c) = condition {
                                if !self
                                    .eval_expr(scope, global, state, lib, this_ptr, &c, level)?
                                    .as_bool()
                                    .map_err(|typ| {
                                        self.make_type_mismatch_err::<bool>(typ, c.position())
                                    })?
                                {
                                    continue;
                                }
                            }

                            result = Some(stmt_block);
                            break;
                        }

                        result
                    } else {
                        // Nothing matches
                        None
                    }
                } else {
                    // Non-hashable
                    None
                };

                if let Some(statements) = stmt_block {
                    if !statements.is_empty() {
                        self.eval_stmt_block(
                            scope, global, state, lib, this_ptr, statements, true, level,
                        )
                    } else {
                        Ok(Dynamic::UNIT)
                    }
                } else {
                    // Default match clause
                    if !def_stmt.is_empty() {
                        self.eval_stmt_block(
                            scope, global, state, lib, this_ptr, def_stmt, true, level,
                        )
                    } else {
                        Ok(Dynamic::UNIT)
                    }
                }
            }

            // Loop
            Stmt::While(Expr::Unit(_), body, _) => loop {
                if !body.is_empty() {
                    match self
                        .eval_stmt_block(scope, global, state, lib, this_ptr, body, true, level)
                    {
                        Ok(_) => (),
                        Err(err) => match *err {
                            ERR::LoopBreak(false, _) => (),
                            ERR::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                            _ => return Err(err),
                        },
                    }
                } else {
                    #[cfg(not(feature = "unchecked"))]
                    self.inc_operations(&mut global.num_operations, body.position())?;
                }
            },

            // While loop
            Stmt::While(expr, body, _) => loop {
                let condition = self
                    .eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, expr.position()))?;

                if !condition {
                    return Ok(Dynamic::UNIT);
                }
                if !body.is_empty() {
                    match self
                        .eval_stmt_block(scope, global, state, lib, this_ptr, body, true, level)
                    {
                        Ok(_) => (),
                        Err(err) => match *err {
                            ERR::LoopBreak(false, _) => (),
                            ERR::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                            _ => return Err(err),
                        },
                    }
                }
            },

            // Do loop
            Stmt::Do(body, expr, options, _) => loop {
                let is_while = !options.contains(AST_OPTION_NEGATED);

                if !body.is_empty() {
                    match self
                        .eval_stmt_block(scope, global, state, lib, this_ptr, body, true, level)
                    {
                        Ok(_) => (),
                        Err(err) => match *err {
                            ERR::LoopBreak(false, _) => continue,
                            ERR::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                            _ => return Err(err),
                        },
                    }
                }

                let condition = self
                    .eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, expr.position()))?;

                if condition ^ is_while {
                    return Ok(Dynamic::UNIT);
                }
            },

            // For loop
            Stmt::For(expr, x, _) => {
                let (Ident { name, .. }, counter, statements) = x.as_ref();
                let iter_obj = self
                    .eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                    .flatten();
                let iter_type = iter_obj.type_id();

                // lib should only contain scripts, so technically they cannot have iterators

                // Search order:
                // 1) Global namespace - functions registered via Engine::register_XXX
                // 2) Global modules - packages
                // 3) Imported modules - functions marked with global namespace
                // 4) Global sub-modules - functions marked with global namespace
                let func = self
                    .global_modules
                    .iter()
                    .find_map(|m| m.get_iter(iter_type))
                    .or_else(|| global.get_iter(iter_type))
                    .or_else(|| {
                        self.global_sub_modules
                            .values()
                            .find_map(|m| m.get_qualified_iter(iter_type))
                    });

                if let Some(func) = func {
                    // Add the loop variables
                    let orig_scope_len = scope.len();
                    let counter_index = if let Some(counter) = counter {
                        scope.push(unsafe_cast_var_name_to_lifetime(&counter.name), 0 as INT);
                        scope.len() - 1
                    } else {
                        usize::MAX
                    };
                    scope.push(unsafe_cast_var_name_to_lifetime(name), ());
                    let index = scope.len() - 1;

                    for (x, iter_value) in func(iter_obj).enumerate() {
                        // Increment counter
                        if counter_index < usize::MAX {
                            #[cfg(not(feature = "unchecked"))]
                            if x > INT::MAX as usize {
                                return Err(ERR::ErrorArithmetic(
                                    format!("for-loop counter overflow: {}", x),
                                    counter.as_ref().expect("`Some`").pos,
                                )
                                .into());
                            }

                            let index_value = (x as INT).into();

                            #[cfg(not(feature = "no_closure"))]
                            {
                                let index_var = scope.get_mut_by_index(counter_index);
                                if index_var.is_shared() {
                                    *index_var.write_lock().expect("`Dynamic`") = index_value;
                                } else {
                                    *index_var = index_value;
                                }
                            }
                            #[cfg(feature = "no_closure")]
                            {
                                *scope.get_mut_by_index(counter_index) = index_value;
                            }
                        }

                        let value = iter_value.flatten();

                        #[cfg(not(feature = "no_closure"))]
                        {
                            let loop_var = scope.get_mut_by_index(index);
                            if loop_var.is_shared() {
                                *loop_var.write_lock().expect("`Dynamic`") = value;
                            } else {
                                *loop_var = value;
                            }
                        }
                        #[cfg(feature = "no_closure")]
                        {
                            *scope.get_mut_by_index(index) = value;
                        }

                        #[cfg(not(feature = "unchecked"))]
                        self.inc_operations(&mut global.num_operations, statements.position())?;

                        if statements.is_empty() {
                            continue;
                        }

                        let result = self.eval_stmt_block(
                            scope, global, state, lib, this_ptr, statements, true, level,
                        );

                        match result {
                            Ok(_) => (),
                            Err(err) => match *err {
                                ERR::LoopBreak(false, _) => (),
                                ERR::LoopBreak(true, _) => break,
                                _ => return Err(err),
                            },
                        }
                    }

                    scope.rewind(orig_scope_len);
                    Ok(Dynamic::UNIT)
                } else {
                    Err(ERR::ErrorFor(expr.position()).into())
                }
            }

            // Continue/Break statement
            Stmt::BreakLoop(options, pos) => {
                Err(ERR::LoopBreak(options.contains(AST_OPTION_BREAK_OUT), *pos).into())
            }

            // Try/Catch statement
            Stmt::TryCatch(x, _) => {
                let (try_stmt, err_var, catch_stmt) = x.as_ref();

                let result = self
                    .eval_stmt_block(scope, global, state, lib, this_ptr, try_stmt, true, level)
                    .map(|_| Dynamic::UNIT);

                match result {
                    Ok(_) => result,
                    Err(err) if err.is_pseudo_error() => Err(err),
                    Err(err) if !err.is_catchable() => Err(err),
                    Err(mut err) => {
                        let err_value = match *err {
                            ERR::ErrorRuntime(ref x, _) => x.clone(),

                            #[cfg(feature = "no_object")]
                            _ => {
                                err.take_position();
                                err.to_string().into()
                            }
                            #[cfg(not(feature = "no_object"))]
                            _ => {
                                let mut err_map = crate::Map::new();
                                let err_pos = err.take_position();

                                err_map.insert("message".into(), err.to_string().into());

                                if !global.source.is_empty() {
                                    err_map.insert("source".into(), global.source.clone().into());
                                }

                                if err_pos.is_none() {
                                    // No position info
                                } else {
                                    let line = err_pos.line().unwrap() as INT;
                                    let position = if err_pos.is_beginning_of_line() {
                                        0
                                    } else {
                                        err_pos.position().unwrap()
                                    } as INT;
                                    err_map.insert("line".into(), line.into());
                                    err_map.insert("position".into(), position.into());
                                }

                                err.dump_fields(&mut err_map);
                                err_map.into()
                            }
                        };

                        let orig_scope_len = scope.len();

                        err_var.as_ref().map(|Ident { name, .. }| {
                            scope.push(unsafe_cast_var_name_to_lifetime(name), err_value)
                        });

                        let result = self.eval_stmt_block(
                            scope, global, state, lib, this_ptr, catch_stmt, true, level,
                        );

                        scope.rewind(orig_scope_len);

                        match result {
                            Ok(_) => Ok(Dynamic::UNIT),
                            Err(result_err) => match *result_err {
                                // Re-throw exception
                                ERR::ErrorRuntime(Dynamic(Union::Unit(_, _, _)), pos) => {
                                    err.set_position(pos);
                                    Err(err)
                                }
                                _ => Err(result_err),
                            },
                        }
                    }
                }
            }

            // Throw value
            Stmt::Return(options, Some(expr), pos) if options.contains(AST_OPTION_BREAK_OUT) => {
                Err(ERR::ErrorRuntime(
                    self.eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                        .flatten(),
                    *pos,
                )
                .into())
            }

            // Empty throw
            Stmt::Return(options, None, pos) if options.contains(AST_OPTION_BREAK_OUT) => {
                Err(ERR::ErrorRuntime(Dynamic::UNIT, *pos).into())
            }

            // Return value
            Stmt::Return(_, Some(expr), pos) => Err(ERR::Return(
                self.eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                    .flatten(),
                *pos,
            )
            .into()),

            // Empty return
            Stmt::Return(_, None, pos) => Err(ERR::Return(Dynamic::UNIT, *pos).into()),

            // Let/const statement
            Stmt::Var(expr, x, options, _) => {
                let name = &x.name;
                let entry_type = if options.contains(AST_OPTION_CONSTANT) {
                    AccessMode::ReadOnly
                } else {
                    AccessMode::ReadWrite
                };
                let export = options.contains(AST_OPTION_PUBLIC);

                let value = self
                    .eval_expr(scope, global, state, lib, this_ptr, expr, level)?
                    .flatten();

                let (var_name, _alias): (Cow<'_, str>, _) = if !rewind_scope {
                    #[cfg(not(feature = "no_function"))]
                    #[cfg(not(feature = "no_module"))]
                    if state.scope_level == 0
                        && entry_type == AccessMode::ReadOnly
                        && lib.iter().any(|&m| !m.is_empty())
                    {
                        // Add a global constant if at top level and there are functions
                        global.set_constant(name.clone(), value.clone());
                    }

                    (
                        name.to_string().into(),
                        if export { Some(name.clone()) } else { None },
                    )
                } else if export {
                    unreachable!("exported variable not on global level");
                } else {
                    (unsafe_cast_var_name_to_lifetime(name).into(), None)
                };

                scope.push_dynamic_value(var_name, entry_type, value);

                #[cfg(not(feature = "no_module"))]
                _alias.map(|alias| scope.add_entry_alias(scope.len() - 1, alias));

                Ok(Dynamic::UNIT)
            }

            // Import statement
            #[cfg(not(feature = "no_module"))]
            Stmt::Import(expr, export, _pos) => {
                // Guard against too many modules
                #[cfg(not(feature = "unchecked"))]
                if global.num_modules_loaded >= self.max_modules() {
                    return Err(ERR::ErrorTooManyModules(*_pos).into());
                }

                if let Some(path) = self
                    .eval_expr(scope, global, state, lib, this_ptr, &expr, level)?
                    .try_cast::<crate::ImmutableString>()
                {
                    use crate::ModuleResolver;

                    let source = match global.source.as_str() {
                        "" => None,
                        s => Some(s),
                    };
                    let path_pos = expr.position();

                    let module = global
                        .embedded_module_resolver
                        .as_ref()
                        .and_then(|r| match r.resolve(self, source, &path, path_pos) {
                            Err(err) if matches!(*err, ERR::ErrorModuleNotFound(_, _)) => None,
                            result => Some(result),
                        })
                        .or_else(|| {
                            self.module_resolver
                                .as_ref()
                                .map(|r| r.resolve(self, source, &path, path_pos))
                        })
                        .unwrap_or_else(|| {
                            Err(ERR::ErrorModuleNotFound(path.to_string(), path_pos).into())
                        })?;

                    if let Some(name) = export.as_ref().map(|x| x.name.clone()) {
                        if !module.is_indexed() {
                            // Index the module (making a clone copy if necessary) if it is not indexed
                            let mut module = crate::func::native::shared_take_or_clone(module);
                            module.build_index();
                            global.push_module(name, module);
                        } else {
                            global.push_module(name, module);
                        }
                    }

                    global.num_modules_loaded += 1;

                    Ok(Dynamic::UNIT)
                } else {
                    Err(self.make_type_mismatch_err::<crate::ImmutableString>("", expr.position()))
                }
            }

            // Export statement
            #[cfg(not(feature = "no_module"))]
            Stmt::Export(list, _) => {
                list.iter().try_for_each(
                    |(Ident { name, pos, .. }, Ident { name: rename, .. })| {
                        // Mark scope variables as public
                        if let Some((index, _)) = scope.get_index(name) {
                            scope.add_entry_alias(
                                index,
                                if rename.is_empty() { name } else { rename }.clone(),
                            );
                            Ok(()) as RhaiResultOf<_>
                        } else {
                            Err(ERR::ErrorVariableNotFound(name.to_string(), *pos).into())
                        }
                    },
                )?;
                Ok(Dynamic::UNIT)
            }

            // Share statement
            #[cfg(not(feature = "no_closure"))]
            Stmt::Share(name) => {
                if let Some((index, _)) = scope.get_index(name) {
                    let val = scope.get_mut_by_index(index);

                    if !val.is_shared() {
                        // Replace the variable with a shared value.
                        *val = std::mem::take(val).into_shared();
                    }
                }
                Ok(Dynamic::UNIT)
            }

            _ => unreachable!("statement cannot be evaluated: {:?}", stmt),
        }
    }
}
