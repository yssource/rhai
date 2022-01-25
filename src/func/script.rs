//! Implement script function-calling mechanism for [`Engine`].
#![cfg(not(feature = "no_function"))]

use super::call::FnCallArgs;
use crate::ast::ScriptFnDef;
use crate::eval::{EvalState, GlobalRuntimeState};
use crate::{Dynamic, Engine, Module, Position, RhaiError, RhaiResult, Scope, StaticVec, ERR};
use std::mem;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

impl Engine {
    /// Call a script-defined function.
    ///
    /// If `rewind_scope` is `false`, arguments are removed from the scope but new variables are not.
    ///
    /// # WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    ///
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_script_fn(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        fn_def: &ScriptFnDef,
        args: &mut FnCallArgs,
        pos: Position,
        rewind_scope: bool,
        level: usize,
    ) -> RhaiResult {
        #[inline(never)]
        fn make_error(
            name: String,
            fn_def: &ScriptFnDef,
            global: &GlobalRuntimeState,
            err: RhaiError,
            pos: Position,
        ) -> RhaiResult {
            Err(ERR::ErrorInFunctionCall(
                name,
                fn_def
                    .lib
                    .as_ref()
                    .and_then(|m| m.id().map(str::to_string))
                    .unwrap_or_else(|| global.source.to_string()),
                err,
                pos,
            )
            .into())
        }

        assert!(fn_def.params.len() == args.len());

        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, pos)?;

        if fn_def.body.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Check for stack overflow
        #[cfg(not(feature = "unchecked"))]
        if level > self.max_call_levels() {
            return Err(ERR::ErrorStackOverflow(pos).into());
        }

        let orig_scope_len = scope.len();
        let orig_imports_len = global.num_imports();

        #[cfg(feature = "debugging")]
        #[cfg(not(feature = "no_function"))]
        let orig_call_stack_len = global.debugger.call_stack_len();

        // Put arguments into scope as variables
        scope.extend(fn_def.params.iter().cloned().zip(args.into_iter().map(|v| {
            // Actually consume the arguments instead of cloning them
            mem::take(*v)
        })));

        // Push a new call stack frame
        #[cfg(feature = "debugging")]
        #[cfg(not(feature = "no_function"))]
        global.debugger.push_call_stack_frame(
            fn_def.name.clone(),
            scope
                .iter()
                .skip(orig_scope_len)
                .map(|(_, _, v)| v.clone())
                .collect(),
            global.source.clone(),
            pos,
        );

        // Merge in encapsulated environment, if any
        let mut lib_merged = StaticVec::with_capacity(lib.len() + 1);
        let orig_fn_resolution_caches_len = state.fn_resolution_caches_len();

        let lib = if let Some(ref fn_lib) = fn_def.lib {
            if fn_lib.is_empty() {
                lib
            } else {
                state.push_fn_resolution_cache();
                lib_merged.push(fn_lib.as_ref());
                lib_merged.extend(lib.iter().cloned());
                &lib_merged
            }
        } else {
            lib
        };

        #[cfg(not(feature = "no_module"))]
        if let Some(ref modules) = fn_def.global {
            modules
                .iter()
                .cloned()
                .for_each(|(n, m)| global.push_import(n, m));
        }

        // Evaluate the function
        let result = self
            .eval_stmt_block(
                scope,
                global,
                state,
                lib,
                this_ptr,
                &fn_def.body,
                rewind_scope,
                level,
            )
            .or_else(|err| match *err {
                // Convert return statement to return value
                ERR::Return(x, _) => Ok(x),
                // Error in sub function call
                ERR::ErrorInFunctionCall(name, src, err, _) => {
                    let fn_name = if src.is_empty() {
                        format!("{} < {}", name, fn_def.name)
                    } else {
                        format!("{} @ '{}' < {}", name, src, fn_def.name)
                    };

                    make_error(fn_name, fn_def, global, err, pos)
                }
                // System errors are passed straight-through
                mut err if err.is_system_exception() => {
                    err.set_position(pos);
                    Err(err.into())
                }
                // Other errors are wrapped in `ErrorInFunctionCall`
                _ => make_error(fn_def.name.to_string(), fn_def, global, err, pos),
            });

        // Remove all local variables and imported modules
        if rewind_scope {
            scope.rewind(orig_scope_len);
        } else if !args.is_empty() {
            // Remove arguments only, leaving new variables in the scope
            scope.remove_range(orig_scope_len, args.len())
        }
        global.truncate_imports(orig_imports_len);

        // Restore state
        state.rewind_fn_resolution_caches(orig_fn_resolution_caches_len);

        // Pop the call stack
        #[cfg(feature = "debugging")]
        #[cfg(not(feature = "no_function"))]
        global.debugger.rewind_call_stack(orig_call_stack_len);

        result
    }

    // Does a script-defined function exist?
    #[must_use]
    pub(crate) fn has_script_fn(
        &self,
        global: Option<&GlobalRuntimeState>,
        state: &mut EvalState,
        lib: &[&Module],
        hash_script: u64,
    ) -> bool {
        let cache = state.fn_resolution_cache_mut();

        if let Some(result) = cache.get(&hash_script).map(|v| v.is_some()) {
            return result;
        }

        // First check script-defined functions
        let result = lib.iter().any(|&m| m.contains_fn(hash_script))
            // Then check the global namespace and packages
            || self.global_modules.iter().any(|m| m.contains_fn(hash_script))
            // Then check imported modules
            || global.map_or(false, |m| m.contains_qualified_fn(hash_script))
            // Then check sub-modules
            || self.global_sub_modules.values().any(|m| m.contains_qualified_fn(hash_script));

        if !result {
            cache.insert(hash_script, None);
        }

        result
    }
}
