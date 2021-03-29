//! Implement function-calling mechanism for [`Engine`].

use crate::ast::FnCallHash;
use crate::engine::{
    FnResolutionCacheEntry, Imports, State, KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_FN_PTR,
    KEYWORD_FN_PTR_CALL, KEYWORD_FN_PTR_CURRY, KEYWORD_IS_DEF_VAR, KEYWORD_PRINT, KEYWORD_TYPE_OF,
    MAX_DYNAMIC_PARAMETERS,
};
use crate::fn_builtin::{get_builtin_binary_op_fn, get_builtin_op_assignment_fn};
use crate::fn_native::{FnAny, FnCallArgs};
use crate::module::NamespaceRef;
use crate::optimize::OptimizationLevel;
use crate::stdlib::{
    any::{type_name, TypeId},
    boxed::Box,
    convert::TryFrom,
    format,
    iter::{empty, once},
    mem,
    string::{String, ToString},
    vec::Vec,
};
use crate::{
    ast::{Expr, Stmt},
    fn_native::CallableFunction,
    RhaiResult,
};
use crate::{
    calc_fn_hash, calc_fn_params_hash, combine_hashes, Dynamic, Engine, EvalAltResult, FnPtr,
    ImmutableString, Module, ParseErrorType, Position, Scope, StaticVec,
};

#[cfg(not(feature = "no_object"))]
use crate::Map;

/// A type that temporarily stores a mutable reference to a `Dynamic`,
/// replacing it with a cloned copy.
#[derive(Debug, Default)]
struct ArgBackup<'a> {
    orig_mut: Option<&'a mut Dynamic>,
    value_copy: Dynamic,
}

impl<'a> ArgBackup<'a> {
    /// This function replaces the first argument of a method call with a clone copy.
    /// This is to prevent a pure function unintentionally consuming the first argument.
    ///
    /// `restore_first_arg` must be called before the end of the scope to prevent the shorter lifetime from leaking.
    ///
    /// # Safety
    ///
    /// This method blindly casts a reference to another lifetime, which saves allocation and string cloning.
    ///
    /// If `restore_first_arg` is called before the end of the scope, the shorter lifetime will not leak.
    #[inline(always)]
    fn change_first_arg_to_copy(&mut self, args: &mut FnCallArgs<'a>) {
        // Clone the original value.
        self.value_copy = args[0].clone();

        // Replace the first reference with a reference to the clone, force-casting the lifetime.
        // Must remember to restore it later with `restore_first_arg`.
        //
        // # Safety
        //
        // Blindly casting a reference to another lifetime saves allocation and string cloning,
        // but must be used with the utmost care.
        //
        // We can do this here because, before the end of this scope, we'd restore the original reference
        // via `restore_first_arg`. Therefore this shorter lifetime does not leak.
        self.orig_mut = Some(mem::replace(args.get_mut(0).unwrap(), unsafe {
            mem::transmute(&mut self.value_copy)
        }));
    }
    /// This function restores the first argument that was replaced by `change_first_arg_to_copy`.
    ///
    /// # Safety
    ///
    /// If `change_first_arg_to_copy` has been called, this function **MUST** be called _BEFORE_ exiting
    /// the current scope.  Otherwise it is undefined behavior as the shorter lifetime will leak.
    #[inline(always)]
    fn restore_first_arg(mut self, args: &mut FnCallArgs<'a>) {
        if let Some(this_pointer) = self.orig_mut.take() {
            args[0] = this_pointer;
        }
    }
}

impl Drop for ArgBackup<'_> {
    #[inline(always)]
    fn drop(&mut self) {
        // Panic if the shorter lifetime leaks.
        assert!(
            self.orig_mut.is_none(),
            "ArgBackup::restore_first_arg has not been called prior to existing this scope"
        );
    }
}

#[cfg(not(feature = "no_closure"))]
#[inline(always)]
pub fn ensure_no_data_race(
    fn_name: &str,
    args: &FnCallArgs,
    is_ref: bool,
) -> Result<(), Box<EvalAltResult>> {
    #[cfg(not(feature = "no_closure"))]
    if let Some((n, _)) = args
        .iter()
        .enumerate()
        .skip(if is_ref { 1 } else { 0 })
        .find(|(_, a)| a.is_locked())
    {
        return EvalAltResult::ErrorDataRace(
            format!("argument #{} of function '{}'", n + 1, fn_name),
            Position::NONE,
        )
        .into();
    }

    Ok(())
}

impl Engine {
    /// Generate the signature for a function call.
    #[inline]
    fn gen_call_signature(
        &self,
        namespace: Option<&NamespaceRef>,
        fn_name: &str,
        args: &[&mut Dynamic],
    ) -> String {
        format!(
            "{}{} ({})",
            namespace.map_or(String::new(), |ns| ns.to_string()),
            fn_name,
            args.iter()
                .map(|a| if a.is::<ImmutableString>() {
                    "&str | ImmutableString | String"
                } else {
                    self.map_type_name((*a).type_name())
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    /// Resolve a function call.
    ///
    /// Search order:
    /// 1) AST - script functions in the AST
    /// 2) Global namespace - functions registered via Engine::register_XXX
    /// 3) Global modules - packages
    /// 4) Imported modules - functions marked with global namespace
    /// 5) Global sub-modules - functions marked with global namespace
    #[inline(always)]
    fn resolve_function<'s>(
        &self,
        mods: &Imports,
        state: &'s mut State,
        lib: &[&Module],
        fn_name: &str,
        hash_script: u64,
        args: Option<&mut FnCallArgs>,
        allow_dynamic: bool,
        is_op_assignment: bool,
    ) -> &'s Option<FnResolutionCacheEntry> {
        let mut hash = if let Some(ref args) = args {
            let hash_params = calc_fn_params_hash(args.iter().map(|a| a.type_id()));
            combine_hashes(hash_script, hash_params)
        } else {
            hash_script
        };

        &*state
            .fn_resolution_cache_mut()
            .entry(hash)
            .or_insert_with(|| {
                let num_args = args.as_ref().map_or(0, |a| a.len());
                let max_bitmask = if !allow_dynamic {
                    0
                } else {
                    1usize << num_args.min(MAX_DYNAMIC_PARAMETERS)
                };
                let mut bitmask = 1usize; // Bitmask of which parameter to replace with `Dynamic`

                loop {
                    let func = lib
                        .iter()
                        .find_map(|m| {
                            m.get_fn(hash).cloned().map(|func| {
                                let source = m.id_raw().cloned();
                                FnResolutionCacheEntry { func, source }
                            })
                        })
                        .or_else(|| {
                            self.global_namespace
                                .get_fn(hash)
                                .cloned()
                                .map(|func| FnResolutionCacheEntry { func, source: None })
                        })
                        .or_else(|| {
                            self.global_modules.iter().find_map(|m| {
                                m.get_fn(hash).cloned().map(|func| {
                                    let source = m.id_raw().cloned();
                                    FnResolutionCacheEntry { func, source }
                                })
                            })
                        })
                        .or_else(|| {
                            mods.get_fn(hash).map(|(func, source)| {
                                let func = func.clone();
                                let source = source.cloned();
                                FnResolutionCacheEntry { func, source }
                            })
                        })
                        .or_else(|| {
                            self.global_sub_modules.values().find_map(|m| {
                                m.get_qualified_fn(hash).cloned().map(|func| {
                                    let source = m.id_raw().cloned();
                                    FnResolutionCacheEntry { func, source }
                                })
                            })
                        });

                    match func {
                        // Specific version found
                        Some(f) => return Some(f),

                        // Stop when all permutations are exhausted
                        None if bitmask >= max_bitmask => {
                            if num_args != 2 {
                                return None;
                            }

                            return args.and_then(|args| {
                                if !is_op_assignment {
                                    get_builtin_binary_op_fn(fn_name, &args[0], &args[1]).map(|f| {
                                        let func = CallableFunction::from_method(
                                            Box::new(f) as Box<FnAny>
                                        );
                                        FnResolutionCacheEntry { func, source: None }
                                    })
                                } else {
                                    let (first, second) = args.split_first().unwrap();

                                    get_builtin_op_assignment_fn(fn_name, *first, second[0]).map(
                                        |f| {
                                            let func = CallableFunction::from_method(
                                                Box::new(f) as Box<FnAny>
                                            );
                                            FnResolutionCacheEntry { func, source: None }
                                        },
                                    )
                                }
                            });
                        }

                        // Try all permutations with `Dynamic` wildcards
                        None => {
                            let hash_params = calc_fn_params_hash(
                                args.as_ref().unwrap().iter().enumerate().map(|(i, a)| {
                                    let mask = 1usize << (num_args - i - 1);
                                    if bitmask & mask != 0 {
                                        // Replace with `Dynamic`
                                        TypeId::of::<Dynamic>()
                                    } else {
                                        a.type_id()
                                    }
                                }),
                            );
                            hash = combine_hashes(hash_script, hash_params);

                            bitmask += 1;
                        }
                    }
                }
            })
    }

    /// Call a native Rust function registered with the [`Engine`].
    ///
    /// # WARNING
    ///
    /// Function call arguments be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_native_fn(
        &self,
        mods: &Imports,
        state: &mut State,
        lib: &[&Module],
        fn_name: &str,
        hash_native: u64,
        args: &mut FnCallArgs,
        is_ref: bool,
        is_op_assignment: bool,
        pos: Position,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        self.inc_operations(state, pos)?;

        let state_source = state.source.clone();

        // Check if function access already in the cache
        let func = self.resolve_function(
            mods,
            state,
            lib,
            fn_name,
            hash_native,
            Some(args),
            true,
            is_op_assignment,
        );

        if let Some(FnResolutionCacheEntry { func, source }) = func {
            assert!(func.is_native());

            // Calling pure function but the first argument is a reference?
            let mut backup: Option<ArgBackup> = None;
            if is_ref && func.is_pure() && !args.is_empty() {
                backup = Some(Default::default());
                backup.as_mut().unwrap().change_first_arg_to_copy(args);
            }

            // Run external function
            let source = source
                .as_ref()
                .or_else(|| state_source.as_ref())
                .map(|s| s.as_str());
            let result = if func.is_plugin_fn() {
                func.get_plugin_fn()
                    .call((self, fn_name, source, mods, lib).into(), args)
            } else {
                func.get_native_fn()((self, fn_name, source, mods, lib).into(), args)
            };

            // Restore the original reference
            if let Some(backup) = backup {
                backup.restore_first_arg(args);
            }

            let result = result.map_err(|err| err.fill_position(pos))?;

            // See if the function match print/debug (which requires special processing)
            return Ok(match fn_name {
                KEYWORD_PRINT => {
                    let text = result.take_immutable_string().map_err(|typ| {
                        EvalAltResult::ErrorMismatchOutputType(
                            self.map_type_name(type_name::<ImmutableString>()).into(),
                            typ.into(),
                            pos,
                        )
                    })?;
                    ((self.print)(&text).into(), false)
                }
                KEYWORD_DEBUG => {
                    let text = result.take_immutable_string().map_err(|typ| {
                        EvalAltResult::ErrorMismatchOutputType(
                            self.map_type_name(type_name::<ImmutableString>()).into(),
                            typ.into(),
                            pos,
                        )
                    })?;
                    let source = state.source.as_ref().map(|s| s.as_str());
                    ((self.debug)(&text, source, pos).into(), false)
                }
                _ => (result, func.is_method()),
            });
        }

        match fn_name {
            // index getter function not found?
            #[cfg(not(feature = "no_index"))]
            crate::engine::FN_IDX_GET => {
                assert!(args.len() == 2);

                EvalAltResult::ErrorFunctionNotFound(
                    format!(
                        "{} [{}]",
                        self.map_type_name(args[0].type_name()),
                        self.map_type_name(args[1].type_name()),
                    ),
                    pos,
                )
                .into()
            }

            // index setter function not found?
            #[cfg(not(feature = "no_index"))]
            crate::engine::FN_IDX_SET => {
                assert!(args.len() == 3);

                EvalAltResult::ErrorFunctionNotFound(
                    format!(
                        "{} [{}]=",
                        self.map_type_name(args[0].type_name()),
                        self.map_type_name(args[1].type_name()),
                    ),
                    pos,
                )
                .into()
            }

            // Getter function not found?
            #[cfg(not(feature = "no_object"))]
            _ if fn_name.starts_with(crate::engine::FN_GET) => {
                assert!(args.len() == 1);

                EvalAltResult::ErrorDotExpr(
                    format!(
                        "Unknown property '{}' - a getter is not registered for type '{}'",
                        &fn_name[crate::engine::FN_GET.len()..],
                        self.map_type_name(args[0].type_name())
                    ),
                    pos,
                )
                .into()
            }

            // Setter function not found?
            #[cfg(not(feature = "no_object"))]
            _ if fn_name.starts_with(crate::engine::FN_SET) => {
                assert!(args.len() == 2);

                EvalAltResult::ErrorDotExpr(
                    format!(
                        "No writable property '{}' - a setter is not registered for type '{}' to handle '{}'",
                        &fn_name[crate::engine::FN_SET.len()..],
                        self.map_type_name(args[0].type_name()),
                        self.map_type_name(args[1].type_name()),
                    ),
                    pos,
                )
                .into()
            }

            // Raise error
            _ => EvalAltResult::ErrorFunctionNotFound(
                self.gen_call_signature(None, fn_name, args.as_ref()),
                pos,
            )
            .into(),
        }
    }

    /// Call a script-defined function.
    ///
    /// # WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    #[cfg(not(feature = "no_function"))]
    pub(crate) fn call_script_fn(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        fn_def: &crate::ast::ScriptFnDef,
        args: &mut FnCallArgs,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        #[inline(always)]
        fn make_error(
            name: crate::stdlib::string::String,
            fn_def: &crate::ast::ScriptFnDef,
            state: &State,
            err: Box<EvalAltResult>,
            pos: Position,
        ) -> RhaiResult {
            EvalAltResult::ErrorInFunctionCall(
                name,
                fn_def
                    .lib
                    .as_ref()
                    .and_then(|m| m.id().map(|id| id.to_string()))
                    .or_else(|| state.source.as_ref().map(|s| s.to_string()))
                    .unwrap_or_default(),
                err,
                pos,
            )
            .into()
        }

        self.inc_operations(state, pos)?;

        if fn_def.body.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Check for stack overflow
        #[cfg(not(feature = "no_function"))]
        #[cfg(not(feature = "unchecked"))]
        if level > self.max_call_levels() {
            return EvalAltResult::ErrorStackOverflow(pos).into();
        }

        let orig_scope_level = state.scope_level;
        state.scope_level += 1;

        let prev_scope_len = scope.len();
        let prev_mods_len = mods.len();

        // Put arguments into scope as variables
        // Actually consume the arguments instead of cloning them
        scope.extend(
            fn_def
                .params
                .iter()
                .zip(args.iter_mut().map(|v| mem::take(*v)))
                .map(|(name, value)| {
                    let var_name: crate::stdlib::borrow::Cow<'_, str> =
                        crate::r#unsafe::unsafe_cast_var_name_to_lifetime(name).into();
                    (var_name, value)
                }),
        );

        // Merge in encapsulated environment, if any
        let lib_merged;

        let (unified_lib, unified) = if let Some(ref env_lib) = fn_def.lib {
            state.push_fn_resolution_cache();
            lib_merged = once(env_lib.as_ref())
                .chain(lib.iter().cloned())
                .collect::<StaticVec<_>>();
            (lib_merged.as_ref(), true)
        } else {
            (lib, false)
        };

        #[cfg(not(feature = "no_module"))]
        if !fn_def.mods.is_empty() {
            fn_def
                .mods
                .iter_raw()
                .for_each(|(n, m)| mods.push(n.clone(), m.clone()));
        }

        // Evaluate the function
        let body = &fn_def.body.statements;

        let result = self
            .eval_stmt_block(scope, mods, state, unified_lib, this_ptr, body, true, level)
            .or_else(|err| match *err {
                // Convert return statement to return value
                EvalAltResult::Return(x, _) => Ok(x),
                // Error in sub function call
                EvalAltResult::ErrorInFunctionCall(name, src, err, _) => {
                    let fn_name = if src.is_empty() {
                        format!("{} < {}", name, fn_def.name)
                    } else {
                        format!("{} @ '{}' < {}", name, src, fn_def.name)
                    };

                    make_error(fn_name, fn_def, state, err, pos)
                }
                // System errors are passed straight-through
                mut err if err.is_system_exception() => {
                    err.set_position(pos);
                    err.into()
                }
                // Other errors are wrapped in `ErrorInFunctionCall`
                _ => make_error(fn_def.name.to_string(), fn_def, state, err, pos),
            });

        // Remove all local variables
        scope.rewind(prev_scope_len);
        mods.truncate(prev_mods_len);
        state.scope_level = orig_scope_level;

        if unified {
            state.pop_fn_resolution_cache();
        }

        result
    }

    // Does a scripted function exist?
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub(crate) fn has_script_fn(
        &self,
        mods: Option<&Imports>,
        state: &mut State,
        lib: &[&Module],
        hash_script: u64,
    ) -> bool {
        let cache = state.fn_resolution_cache_mut();

        if let Some(result) = cache.get(&hash_script).map(|v| v.is_some()) {
            return result;
        }

        // First check script-defined functions
        let result = lib.iter().any(|&m| m.contains_fn(hash_script))
            // Then check registered functions
            || self.global_namespace.contains_fn(hash_script)
            // Then check packages
            || self.global_modules.iter().any(|m| m.contains_fn(hash_script))
            // Then check imported modules
            || mods.map_or(false, |m| m.contains_fn(hash_script))
            // Then check sub-modules
            || self.global_sub_modules.values().any(|m| m.contains_qualified_fn(hash_script));

        if !result {
            cache.insert(hash_script, None);
        }

        result
    }

    /// Perform an actual function call, native Rust or scripted, taking care of special functions.
    ///
    /// # WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn exec_fn_call(
        &self,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        fn_name: &str,
        hash: FnCallHash,
        args: &mut FnCallArgs,
        is_ref: bool,
        _is_method: bool,
        pos: Position,
        _capture_scope: Option<Scope>,
        _level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        // Check for data race.
        #[cfg(not(feature = "no_closure"))]
        ensure_no_data_race(fn_name, args, is_ref)?;

        // These may be redirected from method style calls.
        match fn_name {
            // Handle type_of()
            KEYWORD_TYPE_OF if args.len() == 1 => {
                return Ok((
                    self.map_type_name(args[0].type_name()).to_string().into(),
                    false,
                ));
            }

            // Handle is_def_fn()
            #[cfg(not(feature = "no_function"))]
            crate::engine::KEYWORD_IS_DEF_FN
                if args.len() == 2 && args[0].is::<FnPtr>() && args[1].is::<crate::INT>() =>
            {
                let fn_name = &*args[0].read_lock::<ImmutableString>().unwrap();
                let num_params = args[1].as_int().unwrap();

                return Ok((
                    if num_params < 0 {
                        Dynamic::FALSE
                    } else {
                        let hash_script = calc_fn_hash(empty(), fn_name, num_params as usize);
                        self.has_script_fn(Some(mods), state, lib, hash_script)
                            .into()
                    },
                    false,
                ));
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if args.len() == 1 => {
                return EvalAltResult::ErrorRuntime(
                    format!(
                        "'{}' should not be called this way. Try {}(...);",
                        fn_name, fn_name
                    )
                    .into(),
                    pos,
                )
                .into()
            }

            KEYWORD_FN_PTR | KEYWORD_EVAL | KEYWORD_IS_DEF_VAR if args.len() == 1 => {
                return EvalAltResult::ErrorRuntime(
                    format!(
                        "'{}' should not be called this way. Try {}(...);",
                        fn_name, fn_name
                    )
                    .into(),
                    pos,
                )
                .into()
            }

            KEYWORD_FN_PTR_CALL | KEYWORD_FN_PTR_CURRY if !args.is_empty() => {
                return EvalAltResult::ErrorRuntime(
                    format!(
                        "'{}' should not be called this way. Try {}(...);",
                        fn_name, fn_name
                    )
                    .into(),
                    pos,
                )
                .into()
            }

            _ => (),
        }

        // Scripted function call?
        #[cfg(not(feature = "no_function"))]
        let hash_script = if hash.is_native_only() {
            None
        } else {
            Some(hash.script_hash())
        };

        #[cfg(not(feature = "no_function"))]
        if let Some(FnResolutionCacheEntry { func, source }) = hash_script.and_then(|hash| {
            self.resolve_function(mods, state, lib, fn_name, hash, None, false, false)
                .clone()
        }) {
            // Script function call
            assert!(func.is_script());

            let func = func.get_fn_def();

            if func.body.is_empty() {
                return Ok((Dynamic::UNIT, false));
            }

            let scope: &mut Scope = &mut Default::default();

            // Move captured variables into scope
            #[cfg(not(feature = "no_closure"))]
            if let Some(captured) = _capture_scope {
                if !func.externals.is_empty() {
                    captured
                        .into_iter()
                        .filter(|(name, _, _)| func.externals.contains(name.as_ref()))
                        .for_each(|(name, value, _)| {
                            // Consume the scope values.
                            scope.push_dynamic(name, value);
                        });
                }
            }

            let result = if _is_method {
                // Method call of script function - map first argument to `this`
                let (first, rest) = args.split_first_mut().unwrap();

                let orig_source = mem::take(&mut state.source);
                state.source = source;

                let level = _level + 1;

                let result = self.call_script_fn(
                    scope,
                    mods,
                    state,
                    lib,
                    &mut Some(*first),
                    func,
                    rest,
                    pos,
                    level,
                );

                // Restore the original source
                state.source = orig_source;

                result?
            } else {
                // Normal call of script function
                // The first argument is a reference?
                let mut backup: Option<ArgBackup> = None;
                if is_ref && !args.is_empty() {
                    backup = Some(Default::default());
                    backup.as_mut().unwrap().change_first_arg_to_copy(args);
                }

                let orig_source = mem::take(&mut state.source);
                state.source = source;

                let level = _level + 1;

                let result =
                    self.call_script_fn(scope, mods, state, lib, &mut None, func, args, pos, level);

                // Restore the original source
                state.source = orig_source;

                // Restore the original reference
                if let Some(backup) = backup {
                    backup.restore_first_arg(args);
                }

                result?
            };

            return Ok((result, false));
        }

        // Native function call
        self.call_native_fn(
            mods,
            state,
            lib,
            fn_name,
            hash.native_hash(),
            args,
            is_ref,
            false,
            pos,
        )
    }

    /// Evaluate a list of statements with no `this` pointer.
    /// This is commonly used to evaluate a list of statements in an [`AST`] or a script function body.
    #[inline(always)]
    pub(crate) fn eval_global_statements(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        statements: &[Stmt],
        lib: &[&Module],
        level: usize,
    ) -> RhaiResult {
        self.eval_stmt_block(scope, mods, state, lib, &mut None, statements, false, level)
            .or_else(|err| match *err {
                EvalAltResult::Return(out, _) => Ok(out),
                EvalAltResult::LoopBreak(_, _) => {
                    unreachable!("no outer loop scope to break out of")
                }
                _ => Err(err),
            })
    }

    /// Evaluate a text script in place - used primarily for 'eval'.
    fn eval_script_expr_in_place(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        script: &str,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        self.inc_operations(state, pos)?;

        let script = script.trim();
        if script.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Compile the script text
        // No optimizations because we only run it once
        let ast = self.compile_with_scope_and_optimization_level(
            &Default::default(),
            &[script],
            OptimizationLevel::None,
        )?;

        // If new functions are defined within the eval string, it is an error
        if ast.lib().count().0 != 0 {
            return Err(ParseErrorType::FnWrongDefinition.into());
        }

        // Evaluate the AST
        let mut new_state: State = Default::default();
        new_state.source = state.source.clone();
        new_state.operations = state.operations;

        let result =
            self.eval_global_statements(scope, mods, &mut new_state, ast.statements(), lib, level);

        state.operations = new_state.operations;

        result
    }

    /// Call a dot method.
    #[cfg(not(feature = "no_object"))]
    pub(crate) fn make_method_call(
        &self,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        fn_name: &str,
        mut hash: FnCallHash,
        target: &mut crate::engine::Target,
        (call_args, call_arg_positions): &mut (StaticVec<Dynamic>, StaticVec<Position>),
        pos: Position,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        let is_ref = target.is_ref();

        // Get a reference to the mutation target Dynamic
        let obj = target.as_mut();
        let mut fn_name = fn_name;

        let (result, updated) = match fn_name {
            KEYWORD_FN_PTR_CALL if obj.is::<FnPtr>() => {
                // FnPtr call
                let fn_ptr = obj.read_lock::<FnPtr>().unwrap();
                // Redirect function name
                let fn_name = fn_ptr.fn_name();
                let args_len = call_args.len() + fn_ptr.curry().len();
                // Recalculate hashes
                let new_hash = FnCallHash::from_script(calc_fn_hash(empty(), fn_name, args_len));
                // Arguments are passed as-is, adding the curried arguments
                let mut curry = fn_ptr.curry().iter().cloned().collect::<StaticVec<_>>();
                let mut arg_values = curry
                    .iter_mut()
                    .chain(call_args.iter_mut())
                    .collect::<StaticVec<_>>();
                let args = arg_values.as_mut();

                // Map it to name(args) in function-call style
                self.exec_fn_call(
                    mods, state, lib, fn_name, new_hash, args, false, false, pos, None, level,
                )
            }
            KEYWORD_FN_PTR_CALL => {
                if call_args.len() > 0 {
                    if !call_args[0].is::<FnPtr>() {
                        return Err(self.make_type_mismatch_err::<FnPtr>(
                            self.map_type_name(obj.type_name()),
                            call_arg_positions[0],
                        ));
                    }
                } else {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(obj.type_name()),
                        pos,
                    ));
                }

                // FnPtr call on object
                let fn_ptr = call_args.remove(0).cast::<FnPtr>();
                call_arg_positions.remove(0);
                // Redirect function name
                let fn_name = fn_ptr.fn_name();
                let args_len = call_args.len() + fn_ptr.curry().len();
                // Recalculate hash
                let new_hash = FnCallHash::from_script_and_native(
                    calc_fn_hash(empty(), fn_name, args_len),
                    calc_fn_hash(empty(), fn_name, args_len + 1),
                );
                // Replace the first argument with the object pointer, adding the curried arguments
                let mut curry = fn_ptr.curry().iter().cloned().collect::<StaticVec<_>>();
                let mut arg_values = once(obj)
                    .chain(curry.iter_mut())
                    .chain(call_args.iter_mut())
                    .collect::<StaticVec<_>>();
                let args = arg_values.as_mut();

                // Map it to name(args) in function-call style
                self.exec_fn_call(
                    mods, state, lib, fn_name, new_hash, args, is_ref, true, pos, None, level,
                )
            }
            KEYWORD_FN_PTR_CURRY => {
                if !obj.is::<FnPtr>() {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(obj.type_name()),
                        pos,
                    ));
                }

                let fn_ptr = obj.read_lock::<FnPtr>().unwrap();

                // Curry call
                Ok((
                    if call_args.is_empty() {
                        fn_ptr.clone()
                    } else {
                        FnPtr::new_unchecked(
                            fn_ptr.get_fn_name().clone(),
                            fn_ptr
                                .curry()
                                .iter()
                                .cloned()
                                .chain(call_args.iter_mut().map(|v| mem::take(v)))
                                .collect(),
                        )
                    }
                    .into(),
                    false,
                ))
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if call_args.is_empty() => {
                return Ok((target.is_shared().into(), false));
            }

            _ => {
                let _redirected;

                // Check if it is a map method call in OOP style
                #[cfg(not(feature = "no_object"))]
                if let Some(map) = obj.read_lock::<Map>() {
                    if let Some(val) = map.get(fn_name) {
                        if let Some(fn_ptr) = val.read_lock::<FnPtr>() {
                            // Remap the function name
                            _redirected = fn_ptr.get_fn_name().clone();
                            fn_name = &_redirected;
                            // Add curried arguments
                            fn_ptr
                                .curry()
                                .iter()
                                .cloned()
                                .enumerate()
                                .for_each(|(i, v)| {
                                    call_args.insert(i, v);
                                    call_arg_positions.insert(i, Position::NONE);
                                });
                            // Recalculate the hash based on the new function name and new arguments
                            hash = FnCallHash::from_script_and_native(
                                calc_fn_hash(empty(), fn_name, call_args.len()),
                                calc_fn_hash(empty(), fn_name, call_args.len() + 1),
                            );
                        }
                    }
                };

                // Attached object pointer in front of the arguments
                let mut arg_values = once(obj)
                    .chain(call_args.iter_mut())
                    .collect::<StaticVec<_>>();
                let args = arg_values.as_mut();

                self.exec_fn_call(
                    mods, state, lib, fn_name, hash, args, is_ref, true, pos, None, level,
                )
            }
        }?;

        // Propagate the changed value back to the source if necessary
        if updated {
            target.propagate_changed_value();
        }

        Ok((result, updated))
    }

    /// Call a function in normal function-call style.
    pub(crate) fn make_function_call(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        fn_name: &str,
        args_expr: &[Expr],
        constant_args: &[(Dynamic, Position)],
        mut hash: FnCallHash,
        pos: Position,
        capture_scope: bool,
        level: usize,
    ) -> RhaiResult {
        // Handle call() - Redirect function call
        let redirected;
        let mut args_expr = args_expr;
        let mut constant_args = constant_args;
        let mut total_args = args_expr.len() + constant_args.len();
        let mut curry = StaticVec::new();
        let mut name = fn_name;

        match name {
            // Handle call()
            KEYWORD_FN_PTR_CALL if total_args >= 1 => {
                let (arg, arg_pos) = args_expr.get(0).map_or_else(
                    || Ok(constant_args[0].clone()),
                    |arg| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, arg, level)
                            .map(|v| (v, arg.position()))
                    },
                )?;

                if !arg.is::<FnPtr>() {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(arg.type_name()),
                        arg_pos,
                    ));
                }

                let fn_ptr = arg.cast::<FnPtr>();
                curry.extend(fn_ptr.curry().iter().cloned());

                // Redirect function name
                redirected = fn_ptr.take_data().0;
                name = &redirected;

                // Skip the first argument
                if !args_expr.is_empty() {
                    args_expr = &args_expr[1..];
                } else {
                    constant_args = &constant_args[1..];
                }
                total_args -= 1;

                // Recalculate hash
                let args_len = total_args + curry.len();
                hash = if !hash.is_native_only() {
                    FnCallHash::from_script(calc_fn_hash(empty(), name, args_len))
                } else {
                    FnCallHash::from_native(calc_fn_hash(empty(), name, args_len))
                };
            }
            // Handle Fn()
            KEYWORD_FN_PTR if total_args == 1 => {
                let (arg, arg_pos) = args_expr.get(0).map_or_else(
                    || Ok(constant_args[0].clone()),
                    |arg| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, arg, level)
                            .map(|v| (v, arg.position()))
                    },
                )?;

                // Fn - only in function call style
                return arg
                    .take_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))
                    .and_then(|s| FnPtr::try_from(s))
                    .map(Into::<Dynamic>::into)
                    .map_err(|err| err.fill_position(arg_pos));
            }

            // Handle curry()
            KEYWORD_FN_PTR_CURRY if total_args > 1 => {
                let (arg, arg_pos) = args_expr.get(0).map_or_else(
                    || Ok(constant_args[0].clone()),
                    |arg| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, arg, level)
                            .map(|v| (v, arg.position()))
                    },
                )?;

                if !arg.is::<FnPtr>() {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(arg.type_name()),
                        arg_pos,
                    ));
                }

                let (name, mut fn_curry) = arg.cast::<FnPtr>().take_data();

                // Append the new curried arguments to the existing list.
                if !args_expr.is_empty() {
                    args_expr.iter().skip(1).try_for_each(|expr| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                            .map(|value| fn_curry.push(value))
                    })?;
                    fn_curry.extend(constant_args.iter().map(|(v, _)| v.clone()));
                } else {
                    fn_curry.extend(constant_args.iter().skip(1).map(|(v, _)| v.clone()));
                }

                return Ok(FnPtr::new_unchecked(name, fn_curry).into());
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if total_args == 1 => {
                let arg = args_expr.get(0).map_or_else(
                    || Ok(constant_args[0].0.clone()),
                    |arg| self.eval_expr(scope, mods, state, lib, this_ptr, arg, level),
                )?;
                return Ok(arg.is_shared().into());
            }

            // Handle is_def_fn()
            #[cfg(not(feature = "no_function"))]
            crate::engine::KEYWORD_IS_DEF_FN if total_args == 2 => {
                let (arg, arg_pos) = if !args_expr.is_empty() {
                    (
                        self.eval_expr(scope, mods, state, lib, this_ptr, &args_expr[0], level)?,
                        args_expr[0].position(),
                    )
                } else {
                    constant_args[0].clone()
                };

                let fn_name = arg
                    .take_immutable_string()
                    .map_err(|err| self.make_type_mismatch_err::<ImmutableString>(err, arg_pos))?;

                let (arg, arg_pos) = if args_expr.len() > 1 {
                    (
                        self.eval_expr(scope, mods, state, lib, this_ptr, &args_expr[1], level)?,
                        args_expr[1].position(),
                    )
                } else {
                    constant_args[if args_expr.is_empty() { 1 } else { 0 }].clone()
                };

                let num_params = arg
                    .as_int()
                    .map_err(|err| self.make_type_mismatch_err::<crate::INT>(err, arg_pos))?;

                return Ok(if num_params < 0 {
                    Dynamic::FALSE
                } else {
                    let hash_script = calc_fn_hash(empty(), &fn_name, num_params as usize);
                    self.has_script_fn(Some(mods), state, lib, hash_script)
                        .into()
                });
            }

            // Handle is_def_var()
            KEYWORD_IS_DEF_VAR if total_args == 1 => {
                let (arg, arg_pos) = args_expr.get(0).map_or_else(
                    || Ok(constant_args[0].clone()),
                    |arg| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, arg, level)
                            .map(|v| (v, arg.position()))
                    },
                )?;
                let var_name = arg
                    .take_immutable_string()
                    .map_err(|err| self.make_type_mismatch_err::<ImmutableString>(err, arg_pos))?;
                return Ok(scope.contains(&var_name).into());
            }

            // Handle eval()
            KEYWORD_EVAL if total_args == 1 => {
                // eval - only in function call style
                let prev_len = scope.len();
                let (script, script_pos) = args_expr.get(0).map_or_else(
                    || Ok(constant_args[0].clone()),
                    |script_expr| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, script_expr, level)
                            .map(|v| (v, script_expr.position()))
                    },
                )?;
                let script = script.take_immutable_string().map_err(|typ| {
                    self.make_type_mismatch_err::<ImmutableString>(typ, script_pos)
                })?;
                let result = self.eval_script_expr_in_place(
                    scope,
                    mods,
                    state,
                    lib,
                    &script,
                    script_pos,
                    level + 1,
                );

                // IMPORTANT! If the eval defines new variables in the current scope,
                //            all variable offsets from this point on will be mis-aligned.
                if scope.len() != prev_len {
                    state.always_search = true;
                }

                return result.map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        KEYWORD_EVAL.to_string(),
                        state
                            .source
                            .as_ref()
                            .map(|s| s.to_string())
                            .unwrap_or_default(),
                        err,
                        pos,
                    ))
                });
            }

            _ => (),
        }

        // Normal function call - except for Fn, curry, call and eval (handled above)
        let mut arg_values: StaticVec<_>;
        let mut args: StaticVec<_>;
        let mut is_ref = false;
        let capture = if capture_scope && !scope.is_empty() {
            Some(scope.clone_visible())
        } else {
            None
        };

        if args_expr.is_empty() && constant_args.is_empty() && curry.is_empty() {
            // No arguments
            args = Default::default();
        } else {
            // If the first argument is a variable, and there is no curried arguments,
            // convert to method-call style in order to leverage potential &mut first argument and
            // avoid cloning the value
            if curry.is_empty()
                && !args_expr.is_empty()
                && args_expr[0].get_variable_access(false).is_some()
            {
                // func(x, ...) -> x.func(...)
                arg_values = args_expr
                    .iter()
                    .skip(1)
                    .map(|expr| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                            .map(Dynamic::flatten)
                    })
                    .chain(constant_args.iter().map(|(v, _)| Ok(v.clone())))
                    .collect::<Result<_, _>>()?;

                let (mut target, pos) =
                    self.search_namespace(scope, mods, state, lib, this_ptr, &args_expr[0])?;

                if target.as_ref().is_read_only() {
                    target = target.into_owned();
                }

                self.inc_operations(state, pos)?;

                args = if target.is_shared() || target.is_value() {
                    arg_values.insert(0, target.take_or_clone().flatten());
                    arg_values.iter_mut().collect()
                } else {
                    // Turn it into a method call only if the object is not shared and not a simple value
                    is_ref = true;
                    once(target.take_ref().unwrap())
                        .chain(arg_values.iter_mut())
                        .collect()
                };
            } else {
                // func(..., ...)
                arg_values = args_expr
                    .iter()
                    .map(|expr| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                            .map(Dynamic::flatten)
                    })
                    .chain(constant_args.iter().map(|(v, _)| Ok(v.clone())))
                    .collect::<Result<_, _>>()?;

                args = curry.iter_mut().chain(arg_values.iter_mut()).collect();
            }
        }

        let args = args.as_mut();

        self.exec_fn_call(
            mods, state, lib, name, hash, args, is_ref, false, pos, capture, level,
        )
        .map(|(v, _)| v)
    }

    /// Call a namespace-qualified function in normal function-call style.
    pub(crate) fn make_qualified_function_call(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        namespace: Option<&NamespaceRef>,
        fn_name: &str,
        args_expr: &[Expr],
        constant_args: &[(Dynamic, Position)],
        hash: u64,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        let namespace = namespace.unwrap();
        let mut arg_values: StaticVec<_>;
        let mut first_arg_value = None;
        let mut args: StaticVec<_>;

        if args_expr.is_empty() && constant_args.is_empty() {
            // No arguments
            args = Default::default();
        } else {
            // See if the first argument is a variable (not namespace-qualified).
            // If so, convert to method-call style in order to leverage potential
            // &mut first argument and avoid cloning the value
            if !args_expr.is_empty() && args_expr[0].get_variable_access(true).is_some() {
                // func(x, ...) -> x.func(...)
                arg_values = args_expr
                    .iter()
                    .enumerate()
                    .map(|(i, expr)| {
                        // Skip the first argument
                        if i == 0 {
                            Ok(Default::default())
                        } else {
                            self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                                .map(Dynamic::flatten)
                        }
                    })
                    .chain(constant_args.iter().map(|(v, _)| Ok(v.clone())))
                    .collect::<Result<_, _>>()?;

                // Get target reference to first argument
                let (target, pos) =
                    self.search_scope_only(scope, mods, state, lib, this_ptr, &args_expr[0])?;

                self.inc_operations(state, pos)?;

                if target.is_shared() || target.is_value() {
                    arg_values[0] = target.take_or_clone().flatten();
                    args = arg_values.iter_mut().collect();
                } else {
                    let (first, rest) = arg_values.split_first_mut().unwrap();
                    first_arg_value = Some(first);
                    args = once(target.take_ref().unwrap())
                        .chain(rest.iter_mut())
                        .collect();
                }
            } else {
                // func(..., ...) or func(mod::x, ...)
                arg_values = args_expr
                    .iter()
                    .map(|expr| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                            .map(Dynamic::flatten)
                    })
                    .chain(constant_args.iter().map(|(v, _)| Ok(v.clone())))
                    .collect::<Result<_, _>>()?;

                args = arg_values.iter_mut().collect();
            }
        }

        let module = self.search_imports(mods, state, namespace).ok_or_else(|| {
            EvalAltResult::ErrorModuleNotFound(namespace[0].name.to_string(), namespace[0].pos)
        })?;

        // First search in script-defined functions (can override built-in)
        let func = match module.get_qualified_fn(hash) {
            // Then search in Rust functions
            None => {
                self.inc_operations(state, pos)?;

                let hash_params = calc_fn_params_hash(args.iter().map(|a| a.type_id()));
                let hash_qualified_fn = combine_hashes(hash, hash_params);

                module.get_qualified_fn(hash_qualified_fn)
            }
            r => r,
        };

        // Clone first argument if the function is not a method after-all
        if let Some(first) = first_arg_value {
            if !func.map(|f| f.is_method()).unwrap_or(true) {
                let first_val = args[0].clone();
                args[0] = first;
                *args[0] = first_val;
            }
        }

        match func {
            #[cfg(not(feature = "no_function"))]
            Some(f) if f.is_script() => {
                let fn_def = f.get_fn_def();

                if fn_def.body.is_empty() {
                    Ok(Dynamic::UNIT)
                } else {
                    let args = args.as_mut();
                    let new_scope = &mut Default::default();

                    let mut source = module.id_raw().cloned();
                    mem::swap(&mut state.source, &mut source);

                    let level = level + 1;

                    let result = self.call_script_fn(
                        new_scope, mods, state, lib, &mut None, fn_def, args, pos, level,
                    );

                    state.source = source;

                    result
                }
            }

            Some(f) if f.is_plugin_fn() => f
                .get_plugin_fn()
                .clone()
                .call(
                    (self, fn_name, module.id(), &*mods, lib).into(),
                    args.as_mut(),
                )
                .map_err(|err| err.fill_position(pos)),

            Some(f) if f.is_native() => f.get_native_fn()(
                (self, fn_name, module.id(), &*mods, lib).into(),
                args.as_mut(),
            )
            .map_err(|err| err.fill_position(pos)),

            Some(f) => unreachable!("unknown function type: {:?}", f),

            None => EvalAltResult::ErrorFunctionNotFound(
                self.gen_call_signature(Some(namespace), fn_name, args.as_ref()),
                pos,
            )
            .into(),
        }
    }
}
