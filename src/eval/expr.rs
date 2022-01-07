//! Module defining functions for evaluating an expression.

use super::{ChainArgument, ChainType, EvalContext, EvalState, GlobalRuntimeState, Target};
use crate::ast::{Expr, FnCallExpr, Ident, OpAssignment};
use crate::engine::{FN_IDX_GET, FN_IDX_SET, KEYWORD_GLOBAL, KEYWORD_THIS, OP_CONCAT};
use crate::module::Namespace;
use crate::tokenizer::Token;
use crate::types::dynamic::{AccessMode, Union};
use crate::{
    Dynamic, Engine, ImmutableString, Module, Position, RhaiResult, RhaiResultOf, Scope, Shared,
    StaticVec, ERR, INT,
};
use std::num::NonZeroUsize;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

impl Engine {
    /// Search for a module within an imports stack.
    #[inline]
    #[must_use]
    pub(crate) fn search_imports(
        &self,
        global: &GlobalRuntimeState,
        state: &mut EvalState,
        namespace: &Namespace,
    ) -> Option<Shared<Module>> {
        let root = &namespace[0].name;

        // Qualified - check if the root module is directly indexed
        let index = if state.always_search_scope {
            None
        } else {
            namespace.index()
        };

        if let Some(index) = index {
            let offset = global.num_imported_modules() - index.get();
            Some(global.get_shared_module(offset).unwrap())
        } else {
            global
                .find_module(root)
                .map(|n| global.get_shared_module(n).unwrap())
                .or_else(|| self.global_sub_modules.get(root).cloned())
        }
    }

    /// Search for a variable within the scope or within imports,
    /// depending on whether the variable name is namespace-qualified.
    pub(crate) fn search_namespace<'s>(
        &self,
        scope: &'s mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &'s mut Option<&mut Dynamic>,
        expr: &Expr,
    ) -> RhaiResultOf<(Target<'s>, Position)> {
        match expr {
            Expr::Variable(Some(_), _, _) => {
                self.search_scope_only(scope, global, state, lib, this_ptr, expr)
            }
            Expr::Variable(None, _var_pos, v) => match v.as_ref() {
                // Normal variable access
                (_, None, _) => self.search_scope_only(scope, global, state, lib, this_ptr, expr),

                // Qualified variable access
                #[cfg(not(feature = "no_module"))]
                (_, Some((namespace, hash_var)), var_name) => {
                    if let Some(module) = self.search_imports(global, state, namespace) {
                        // foo:bar::baz::VARIABLE
                        return match module.get_qualified_var(*hash_var) {
                            Ok(target) => {
                                let mut target = target.clone();
                                // Module variables are constant
                                target.set_access_mode(AccessMode::ReadOnly);
                                Ok((target.into(), *_var_pos))
                            }
                            Err(err) => Err(match *err {
                                ERR::ErrorVariableNotFound(_, _) => ERR::ErrorVariableNotFound(
                                    format!(
                                        "{}{}{}",
                                        namespace,
                                        Token::DoubleColon.literal_syntax(),
                                        var_name
                                    ),
                                    namespace[0].pos,
                                )
                                .into(),
                                _ => err.fill_position(*_var_pos),
                            }),
                        };
                    }

                    #[cfg(not(feature = "no_function"))]
                    if namespace.len() == 1 && namespace[0].name == KEYWORD_GLOBAL {
                        // global::VARIABLE
                        let global_constants = global.constants_mut();

                        if let Some(mut guard) = global_constants {
                            if let Some(value) = guard.get_mut(var_name) {
                                let mut target: Target = value.clone().into();
                                // Module variables are constant
                                target.set_access_mode(AccessMode::ReadOnly);
                                return Ok((target.into(), *_var_pos));
                            }
                        }

                        return Err(ERR::ErrorVariableNotFound(
                            format!(
                                "{}{}{}",
                                namespace,
                                Token::DoubleColon.literal_syntax(),
                                var_name
                            ),
                            namespace[0].pos,
                        )
                        .into());
                    }

                    Err(ERR::ErrorModuleNotFound(namespace.to_string(), namespace[0].pos).into())
                }

                #[cfg(feature = "no_module")]
                (_, Some((_, _)), _) => unreachable!("qualified access under no_module"),
            },
            _ => unreachable!("Expr::Variable expected but gets {:?}", expr),
        }
    }

    /// Search for a variable within the scope
    ///
    /// # Panics
    ///
    /// Panics if `expr` is not [`Expr::Variable`].
    pub(crate) fn search_scope_only<'s>(
        &self,
        scope: &'s mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &'s mut Option<&mut Dynamic>,
        expr: &Expr,
    ) -> RhaiResultOf<(Target<'s>, Position)> {
        // Make sure that the pointer indirection is taken only when absolutely necessary.

        let (index, var_pos) = match expr {
            // Check if the variable is `this`
            Expr::Variable(None, pos, v) if v.0.is_none() && v.2 == KEYWORD_THIS => {
                return if let Some(val) = this_ptr {
                    Ok(((*val).into(), *pos))
                } else {
                    Err(ERR::ErrorUnboundThis(*pos).into())
                }
            }
            _ if state.always_search_scope => (0, expr.position()),
            Expr::Variable(Some(i), pos, _) => (i.get() as usize, *pos),
            Expr::Variable(None, pos, v) => (v.0.map(NonZeroUsize::get).unwrap_or(0), *pos),
            _ => unreachable!("Expr::Variable expected but gets {:?}", expr),
        };

        // Check the variable resolver, if any
        if let Some(ref resolve_var) = self.resolve_var {
            let context = EvalContext {
                engine: self,
                scope,
                global,
                state,
                lib,
                this_ptr,
                level: 0,
            };
            match resolve_var(
                expr.get_variable_name(true).expect("`Expr::Variable`"),
                index,
                &context,
            ) {
                Ok(Some(mut result)) => {
                    result.set_access_mode(AccessMode::ReadOnly);
                    return Ok((result.into(), var_pos));
                }
                Ok(None) => (),
                Err(err) => return Err(err.fill_position(var_pos)),
            }
        }

        let index = if index > 0 {
            scope.len() - index
        } else {
            // Find the variable in the scope
            let var_name = expr.get_variable_name(true).expect("`Expr::Variable`");
            scope
                .get_index(var_name)
                .ok_or_else(|| ERR::ErrorVariableNotFound(var_name.to_string(), var_pos))?
                .0
        };

        let val = scope.get_mut_by_index(index);

        Ok((val.into(), var_pos))
    }

    /// Chain-evaluate a dot/index chain.
    /// [`Position`] in [`EvalAltResult`] is [`NONE`][Position::NONE] and must be set afterwards.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_dot_index_chain_helper(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        target: &mut Target,
        root: (&str, Position),
        rhs: &Expr,
        terminate_chaining: bool,
        idx_values: &mut StaticVec<ChainArgument>,
        chain_type: ChainType,
        level: usize,
        new_val: Option<((Dynamic, Position), (Option<OpAssignment>, Position))>,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        let is_ref_mut = target.is_ref();
        let _terminate_chaining = terminate_chaining;

        // Pop the last index value
        let idx_val = idx_values.pop().unwrap();

        match chain_type {
            #[cfg(not(feature = "no_index"))]
            ChainType::Indexing => {
                let pos = rhs.position();
                let root_pos = idx_val.position();
                let idx_val = idx_val.into_index_value().expect("`ChainType::Index`");

                match rhs {
                    // xxx[idx].expr... | xxx[idx][expr]...
                    Expr::Dot(x, term, x_pos) | Expr::Index(x, term, x_pos)
                        if !_terminate_chaining =>
                    {
                        let mut idx_val_for_setter = idx_val.clone();
                        let idx_pos = x.lhs.position();
                        let rhs_chain = rhs.into();

                        let (try_setter, result) = {
                            let mut obj = self.get_indexed_mut(
                                global, state, lib, target, idx_val, idx_pos, false, true, level,
                            )?;
                            let is_obj_temp_val = obj.is_temp_value();
                            let obj_ptr = &mut obj;

                            match self.eval_dot_index_chain_helper(
                                global, state, lib, this_ptr, obj_ptr, root, &x.rhs, *term,
                                idx_values, rhs_chain, level, new_val,
                            ) {
                                Ok((result, true)) if is_obj_temp_val => {
                                    (Some(obj.take_or_clone()), (result, true))
                                }
                                Ok(result) => (None, result),
                                Err(err) => return Err(err.fill_position(*x_pos)),
                            }
                        };

                        if let Some(mut new_val) = try_setter {
                            // Try to call index setter if value is changed
                            let hash_set =
                                crate::ast::FnCallHashes::from_native(global.hash_idx_set());
                            let args = &mut [target, &mut idx_val_for_setter, &mut new_val];

                            if let Err(err) = self.exec_fn_call(
                                global, state, lib, FN_IDX_SET, hash_set, args, is_ref_mut, true,
                                root_pos, None, level,
                            ) {
                                // Just ignore if there is no index setter
                                if !matches!(*err, ERR::ErrorFunctionNotFound(_, _)) {
                                    return Err(err);
                                }
                            }
                        }

                        Ok(result)
                    }
                    // xxx[rhs] op= new_val
                    _ if new_val.is_some() => {
                        let ((new_val, new_pos), (op_info, op_pos)) = new_val.expect("`Some`");
                        let mut idx_val_for_setter = idx_val.clone();

                        let try_setter = match self.get_indexed_mut(
                            global, state, lib, target, idx_val, pos, true, false, level,
                        ) {
                            // Indexed value is a reference - update directly
                            Ok(ref mut obj_ptr) => {
                                self.eval_op_assignment(
                                    global, state, lib, op_info, op_pos, obj_ptr, root, new_val,
                                )
                                .map_err(|err| err.fill_position(new_pos))?;
                                #[cfg(not(feature = "unchecked"))]
                                self.check_data_size(obj_ptr, new_pos)?;
                                None
                            }
                            // Can't index - try to call an index setter
                            #[cfg(not(feature = "no_index"))]
                            Err(err) if matches!(*err, ERR::ErrorIndexingType(_, _)) => {
                                Some(new_val)
                            }
                            // Any other error
                            Err(err) => return Err(err),
                        };

                        if let Some(mut new_val) = try_setter {
                            // Try to call index setter
                            let hash_set =
                                crate::ast::FnCallHashes::from_native(global.hash_idx_set());
                            let args = &mut [target, &mut idx_val_for_setter, &mut new_val];

                            self.exec_fn_call(
                                global, state, lib, FN_IDX_SET, hash_set, args, is_ref_mut, true,
                                root_pos, None, level,
                            )?;
                        }

                        Ok((Dynamic::UNIT, true))
                    }
                    // xxx[rhs]
                    _ => self
                        .get_indexed_mut(
                            global, state, lib, target, idx_val, pos, false, true, level,
                        )
                        .map(|v| (v.take_or_clone(), false)),
                }
            }

            #[cfg(not(feature = "no_object"))]
            ChainType::Dotting => {
                match rhs {
                    // xxx.fn_name(arg_expr_list)
                    Expr::FnCall(x, pos) if !x.is_qualified() && new_val.is_none() => {
                        let FnCallExpr { name, hashes, .. } = x.as_ref();
                        let call_args = &mut idx_val.into_fn_call_args();
                        self.make_method_call(
                            global, state, lib, name, *hashes, target, call_args, *pos, level,
                        )
                    }
                    // xxx.fn_name(...) = ???
                    Expr::FnCall(_, _) if new_val.is_some() => {
                        unreachable!("method call cannot be assigned to")
                    }
                    // xxx.module::fn_name(...) - syntax error
                    Expr::FnCall(_, _) => {
                        unreachable!("function call in dot chain should not be namespace-qualified")
                    }
                    // {xxx:map}.id op= ???
                    Expr::Property(x) if target.is::<crate::Map>() && new_val.is_some() => {
                        let (name, pos) = &x.2;
                        let ((new_val, new_pos), (op_info, op_pos)) = new_val.expect("`Some`");
                        let index = name.into();
                        {
                            let val_target = &mut self.get_indexed_mut(
                                global, state, lib, target, index, *pos, true, false, level,
                            )?;
                            self.eval_op_assignment(
                                global, state, lib, op_info, op_pos, val_target, root, new_val,
                            )
                            .map_err(|err| err.fill_position(new_pos))?;
                        }
                        #[cfg(not(feature = "unchecked"))]
                        self.check_data_size(target.source(), new_pos)?;
                        Ok((Dynamic::UNIT, true))
                    }
                    // {xxx:map}.id
                    Expr::Property(x) if target.is::<crate::Map>() => {
                        let (name, pos) = &x.2;
                        let index = name.into();
                        let val = self.get_indexed_mut(
                            global, state, lib, target, index, *pos, false, false, level,
                        )?;
                        Ok((val.take_or_clone(), false))
                    }
                    // xxx.id op= ???
                    Expr::Property(x) if new_val.is_some() => {
                        let ((getter, hash_get), (setter, hash_set), (name, pos)) = x.as_ref();
                        let ((mut new_val, new_pos), (op_info, op_pos)) = new_val.expect("`Some`");

                        if op_info.is_some() {
                            let hash = crate::ast::FnCallHashes::from_native(*hash_get);
                            let args = &mut [target.as_mut()];
                            let (mut orig_val, _) = self
                                .exec_fn_call(
                                    global, state, lib, getter, hash, args, is_ref_mut, true, *pos,
                                    None, level,
                                )
                                .or_else(|err| match *err {
                                    // Try an indexer if property does not exist
                                    ERR::ErrorDotExpr(_, _) => {
                                        let prop = name.into();
                                        self.get_indexed_mut(
                                            global, state, lib, target, prop, *pos, false, true,
                                            level,
                                        )
                                        .map(|v| (v.take_or_clone(), false))
                                        .map_err(
                                            |idx_err| match *idx_err {
                                                ERR::ErrorIndexingType(_, _) => err,
                                                _ => idx_err,
                                            },
                                        )
                                    }
                                    _ => Err(err),
                                })?;

                            self.eval_op_assignment(
                                global,
                                state,
                                lib,
                                op_info,
                                op_pos,
                                &mut (&mut orig_val).into(),
                                root,
                                new_val,
                            )
                            .map_err(|err| err.fill_position(new_pos))?;

                            new_val = orig_val;
                        }

                        let hash = crate::ast::FnCallHashes::from_native(*hash_set);
                        let args = &mut [target.as_mut(), &mut new_val];
                        self.exec_fn_call(
                            global, state, lib, setter, hash, args, is_ref_mut, true, *pos, None,
                            level,
                        )
                        .or_else(|err| match *err {
                            // Try an indexer if property does not exist
                            ERR::ErrorDotExpr(_, _) => {
                                let args = &mut [target, &mut name.into(), &mut new_val];
                                let hash_set =
                                    crate::ast::FnCallHashes::from_native(global.hash_idx_set());
                                let pos = Position::NONE;

                                self.exec_fn_call(
                                    global, state, lib, FN_IDX_SET, hash_set, args, is_ref_mut,
                                    true, pos, None, level,
                                )
                                .map_err(
                                    |idx_err| match *idx_err {
                                        ERR::ErrorIndexingType(_, _) => err,
                                        _ => idx_err,
                                    },
                                )
                            }
                            _ => Err(err),
                        })
                    }
                    // xxx.id
                    Expr::Property(x) => {
                        let ((getter, hash_get), _, (name, pos)) = x.as_ref();
                        let hash = crate::ast::FnCallHashes::from_native(*hash_get);
                        let args = &mut [target.as_mut()];
                        self.exec_fn_call(
                            global, state, lib, getter, hash, args, is_ref_mut, true, *pos, None,
                            level,
                        )
                        .map_or_else(
                            |err| match *err {
                                // Try an indexer if property does not exist
                                ERR::ErrorDotExpr(_, _) => {
                                    let prop = name.into();
                                    self.get_indexed_mut(
                                        global, state, lib, target, prop, *pos, false, true, level,
                                    )
                                    .map(|v| (v.take_or_clone(), false))
                                    .map_err(|idx_err| {
                                        match *idx_err {
                                            ERR::ErrorIndexingType(_, _) => err,
                                            _ => idx_err,
                                        }
                                    })
                                }
                                _ => Err(err),
                            },
                            // Assume getters are always pure
                            |(v, _)| Ok((v, false)),
                        )
                    }
                    // {xxx:map}.sub_lhs[expr] | {xxx:map}.sub_lhs.expr
                    Expr::Index(x, term, x_pos) | Expr::Dot(x, term, x_pos)
                        if target.is::<crate::Map>() =>
                    {
                        let val_target = &mut match x.lhs {
                            Expr::Property(ref p) => {
                                let (name, pos) = &p.2;
                                let index = name.into();
                                self.get_indexed_mut(
                                    global, state, lib, target, index, *pos, false, true, level,
                                )?
                            }
                            // {xxx:map}.fn_name(arg_expr_list)[expr] | {xxx:map}.fn_name(arg_expr_list).expr
                            Expr::FnCall(ref x, pos) if !x.is_qualified() => {
                                let FnCallExpr { name, hashes, .. } = x.as_ref();
                                let call_args = &mut idx_val.into_fn_call_args();
                                let (val, _) = self.make_method_call(
                                    global, state, lib, name, *hashes, target, call_args, pos,
                                    level,
                                )?;
                                val.into()
                            }
                            // {xxx:map}.module::fn_name(...) - syntax error
                            Expr::FnCall(_, _) => unreachable!(
                                "function call in dot chain should not be namespace-qualified"
                            ),
                            // Others - syntax error
                            ref expr => unreachable!("invalid dot expression: {:?}", expr),
                        };
                        let rhs_chain = rhs.into();

                        self.eval_dot_index_chain_helper(
                            global, state, lib, this_ptr, val_target, root, &x.rhs, *term,
                            idx_values, rhs_chain, level, new_val,
                        )
                        .map_err(|err| err.fill_position(*x_pos))
                    }
                    // xxx.sub_lhs[expr] | xxx.sub_lhs.expr
                    Expr::Index(x, term, x_pos) | Expr::Dot(x, term, x_pos) => {
                        match x.lhs {
                            // xxx.prop[expr] | xxx.prop.expr
                            Expr::Property(ref p) => {
                                let ((getter, hash_get), (setter, hash_set), (name, pos)) =
                                    p.as_ref();
                                let rhs_chain = rhs.into();
                                let hash_get = crate::ast::FnCallHashes::from_native(*hash_get);
                                let hash_set = crate::ast::FnCallHashes::from_native(*hash_set);
                                let mut arg_values = [target.as_mut(), &mut Dynamic::UNIT.clone()];
                                let args = &mut arg_values[..1];

                                // Assume getters are always pure
                                let (mut val, _) = self
                                    .exec_fn_call(
                                        global, state, lib, getter, hash_get, args, is_ref_mut,
                                        true, *pos, None, level,
                                    )
                                    .or_else(|err| match *err {
                                        // Try an indexer if property does not exist
                                        ERR::ErrorDotExpr(_, _) => {
                                            let prop = name.into();
                                            self.get_indexed_mut(
                                                global, state, lib, target, prop, *pos, false,
                                                true, level,
                                            )
                                            .map(|v| (v.take_or_clone(), false))
                                            .map_err(
                                                |idx_err| match *idx_err {
                                                    ERR::ErrorIndexingType(_, _) => err,
                                                    _ => idx_err,
                                                },
                                            )
                                        }
                                        _ => Err(err),
                                    })?;

                                let val = &mut val;

                                let (result, may_be_changed) = self
                                    .eval_dot_index_chain_helper(
                                        global,
                                        state,
                                        lib,
                                        this_ptr,
                                        &mut val.into(),
                                        root,
                                        &x.rhs,
                                        *term,
                                        idx_values,
                                        rhs_chain,
                                        level,
                                        new_val,
                                    )
                                    .map_err(|err| err.fill_position(*x_pos))?;

                                // Feed the value back via a setter just in case it has been updated
                                if may_be_changed {
                                    // Re-use args because the first &mut parameter will not be consumed
                                    let mut arg_values = [target.as_mut(), val];
                                    let args = &mut arg_values;
                                    self.exec_fn_call(
                                        global, state, lib, setter, hash_set, args, is_ref_mut,
                                        true, *pos, None, level,
                                    )
                                    .or_else(
                                        |err| match *err {
                                            // Try an indexer if property does not exist
                                            ERR::ErrorDotExpr(_, _) => {
                                                let args =
                                                    &mut [target.as_mut(), &mut name.into(), val];
                                                let hash_set =
                                                    crate::ast::FnCallHashes::from_native(
                                                        global.hash_idx_set(),
                                                    );
                                                self.exec_fn_call(
                                                    global, state, lib, FN_IDX_SET, hash_set, args,
                                                    is_ref_mut, true, *pos, None, level,
                                                )
                                                .or_else(|idx_err| match *idx_err {
                                                    ERR::ErrorIndexingType(_, _) => {
                                                        // If there is no setter, no need to feed it back because
                                                        // the property is read-only
                                                        Ok((Dynamic::UNIT, false))
                                                    }
                                                    _ => Err(idx_err),
                                                })
                                            }
                                            _ => Err(err),
                                        },
                                    )?;
                                }

                                Ok((result, may_be_changed))
                            }
                            // xxx.fn_name(arg_expr_list)[expr] | xxx.fn_name(arg_expr_list).expr
                            Expr::FnCall(ref f, pos) if !f.is_qualified() => {
                                let FnCallExpr { name, hashes, .. } = f.as_ref();
                                let rhs_chain = rhs.into();
                                let args = &mut idx_val.into_fn_call_args();
                                let (mut val, _) = self.make_method_call(
                                    global, state, lib, name, *hashes, target, args, pos, level,
                                )?;
                                let val = &mut val;
                                let target = &mut val.into();

                                self.eval_dot_index_chain_helper(
                                    global, state, lib, this_ptr, target, root, &x.rhs, *term,
                                    idx_values, rhs_chain, level, new_val,
                                )
                                .map_err(|err| err.fill_position(pos))
                            }
                            // xxx.module::fn_name(...) - syntax error
                            Expr::FnCall(_, _) => unreachable!(
                                "function call in dot chain should not be namespace-qualified"
                            ),
                            // Others - syntax error
                            ref expr => unreachable!("invalid dot expression: {:?}", expr),
                        }
                    }
                    // Syntax error
                    _ => Err(ERR::ErrorDotExpr("".into(), rhs.position()).into()),
                }
            }
        }
    }

    /// Evaluate a dot/index chain.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    pub(crate) fn eval_dot_index_chain(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
        new_val: Option<((Dynamic, Position), (Option<OpAssignment>, Position))>,
    ) -> RhaiResult {
        let (crate::ast::BinaryExpr { lhs, rhs }, chain_type, term, op_pos) = match expr {
            #[cfg(not(feature = "no_index"))]
            Expr::Index(x, term, pos) => (x.as_ref(), ChainType::Indexing, *term, *pos),
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(x, term, pos) => (x.as_ref(), ChainType::Dotting, *term, *pos),
            expr => unreachable!("Expr::Index or Expr::Dot expected but gets {:?}", expr),
        };

        let idx_values = &mut StaticVec::new_const();

        self.eval_dot_index_chain_arguments(
            scope, global, state, lib, this_ptr, rhs, term, chain_type, idx_values, 0, level,
        )?;

        let is_assignment = new_val.is_some();

        match lhs {
            // id.??? or id[???]
            Expr::Variable(_, var_pos, x) => {
                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, *var_pos)?;

                let (mut target, _) =
                    self.search_namespace(scope, global, state, lib, this_ptr, lhs)?;

                let obj_ptr = &mut target;
                let root = (x.2.as_str(), *var_pos);

                self.eval_dot_index_chain_helper(
                    global, state, lib, &mut None, obj_ptr, root, rhs, term, idx_values,
                    chain_type, level, new_val,
                )
                .map(|(v, _)| v)
                .map_err(|err| err.fill_position(op_pos))
            }
            // {expr}.??? = ??? or {expr}[???] = ???
            _ if is_assignment => unreachable!("cannot assign to an expression"),
            // {expr}.??? or {expr}[???]
            expr => {
                let value = self.eval_expr(scope, global, state, lib, this_ptr, expr, level)?;
                let obj_ptr = &mut value.into();
                let root = ("", expr.position());
                self.eval_dot_index_chain_helper(
                    global, state, lib, this_ptr, obj_ptr, root, rhs, term, idx_values, chain_type,
                    level, new_val,
                )
                .map(|(v, _)| if is_assignment { Dynamic::UNIT } else { v })
                .map_err(|err| err.fill_position(op_pos))
            }
        }
    }

    /// Evaluate a chain of indexes and store the results in a [`StaticVec`].
    /// [`StaticVec`] is used to avoid an allocation in the overwhelming cases of
    /// just a few levels of indexing.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_dot_index_chain_arguments(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        terminate_chaining: bool,
        parent_chain_type: ChainType,
        idx_values: &mut StaticVec<ChainArgument>,
        size: usize,
        level: usize,
    ) -> RhaiResultOf<()> {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, expr.position())?;

        let _parent_chain_type = parent_chain_type;

        match expr {
            #[cfg(not(feature = "no_object"))]
            Expr::FnCall(x, _) if _parent_chain_type == ChainType::Dotting && !x.is_qualified() => {
                let crate::ast::FnCallExpr {
                    args, constants, ..
                } = x.as_ref();

                let (values, pos) = args.iter().try_fold(
                    (crate::FnArgsVec::with_capacity(args.len()), Position::NONE),
                    |(mut values, mut pos), expr| -> RhaiResultOf<_> {
                        let (value, arg_pos) = self.get_arg_value(
                            scope, global, state, lib, this_ptr, level, expr, constants,
                        )?;
                        if values.is_empty() {
                            pos = arg_pos;
                        }
                        values.push(value.flatten());
                        Ok((values, pos))
                    },
                )?;

                idx_values.push(ChainArgument::from_fn_call_args(values, pos));
            }
            #[cfg(not(feature = "no_object"))]
            Expr::FnCall(_, _) if _parent_chain_type == ChainType::Dotting => {
                unreachable!("function call in dot chain should not be namespace-qualified")
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Property(x) if _parent_chain_type == ChainType::Dotting => {
                idx_values.push(ChainArgument::Property((x.2).1))
            }
            Expr::Property(_) => unreachable!("unexpected Expr::Property for indexing"),

            Expr::Index(x, term, _) | Expr::Dot(x, term, _) if !terminate_chaining => {
                let crate::ast::BinaryExpr { lhs, rhs, .. } = x.as_ref();

                // Evaluate in left-to-right order
                let lhs_arg_val = match lhs {
                    #[cfg(not(feature = "no_object"))]
                    Expr::Property(x) if _parent_chain_type == ChainType::Dotting => {
                        ChainArgument::Property((x.2).1)
                    }
                    Expr::Property(_) => unreachable!("unexpected Expr::Property for indexing"),

                    #[cfg(not(feature = "no_object"))]
                    Expr::FnCall(x, _)
                        if _parent_chain_type == ChainType::Dotting && !x.is_qualified() =>
                    {
                        let crate::ast::FnCallExpr {
                            args, constants, ..
                        } = x.as_ref();

                        let (values, pos) = args.iter().try_fold(
                            (crate::FnArgsVec::with_capacity(args.len()), Position::NONE),
                            |(mut values, mut pos), expr| -> RhaiResultOf<_> {
                                let (value, arg_pos) = self.get_arg_value(
                                    scope, global, state, lib, this_ptr, level, expr, constants,
                                )?;
                                if values.is_empty() {
                                    pos = arg_pos
                                }
                                values.push(value.flatten());
                                Ok((values, pos))
                            },
                        )?;
                        ChainArgument::from_fn_call_args(values, pos)
                    }
                    #[cfg(not(feature = "no_object"))]
                    Expr::FnCall(_, _) if _parent_chain_type == ChainType::Dotting => {
                        unreachable!("function call in dot chain should not be namespace-qualified")
                    }
                    #[cfg(not(feature = "no_object"))]
                    expr if _parent_chain_type == ChainType::Dotting => {
                        unreachable!("invalid dot expression: {:?}", expr);
                    }
                    #[cfg(not(feature = "no_index"))]
                    _ if _parent_chain_type == ChainType::Indexing => self
                        .eval_expr(scope, global, state, lib, this_ptr, lhs, level)
                        .map(|v| ChainArgument::from_index_value(v.flatten(), lhs.position()))?,
                    expr => unreachable!("unknown chained expression: {:?}", expr),
                };

                // Push in reverse order
                let chain_type = expr.into();

                self.eval_dot_index_chain_arguments(
                    scope, global, state, lib, this_ptr, rhs, *term, chain_type, idx_values, size,
                    level,
                )?;

                idx_values.push(lhs_arg_val);
            }

            #[cfg(not(feature = "no_object"))]
            _ if _parent_chain_type == ChainType::Dotting => {
                unreachable!("invalid dot expression: {:?}", expr);
            }
            #[cfg(not(feature = "no_index"))]
            _ if _parent_chain_type == ChainType::Indexing => idx_values.push(
                self.eval_expr(scope, global, state, lib, this_ptr, expr, level)
                    .map(|v| ChainArgument::from_index_value(v.flatten(), expr.position()))?,
            ),
            _ => unreachable!("unknown chained expression: {:?}", expr),
        }

        Ok(())
    }

    /// Get the value at the indexed position of a base type.
    /// [`Position`] in [`EvalAltResult`] may be [`NONE`][Position::NONE] and should be set afterwards.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn get_indexed_mut<'t>(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        target: &'t mut Dynamic,
        idx: Dynamic,
        idx_pos: Position,
        add_if_not_found: bool,
        use_indexers: bool,
        level: usize,
    ) -> RhaiResultOf<Target<'t>> {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, Position::NONE)?;

        let mut idx = idx;
        let _add_if_not_found = add_if_not_found;

        match target {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(arr, _, _)) => {
                // val_array[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;

                let arr_len = arr.len();

                #[cfg(not(feature = "unchecked"))]
                let arr_idx = if index < 0 {
                    // Count from end if negative
                    arr_len
                        - index
                            .checked_abs()
                            .ok_or_else(|| ERR::ErrorArrayBounds(arr_len, index, idx_pos))
                            .and_then(|n| {
                                if n as usize > arr_len {
                                    Err(ERR::ErrorArrayBounds(arr_len, index, idx_pos).into())
                                } else {
                                    Ok(n as usize)
                                }
                            })?
                } else {
                    index as usize
                };
                #[cfg(feature = "unchecked")]
                let arr_idx = if index < 0 {
                    // Count from end if negative
                    arr_len - index.abs() as usize
                } else {
                    index as usize
                };

                arr.get_mut(arr_idx)
                    .map(Target::from)
                    .ok_or_else(|| ERR::ErrorArrayBounds(arr_len, index, idx_pos).into())
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Blob(arr, _, _)) => {
                // val_blob[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;

                let arr_len = arr.len();

                #[cfg(not(feature = "unchecked"))]
                let arr_idx = if index < 0 {
                    // Count from end if negative
                    arr_len
                        - index
                            .checked_abs()
                            .ok_or_else(|| ERR::ErrorArrayBounds(arr_len, index, idx_pos))
                            .and_then(|n| {
                                if n as usize > arr_len {
                                    Err(ERR::ErrorArrayBounds(arr_len, index, idx_pos).into())
                                } else {
                                    Ok(n as usize)
                                }
                            })?
                } else {
                    index as usize
                };
                #[cfg(feature = "unchecked")]
                let arr_idx = if index < 0 {
                    // Count from end if negative
                    arr_len - index.abs() as usize
                } else {
                    index as usize
                };

                let value = arr
                    .get(arr_idx)
                    .map(|&v| (v as INT).into())
                    .ok_or_else(|| Box::new(ERR::ErrorArrayBounds(arr_len, index, idx_pos)))?;
                Ok(Target::BlobByte {
                    source: target,
                    value,
                    index: arr_idx,
                })
            }

            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(map, _, _)) => {
                // val_map[idx]
                let index = idx.read_lock::<ImmutableString>().ok_or_else(|| {
                    self.make_type_mismatch_err::<ImmutableString>(idx.type_name(), idx_pos)
                })?;

                if _add_if_not_found && !map.contains_key(index.as_str()) {
                    map.insert(index.clone().into(), Dynamic::UNIT);
                }

                Ok(map
                    .get_mut(index.as_str())
                    .map(Target::from)
                    .unwrap_or_else(|| Target::from(Dynamic::UNIT)))
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Int(value, _, _))
                if idx.is::<crate::ExclusiveRange>() || idx.is::<crate::InclusiveRange>() =>
            {
                #[cfg(not(feature = "only_i32"))]
                type BASE = u64;
                #[cfg(feature = "only_i32")]
                type BASE = u32;

                // val_int[range]
                const BITS: usize = std::mem::size_of::<INT>() * 8;

                let (shift, mask) = if let Some(range) = idx.read_lock::<crate::ExclusiveRange>() {
                    let start = range.start;
                    let end = range.end;

                    if start < 0 || start as usize >= BITS {
                        return Err(ERR::ErrorBitFieldBounds(BITS, start, idx_pos).into());
                    } else if end < 0 || end as usize >= BITS {
                        return Err(ERR::ErrorBitFieldBounds(BITS, end, idx_pos).into());
                    } else if end <= start {
                        (0, 0)
                    } else if end as usize == BITS && start == 0 {
                        // -1 = all bits set
                        (0, -1)
                    } else {
                        (
                            start as u8,
                            // 2^bits - 1
                            (((2 as BASE).pow((end - start) as u32) - 1) as INT) << start,
                        )
                    }
                } else if let Some(range) = idx.read_lock::<crate::InclusiveRange>() {
                    let start = *range.start();
                    let end = *range.end();

                    if start < 0 || start as usize >= BITS {
                        return Err(ERR::ErrorBitFieldBounds(BITS, start, idx_pos).into());
                    } else if end < 0 || end as usize >= BITS {
                        return Err(ERR::ErrorBitFieldBounds(BITS, end, idx_pos).into());
                    } else if end < start {
                        (0, 0)
                    } else if end as usize == BITS - 1 && start == 0 {
                        // -1 = all bits set
                        (0, -1)
                    } else {
                        (
                            start as u8,
                            // 2^bits - 1
                            (((2 as BASE).pow((end - start + 1) as u32) - 1) as INT) << start,
                        )
                    }
                } else {
                    unreachable!("Range or RangeInclusive expected but gets {:?}", idx);
                };

                let field_value = (*value & mask) >> shift;

                Ok(Target::BitField {
                    source: target,
                    value: field_value.into(),
                    mask,
                    shift,
                })
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Int(value, _, _)) => {
                // val_int[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;

                const BITS: usize = std::mem::size_of::<INT>() * 8;

                let (bit_value, offset) = if index >= 0 {
                    let offset = index as usize;
                    (
                        if offset >= BITS {
                            return Err(ERR::ErrorBitFieldBounds(BITS, index, idx_pos).into());
                        } else {
                            (*value & (1 << offset)) != 0
                        },
                        offset as u8,
                    )
                } else if let Some(abs_index) = index.checked_abs() {
                    let offset = abs_index as usize;
                    (
                        // Count from end if negative
                        if offset > BITS {
                            return Err(ERR::ErrorBitFieldBounds(BITS, index, idx_pos).into());
                        } else {
                            (*value & (1 << (BITS - offset))) != 0
                        },
                        offset as u8,
                    )
                } else {
                    return Err(ERR::ErrorBitFieldBounds(BITS, index, idx_pos).into());
                };

                Ok(Target::Bit {
                    source: target,
                    value: bit_value.into(),
                    bit: offset,
                })
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Str(s, _, _)) => {
                // val_string[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;

                let (ch, offset) = if index >= 0 {
                    let offset = index as usize;
                    (
                        s.chars().nth(offset).ok_or_else(|| {
                            let chars_len = s.chars().count();
                            ERR::ErrorStringBounds(chars_len, index, idx_pos)
                        })?,
                        offset,
                    )
                } else if let Some(abs_index) = index.checked_abs() {
                    let offset = abs_index as usize;
                    (
                        // Count from end if negative
                        s.chars().rev().nth(offset - 1).ok_or_else(|| {
                            let chars_len = s.chars().count();
                            ERR::ErrorStringBounds(chars_len, index, idx_pos)
                        })?,
                        offset,
                    )
                } else {
                    let chars_len = s.chars().count();
                    return Err(ERR::ErrorStringBounds(chars_len, index, idx_pos).into());
                };

                Ok(Target::StringChar {
                    source: target,
                    value: ch.into(),
                    index: offset,
                })
            }

            _ if use_indexers => {
                let args = &mut [target, &mut idx];
                let hash_get = crate::ast::FnCallHashes::from_native(global.hash_idx_get());
                let idx_pos = Position::NONE;

                self.exec_fn_call(
                    global, state, lib, FN_IDX_GET, hash_get, args, true, true, idx_pos, None,
                    level,
                )
                .map(|(v, _)| v.into())
            }

            _ => Err(ERR::ErrorIndexingType(
                format!(
                    "{} [{}]",
                    self.map_type_name(target.type_name()),
                    self.map_type_name(idx.type_name())
                ),
                Position::NONE,
            )
            .into()),
        }
    }

    /// Evaluate a function call expression.
    pub(crate) fn eval_fn_call_expr(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &FnCallExpr,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        let FnCallExpr {
            name,
            namespace,
            capture_parent_scope: capture,
            hashes,
            args,
            constants,
            ..
        } = expr;

        if let Some(namespace) = namespace.as_ref() {
            // Qualified function call
            let hash = hashes.native;

            self.make_qualified_function_call(
                scope, global, state, lib, this_ptr, namespace, name, args, constants, hash, pos,
                level,
            )
        } else {
            // Normal function call
            self.make_function_call(
                scope, global, state, lib, this_ptr, name, args, constants, *hashes, pos, *capture,
                level,
            )
        }
    }

    /// Evaluate an expression.
    pub(crate) fn eval_expr(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
    ) -> RhaiResult {
        // Coded this way for better branch prediction.
        // Popular branches are lifted out of the `match` statement into their own branches.

        // Function calls should account for a relatively larger portion of expressions because
        // binary operators are also function calls.
        if let Expr::FnCall(x, pos) = expr {
            #[cfg(not(feature = "unchecked"))]
            self.inc_operations(&mut global.num_operations, expr.position())?;

            return self.eval_fn_call_expr(scope, global, state, lib, this_ptr, x, *pos, level);
        }

        // Then variable access.
        // We shouldn't do this for too many variants because, soon or later, the added comparisons
        // will cost more than the mis-predicted `match` branch.
        if let Expr::Variable(index, var_pos, x) = expr {
            #[cfg(not(feature = "unchecked"))]
            self.inc_operations(&mut global.num_operations, expr.position())?;

            return if index.is_none() && x.0.is_none() && x.2 == KEYWORD_THIS {
                this_ptr
                    .as_deref()
                    .cloned()
                    .ok_or_else(|| ERR::ErrorUnboundThis(*var_pos).into())
            } else {
                self.search_namespace(scope, global, state, lib, this_ptr, expr)
                    .map(|(val, _)| val.take_or_clone())
            };
        }

        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, expr.position())?;

        match expr {
            // Constants
            Expr::DynamicConstant(x, _) => Ok(x.as_ref().clone()),
            Expr::IntegerConstant(x, _) => Ok((*x).into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(x, _) => Ok((*x).into()),
            Expr::StringConstant(x, _) => Ok(x.clone().into()),
            Expr::CharConstant(x, _) => Ok((*x).into()),
            Expr::BoolConstant(x, _) => Ok((*x).into()),
            Expr::Unit(_) => Ok(Dynamic::UNIT),

            // `... ${...} ...`
            Expr::InterpolatedString(x, pos) => {
                let mut pos = *pos;
                let mut result: Dynamic = self.const_empty_string().into();

                for expr in x.iter() {
                    let item = self.eval_expr(scope, global, state, lib, this_ptr, expr, level)?;

                    self.eval_op_assignment(
                        global,
                        state,
                        lib,
                        Some(OpAssignment::new(OP_CONCAT)),
                        pos,
                        &mut (&mut result).into(),
                        ("", Position::NONE),
                        item,
                    )
                    .map_err(|err| err.fill_position(expr.position()))?;

                    pos = expr.position();
                }

                Ok(result)
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Array(x, _) => {
                let mut arr = Dynamic::from_array(crate::Array::with_capacity(x.len()));

                #[cfg(not(feature = "unchecked"))]
                let mut sizes = (0, 0, 0);

                for item_expr in x.iter() {
                    let value = self
                        .eval_expr(scope, global, state, lib, this_ptr, item_expr, level)?
                        .flatten();

                    #[cfg(not(feature = "unchecked"))]
                    let val_sizes = Self::calc_data_sizes(&value, true);

                    arr.write_lock::<crate::Array>()
                        .expect("`Array`")
                        .push(value);

                    #[cfg(not(feature = "unchecked"))]
                    if self.has_data_size_limit() {
                        sizes = (
                            sizes.0 + val_sizes.0,
                            sizes.1 + val_sizes.1,
                            sizes.2 + val_sizes.2,
                        );
                        self.raise_err_if_over_data_size_limit(sizes, item_expr.position())?;
                    }
                }

                Ok(arr)
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Map(x, _) => {
                let mut map = Dynamic::from_map(x.1.clone());

                #[cfg(not(feature = "unchecked"))]
                let mut sizes = (0, 0, 0);

                for (Ident { name, .. }, value_expr) in x.0.iter() {
                    let key = name.as_str();
                    let value = self
                        .eval_expr(scope, global, state, lib, this_ptr, value_expr, level)?
                        .flatten();

                    #[cfg(not(feature = "unchecked"))]
                    let val_sizes = Self::calc_data_sizes(&value, true);

                    *map.write_lock::<crate::Map>()
                        .expect("`Map`")
                        .get_mut(key)
                        .unwrap() = value;

                    #[cfg(not(feature = "unchecked"))]
                    if self.has_data_size_limit() {
                        sizes = (
                            sizes.0 + val_sizes.0,
                            sizes.1 + val_sizes.1,
                            sizes.2 + val_sizes.2,
                        );
                        self.raise_err_if_over_data_size_limit(sizes, value_expr.position())?;
                    }
                }

                Ok(map)
            }

            Expr::And(x, _) => {
                Ok((self
                    .eval_expr(scope, global, state, lib, this_ptr, &x.lhs, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, x.lhs.position()))?
                    && // Short-circuit using &&
                self
                    .eval_expr(scope, global, state, lib, this_ptr, &x.rhs, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, x.rhs.position()))?)
                .into())
            }

            Expr::Or(x, _) => {
                Ok((self
                    .eval_expr(scope, global, state, lib, this_ptr, &x.lhs, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, x.lhs.position()))?
                    || // Short-circuit using ||
                self
                    .eval_expr(scope, global, state, lib, this_ptr, &x.rhs, level)?
                    .as_bool()
                    .map_err(|typ| self.make_type_mismatch_err::<bool>(typ, x.rhs.position()))?)
                .into())
            }

            Expr::Custom(custom, _) => {
                let expressions: StaticVec<_> = custom.inputs.iter().map(Into::into).collect();
                let key_token = custom.tokens.first().unwrap();
                let custom_def = self.custom_syntax.get(key_token).unwrap();
                let mut context = EvalContext {
                    engine: self,
                    scope,
                    global,
                    state,
                    lib,
                    this_ptr,
                    level,
                };

                let result = (custom_def.func)(&mut context, &expressions);

                self.check_return_value(result, expr.position())
            }

            Expr::Stmt(x) if x.is_empty() => Ok(Dynamic::UNIT),
            Expr::Stmt(x) => {
                self.eval_stmt_block(scope, global, state, lib, this_ptr, x, true, level)
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Index(_, _, _) => {
                self.eval_dot_index_chain(scope, global, state, lib, this_ptr, expr, level, None)
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Dot(_, _, _) => {
                self.eval_dot_index_chain(scope, global, state, lib, this_ptr, expr, level, None)
            }

            _ => unreachable!("expression cannot be evaluated: {:?}", expr),
        }
    }
}
