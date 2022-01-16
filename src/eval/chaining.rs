//! Types to support chaining operations (i.e. indexing and dotting).
#![cfg(any(not(feature = "no_index"), not(feature = "no_object")))]

use super::{EvalState, GlobalRuntimeState, Target};
use crate::ast::{Expr, OpAssignment};
use crate::types::dynamic::Union;
use crate::{Dynamic, Engine, Module, Position, RhaiResult, RhaiResultOf, Scope, StaticVec, ERR};
use std::hash::Hash;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// Method of chaining.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ChainType {
    /// Indexing.
    #[cfg(not(feature = "no_index"))]
    Indexing,
    /// Dotting.
    #[cfg(not(feature = "no_object"))]
    Dotting,
}

impl From<&Expr> for ChainType {
    #[inline]
    fn from(expr: &Expr) -> Self {
        match expr {
            #[cfg(not(feature = "no_index"))]
            Expr::Index(_, _, _) => Self::Indexing,
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(_, _, _) => Self::Dotting,
            expr => unreachable!("Expr::Index or Expr::Dot expected but gets {:?}", expr),
        }
    }
}

/// Value of a chaining argument.
#[derive(Debug, Clone, Hash)]
pub enum ChainArgument {
    /// Dot-property access.
    #[cfg(not(feature = "no_object"))]
    Property(Position),
    /// Arguments to a dot method call.
    /// Wrapped values are the arguments plus the [position][Position] of the first argument.
    ///
    /// Since many dotted function calls have no arguments (e.g. `string.len()`), it is better to
    /// reduce the size of [`ChainArgument`] by using a boxed slice.
    #[cfg(not(feature = "no_object"))]
    MethodCallArgs(Option<Box<[Dynamic]>>, Position),
    /// Index value and [position][Position].
    #[cfg(not(feature = "no_index"))]
    IndexValue(Dynamic, Position),
}

impl ChainArgument {
    /// Return the index value.
    #[inline(always)]
    #[cfg(not(feature = "no_index"))]
    #[must_use]
    pub fn into_index_value(self) -> Option<Dynamic> {
        match self {
            Self::IndexValue(value, _) => Some(value),
            #[cfg(not(feature = "no_object"))]
            _ => None,
        }
    }
    /// Return the list of method call arguments.
    ///
    /// # Panics
    ///
    /// Panics if the [`ChainArgument`] is not [`MethodCallArgs`][ChainArgument::MethodCallArgs].
    #[inline(always)]
    #[cfg(not(feature = "no_object"))]
    #[must_use]
    pub fn into_fn_call_args(self) -> (crate::FnArgsVec<Dynamic>, Position) {
        match self {
            Self::MethodCallArgs(None, pos) => (crate::FnArgsVec::new_const(), pos),
            Self::MethodCallArgs(Some(mut values), pos) => {
                (values.iter_mut().map(std::mem::take).collect(), pos)
            }
            x => unreachable!("ChainArgument::MethodCallArgs expected but gets {:?}", x),
        }
    }
    /// Return the [position][Position].
    #[inline(always)]
    #[must_use]
    #[allow(dead_code)]
    pub const fn position(&self) -> Position {
        match self {
            #[cfg(not(feature = "no_object"))]
            ChainArgument::Property(pos) => *pos,
            #[cfg(not(feature = "no_object"))]
            ChainArgument::MethodCallArgs(_, pos) => *pos,
            #[cfg(not(feature = "no_index"))]
            ChainArgument::IndexValue(_, pos) => *pos,
        }
    }
    /// Create n [`MethodCallArgs`][ChainArgument::MethodCallArgs].
    #[inline(always)]
    #[cfg(not(feature = "no_object"))]
    #[must_use]
    pub fn from_fn_call_args(values: crate::FnArgsVec<Dynamic>, pos: Position) -> Self {
        if values.is_empty() {
            Self::MethodCallArgs(None, pos)
        } else {
            Self::MethodCallArgs(Some(values.into_vec().into()), pos)
        }
    }
    /// Create an [`IndexValue`][ChainArgument::IndexValue].
    #[inline(always)]
    #[cfg(not(feature = "no_index"))]
    #[must_use]
    pub const fn from_index_value(value: Dynamic, pos: Position) -> Self {
        Self::IndexValue(value, pos)
    }
}

impl Engine {
    /// Chain-evaluate a dot/index chain.
    /// [`Position`] in [`EvalAltResult`] is [`NONE`][Position::NONE] and must be set afterwards.
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
        idx_values: &mut StaticVec<super::ChainArgument>,
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
                            let fn_name = crate::engine::FN_IDX_SET;

                            if let Err(err) = self.exec_fn_call(
                                global, state, lib, fn_name, hash_set, args, is_ref_mut, true,
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
                            let fn_name = crate::engine::FN_IDX_SET;

                            self.exec_fn_call(
                                global, state, lib, fn_name, hash_set, args, is_ref_mut, true,
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
                        let crate::ast::FnCallExpr { name, hashes, .. } = x.as_ref();
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
                                let fn_name = crate::engine::FN_IDX_SET;
                                let hash_set =
                                    crate::ast::FnCallHashes::from_native(global.hash_idx_set());
                                let pos = Position::NONE;

                                self.exec_fn_call(
                                    global, state, lib, fn_name, hash_set, args, is_ref_mut, true,
                                    pos, None, level,
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
                                let crate::ast::FnCallExpr { name, hashes, .. } = x.as_ref();
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
                                                let fn_name = crate::engine::FN_IDX_SET;
                                                let hash_set =
                                                    crate::ast::FnCallHashes::from_native(
                                                        global.hash_idx_set(),
                                                    );
                                                self.exec_fn_call(
                                                    global, state, lib, fn_name, hash_set, args,
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
                                let crate::ast::FnCallExpr { name, hashes, .. } = f.as_ref();
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
        idx_values: &mut StaticVec<super::ChainArgument>,
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

                idx_values.push(super::ChainArgument::from_fn_call_args(values, pos));
            }
            #[cfg(not(feature = "no_object"))]
            Expr::FnCall(_, _) if _parent_chain_type == ChainType::Dotting => {
                unreachable!("function call in dot chain should not be namespace-qualified")
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Property(x) if _parent_chain_type == ChainType::Dotting => {
                idx_values.push(super::ChainArgument::Property((x.2).1))
            }
            Expr::Property(_) => unreachable!("unexpected Expr::Property for indexing"),

            Expr::Index(x, term, _) | Expr::Dot(x, term, _) if !terminate_chaining => {
                let crate::ast::BinaryExpr { lhs, rhs, .. } = x.as_ref();

                // Evaluate in left-to-right order
                let lhs_arg_val = match lhs {
                    #[cfg(not(feature = "no_object"))]
                    Expr::Property(x) if _parent_chain_type == ChainType::Dotting => {
                        super::ChainArgument::Property((x.2).1)
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
                        super::ChainArgument::from_fn_call_args(values, pos)
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
                        .map(|v| {
                            super::ChainArgument::from_index_value(v.flatten(), lhs.position())
                        })?,
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
                    .map(|v| {
                        super::ChainArgument::from_index_value(v.flatten(), expr.position())
                    })?,
            ),
            _ => unreachable!("unknown chained expression: {:?}", expr),
        }

        Ok(())
    }

    /// Get the value at the indexed position of a base type.
    /// [`Position`] in [`EvalAltResult`] may be [`NONE`][Position::NONE] and should be set afterwards.
    fn get_indexed_mut<'t>(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        target: &'t mut Dynamic,
        idx: Dynamic,
        pos: Position,
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
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, pos))?;
                let len = arr.len();
                let arr_idx = super::calc_index(len, index, true, || {
                    ERR::ErrorArrayBounds(len, index, pos).into()
                })?;

                Ok(arr.get_mut(arr_idx).map(Target::from).unwrap())
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Blob(arr, _, _)) => {
                // val_blob[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, pos))?;
                let len = arr.len();
                let arr_idx = super::calc_index(len, index, true, || {
                    ERR::ErrorArrayBounds(len, index, pos).into()
                })?;

                let value = arr.get(arr_idx).map(|&v| (v as crate::INT).into()).unwrap();

                Ok(Target::BlobByte {
                    source: target,
                    value,
                    index: arr_idx,
                })
            }

            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(map, _, _)) => {
                // val_map[idx]
                let index = idx.read_lock::<crate::ImmutableString>().ok_or_else(|| {
                    self.make_type_mismatch_err::<crate::ImmutableString>(idx.type_name(), pos)
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
                // val_int[range]
                const BITS: usize = std::mem::size_of::<crate::INT>() * 8;

                let (shift, mask) = if let Some(range) = idx.read_lock::<crate::ExclusiveRange>() {
                    let start = range.start;
                    let end = range.end;

                    let start = super::calc_index(BITS, start, false, || {
                        ERR::ErrorBitFieldBounds(BITS, start, pos).into()
                    })?;
                    let end = super::calc_index(BITS, end, false, || {
                        ERR::ErrorBitFieldBounds(BITS, end, pos).into()
                    })?;

                    if end <= start {
                        (0, 0)
                    } else if end == BITS && start == 0 {
                        // -1 = all bits set
                        (0, -1)
                    } else {
                        (
                            start as u8,
                            // 2^bits - 1
                            (((2 as crate::UNSIGNED_INT).pow((end - start) as u32) - 1)
                                as crate::INT)
                                << start,
                        )
                    }
                } else if let Some(range) = idx.read_lock::<crate::InclusiveRange>() {
                    let start = *range.start();
                    let end = *range.end();

                    let start = super::calc_index(BITS, start, false, || {
                        ERR::ErrorBitFieldBounds(BITS, start, pos).into()
                    })?;
                    let end = super::calc_index(BITS, end, false, || {
                        ERR::ErrorBitFieldBounds(BITS, end, pos).into()
                    })?;

                    if end < start {
                        (0, 0)
                    } else if end == BITS - 1 && start == 0 {
                        // -1 = all bits set
                        (0, -1)
                    } else {
                        (
                            start as u8,
                            // 2^bits - 1
                            (((2 as crate::UNSIGNED_INT).pow((end - start + 1) as u32) - 1)
                                as crate::INT)
                                << start,
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
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, pos))?;

                const BITS: usize = std::mem::size_of::<crate::INT>() * 8;

                let bit = super::calc_index(BITS, index, true, || {
                    ERR::ErrorBitFieldBounds(BITS, index, pos).into()
                })?;

                let bit_value = (*value & (1 << bit)) != 0;

                Ok(Target::Bit {
                    source: target,
                    value: bit_value.into(),
                    bit: bit as u8,
                })
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Str(s, _, _)) => {
                // val_string[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, pos))?;

                let (ch, offset) = if index >= 0 {
                    let offset = index as usize;
                    (
                        s.chars().nth(offset).ok_or_else(|| {
                            let chars_len = s.chars().count();
                            ERR::ErrorStringBounds(chars_len, index, pos)
                        })?,
                        offset,
                    )
                } else if let Some(abs_index) = index.checked_abs() {
                    let offset = abs_index as usize;
                    (
                        // Count from end if negative
                        s.chars().rev().nth(offset - 1).ok_or_else(|| {
                            let chars_len = s.chars().count();
                            ERR::ErrorStringBounds(chars_len, index, pos)
                        })?,
                        offset,
                    )
                } else {
                    let chars_len = s.chars().count();
                    return Err(ERR::ErrorStringBounds(chars_len, index, pos).into());
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
                self.exec_fn_call(
                    global,
                    state,
                    lib,
                    crate::engine::FN_IDX_GET,
                    hash_get,
                    args,
                    true,
                    true,
                    Position::NONE,
                    None,
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
}
