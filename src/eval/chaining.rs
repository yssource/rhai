//! Types to support chaining operations (i.e. indexing and dotting).
#![cfg(any(not(feature = "no_index"), not(feature = "no_object")))]

use super::{Caches, GlobalRuntimeState, Target};
use crate::ast::{ASTFlags, Expr, OpAssignment};
use crate::types::dynamic::Union;
use crate::{Dynamic, Engine, FnArgsVec, Module, Position, RhaiResult, RhaiResultOf, Scope, ERR};
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
            Expr::Index(..) => Self::Indexing,
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(..) => Self::Dotting,
            expr => unreachable!("Expr::Index or Expr::Dot expected but gets {:?}", expr),
        }
    }
}

impl Engine {
    /// Chain-evaluate a dot/index chain.
    /// [`Position`] in [`EvalAltResult`] may be [`NONE`][Position::NONE] and should be set afterwards.
    fn eval_dot_index_chain_helper(
        &self,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        target: &mut Target,
        root: (&str, Position),
        _parent: &Expr,
        rhs: &Expr,
        _parent_options: ASTFlags,
        idx_values: &mut FnArgsVec<Dynamic>,
        chain_type: ChainType,
        level: usize,
        new_val: Option<(Dynamic, OpAssignment)>,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        let is_ref_mut = target.is_ref();

        #[cfg(feature = "debugging")]
        let scope = &mut Scope::new();

        match chain_type {
            #[cfg(not(feature = "no_index"))]
            ChainType::Indexing => {
                // Check for existence with the null conditional operator
                if _parent_options.contains(ASTFlags::NEGATED) && target.is::<()>() {
                    return Ok((Dynamic::UNIT, false));
                }

                let pos = rhs.start_position();

                match rhs {
                    // xxx[idx].expr... | xxx[idx][expr]...
                    Expr::Dot(x, options, x_pos) | Expr::Index(x, options, x_pos)
                        if !_parent_options.contains(ASTFlags::BREAK) =>
                    {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, _parent, level)?;

                        let idx_val = idx_values.pop().unwrap();
                        let mut idx_val_for_setter = idx_val.clone();
                        let idx_pos = x.lhs.start_position();
                        let rhs_chain = rhs.into();

                        let (try_setter, result) = {
                            let mut obj = self.get_indexed_mut(
                                global, caches, lib, target, idx_val, idx_pos, false, true, level,
                            )?;
                            let is_obj_temp_val = obj.is_temp_value();
                            let obj_ptr = &mut obj;

                            match self.eval_dot_index_chain_helper(
                                global, caches, lib, this_ptr, obj_ptr, root, rhs, &x.rhs,
                                *options, idx_values, rhs_chain, level, new_val,
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
                            let idx = &mut idx_val_for_setter;
                            let new_val = &mut new_val;
                            self.call_indexer_set(
                                global, caches, lib, target, idx, new_val, is_ref_mut, level,
                            )
                            .or_else(|e| match *e {
                                ERR::ErrorIndexingType(..) => Ok((Dynamic::UNIT, false)),
                                _ => Err(e),
                            })?;
                        }

                        Ok(result)
                    }
                    // xxx[rhs] op= new_val
                    _ if new_val.is_some() => {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, _parent, level)?;

                        let (new_val, op_info) = new_val.expect("`Some`");
                        let idx_val = idx_values.pop().unwrap();
                        let mut idx_val2 = idx_val.clone();

                        let try_setter = match self.get_indexed_mut(
                            global, caches, lib, target, idx_val, pos, true, false, level,
                        ) {
                            // Indexed value is not a temp value - update directly
                            Ok(ref mut obj_ptr) => {
                                self.eval_op_assignment(
                                    global, caches, lib, op_info, obj_ptr, root, new_val, level,
                                )?;
                                #[cfg(not(feature = "unchecked"))]
                                self.check_data_size(obj_ptr, op_info.pos)?;
                                None
                            }
                            // Indexed value cannot be referenced - use indexer
                            #[cfg(not(feature = "no_index"))]
                            Err(err) if matches!(*err, ERR::ErrorIndexingType(..)) => Some(new_val),
                            // Any other error
                            Err(err) => return Err(err),
                        };

                        if let Some(mut new_val) = try_setter {
                            let idx = &mut idx_val2;

                            // Is this an op-assignment?
                            if op_info.is_op_assignment() {
                                let idx = &mut idx.clone();
                                // Call the index getter to get the current value
                                if let Ok(val) =
                                    self.call_indexer_get(global, caches, lib, target, idx, level)
                                {
                                    let mut res = val.into();
                                    // Run the op-assignment
                                    self.eval_op_assignment(
                                        global, caches, lib, op_info, &mut res, root, new_val,
                                        level,
                                    )?;
                                    // Replace new value
                                    new_val = res.take_or_clone();
                                    #[cfg(not(feature = "unchecked"))]
                                    self.check_data_size(&new_val, op_info.pos)?;
                                }
                            }

                            // Try to call index setter
                            let new_val = &mut new_val;
                            self.call_indexer_set(
                                global, caches, lib, target, idx, new_val, is_ref_mut, level,
                            )?;
                        }

                        Ok((Dynamic::UNIT, true))
                    }
                    // xxx[rhs]
                    _ => {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, _parent, level)?;

                        let idx_val = idx_values.pop().unwrap();

                        self.get_indexed_mut(
                            global, caches, lib, target, idx_val, pos, false, true, level,
                        )
                        .map(|v| (v.take_or_clone(), false))
                    }
                }
            }

            #[cfg(not(feature = "no_object"))]
            ChainType::Dotting => {
                // Check for existence with the Elvis operator
                if _parent_options.contains(ASTFlags::NEGATED) && target.is::<()>() {
                    return Ok((Dynamic::UNIT, false));
                }

                match rhs {
                    // xxx.fn_name(arg_expr_list)
                    Expr::MethodCall(x, pos) if !x.is_qualified() && new_val.is_none() => {
                        #[cfg(feature = "debugging")]
                        let reset_debugger =
                            self.run_debugger_with_reset(scope, global, lib, this_ptr, rhs, level)?;

                        let crate::ast::FnCallExpr {
                            name, hashes, args, ..
                        } = &**x;

                        let offset = idx_values.len() - args.len();
                        let call_args = &mut idx_values[offset..];
                        let pos1 = args.get(0).map_or(Position::NONE, Expr::position);

                        let result = self.make_method_call(
                            global, caches, lib, name, *hashes, target, call_args, pos1, *pos,
                            level,
                        );

                        idx_values.truncate(offset);

                        #[cfg(feature = "debugging")]
                        global.debugger.reset_status(reset_debugger);

                        result
                    }
                    // xxx.fn_name(...) = ???
                    Expr::MethodCall(..) if new_val.is_some() => {
                        unreachable!("method call cannot be assigned to")
                    }
                    // xxx.module::fn_name(...) - syntax error
                    Expr::MethodCall(..) => {
                        unreachable!("function call in dot chain should not be namespace-qualified")
                    }
                    // {xxx:map}.id op= ???
                    Expr::Property(x, pos) if target.is::<crate::Map>() && new_val.is_some() => {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, rhs, level)?;

                        let index = x.2.clone().into();
                        let (new_val, op_info) = new_val.expect("`Some`");
                        {
                            let val_target = &mut self.get_indexed_mut(
                                global, caches, lib, target, index, *pos, true, false, level,
                            )?;
                            self.eval_op_assignment(
                                global, caches, lib, op_info, val_target, root, new_val, level,
                            )?;
                        }
                        #[cfg(not(feature = "unchecked"))]
                        self.check_data_size(target.source(), op_info.pos)?;
                        Ok((Dynamic::UNIT, true))
                    }
                    // {xxx:map}.id
                    Expr::Property(x, pos) if target.is::<crate::Map>() => {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, rhs, level)?;

                        let index = x.2.clone().into();
                        let val = self.get_indexed_mut(
                            global, caches, lib, target, index, *pos, false, false, level,
                        )?;
                        Ok((val.take_or_clone(), false))
                    }
                    // xxx.id op= ???
                    Expr::Property(x, pos) if new_val.is_some() => {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, rhs, level)?;

                        let ((getter, hash_get), (setter, hash_set), name) = &**x;
                        let (mut new_val, op_info) = new_val.expect("`Some`");

                        if op_info.is_op_assignment() {
                            let args = &mut [target.as_mut()];
                            let (mut orig_val, ..) = self
                                .call_native_fn(
                                    global, caches, lib, getter, *hash_get, args, is_ref_mut,
                                    false, *pos, level,
                                )
                                .or_else(|err| match *err {
                                    // Try an indexer if property does not exist
                                    ERR::ErrorDotExpr(..) => {
                                        let mut prop = name.into();
                                        self.call_indexer_get(
                                            global, caches, lib, target, &mut prop, level,
                                        )
                                        .map(|r| (r, false))
                                        .map_err(|e| {
                                            match *e {
                                                ERR::ErrorIndexingType(..) => err,
                                                _ => e,
                                            }
                                        })
                                    }
                                    _ => Err(err),
                                })?;

                            {
                                let orig_val = &mut (&mut orig_val).into();

                                self.eval_op_assignment(
                                    global, caches, lib, op_info, orig_val, root, new_val, level,
                                )?;
                            }

                            new_val = orig_val;
                        }

                        let args = &mut [target.as_mut(), &mut new_val];
                        self.call_native_fn(
                            global, caches, lib, setter, *hash_set, args, is_ref_mut, false, *pos,
                            level,
                        )
                        .or_else(|err| match *err {
                            // Try an indexer if property does not exist
                            ERR::ErrorDotExpr(..) => {
                                let idx = &mut name.into();
                                let new_val = &mut new_val;
                                self.call_indexer_set(
                                    global, caches, lib, target, idx, new_val, is_ref_mut, level,
                                )
                                .map_err(|e| match *e {
                                    ERR::ErrorIndexingType(..) => err,
                                    _ => e,
                                })
                            }
                            _ => Err(err),
                        })
                    }
                    // xxx.id
                    Expr::Property(x, pos) => {
                        #[cfg(feature = "debugging")]
                        self.run_debugger(scope, global, lib, this_ptr, rhs, level)?;

                        let ((getter, hash_get), _, name) = &**x;
                        let args = &mut [target.as_mut()];
                        self.call_native_fn(
                            global, caches, lib, getter, *hash_get, args, is_ref_mut, false, *pos,
                            level,
                        )
                        .map_or_else(
                            |err| match *err {
                                // Try an indexer if property does not exist
                                ERR::ErrorDotExpr(..) => {
                                    let mut prop = name.into();
                                    self.call_indexer_get(
                                        global, caches, lib, target, &mut prop, level,
                                    )
                                    .map(|r| (r, false))
                                    .map_err(|e| match *e {
                                        ERR::ErrorIndexingType(..) => err,
                                        _ => e,
                                    })
                                }
                                _ => Err(err),
                            },
                            // Assume getters are always pure
                            |(v, ..)| Ok((v, false)),
                        )
                    }
                    // {xxx:map}.sub_lhs[expr] | {xxx:map}.sub_lhs.expr
                    Expr::Index(x, options, x_pos) | Expr::Dot(x, options, x_pos)
                        if target.is::<crate::Map>() =>
                    {
                        let _node = &x.lhs;

                        let val_target = &mut match x.lhs {
                            Expr::Property(ref p, pos) => {
                                #[cfg(feature = "debugging")]
                                self.run_debugger(scope, global, lib, this_ptr, _node, level)?;

                                let index = p.2.clone().into();
                                self.get_indexed_mut(
                                    global, caches, lib, target, index, pos, false, true, level,
                                )?
                            }
                            // {xxx:map}.fn_name(arg_expr_list)[expr] | {xxx:map}.fn_name(arg_expr_list).expr
                            Expr::MethodCall(ref x, pos) if !x.is_qualified() => {
                                #[cfg(feature = "debugging")]
                                let reset_debugger = self.run_debugger_with_reset(
                                    scope, global, lib, this_ptr, _node, level,
                                )?;

                                let crate::ast::FnCallExpr {
                                    name, hashes, args, ..
                                } = &**x;

                                let offset = idx_values.len() - args.len();
                                let call_args = &mut idx_values[offset..];
                                let pos1 = args.get(0).map_or(Position::NONE, Expr::position);

                                let result = self.make_method_call(
                                    global, caches, lib, name, *hashes, target, call_args, pos1,
                                    pos, level,
                                );

                                idx_values.truncate(offset);

                                #[cfg(feature = "debugging")]
                                global.debugger.reset_status(reset_debugger);

                                result?.0.into()
                            }
                            // {xxx:map}.module::fn_name(...) - syntax error
                            Expr::MethodCall(..) => unreachable!(
                                "function call in dot chain should not be namespace-qualified"
                            ),
                            // Others - syntax error
                            ref expr => unreachable!("invalid dot expression: {:?}", expr),
                        };
                        let rhs_chain = rhs.into();

                        self.eval_dot_index_chain_helper(
                            global, caches, lib, this_ptr, val_target, root, rhs, &x.rhs, *options,
                            idx_values, rhs_chain, level, new_val,
                        )
                        .map_err(|err| err.fill_position(*x_pos))
                    }
                    // xxx.sub_lhs[expr] | xxx.sub_lhs.expr
                    Expr::Index(x, options, x_pos) | Expr::Dot(x, options, x_pos) => {
                        let _node = &x.lhs;

                        match x.lhs {
                            // xxx.prop[expr] | xxx.prop.expr
                            Expr::Property(ref p, pos) => {
                                #[cfg(feature = "debugging")]
                                self.run_debugger(scope, global, lib, this_ptr, _node, level)?;

                                let ((getter, hash_get), (setter, hash_set), name) = &**p;
                                let rhs_chain = rhs.into();
                                let mut arg_values = [target.as_mut(), &mut Dynamic::UNIT.clone()];
                                let args = &mut arg_values[..1];

                                // Assume getters are always pure
                                let (mut val, ..) = self
                                    .call_native_fn(
                                        global, caches, lib, getter, *hash_get, args, is_ref_mut,
                                        false, pos, level,
                                    )
                                    .or_else(|err| match *err {
                                        // Try an indexer if property does not exist
                                        ERR::ErrorDotExpr(..) => {
                                            let mut prop = name.into();
                                            self.call_indexer_get(
                                                global, caches, lib, target, &mut prop, level,
                                            )
                                            .map(|r| (r, false))
                                            .map_err(
                                                |e| match *e {
                                                    ERR::ErrorIndexingType(..) => err,
                                                    _ => e,
                                                },
                                            )
                                        }
                                        _ => Err(err),
                                    })?;

                                let val = &mut (&mut val).into();

                                let (result, may_be_changed) = self
                                    .eval_dot_index_chain_helper(
                                        global, caches, lib, this_ptr, val, root, rhs, &x.rhs,
                                        *options, idx_values, rhs_chain, level, new_val,
                                    )
                                    .map_err(|err| err.fill_position(*x_pos))?;

                                // Feed the value back via a setter just in case it has been updated
                                if may_be_changed {
                                    // Re-use args because the first &mut parameter will not be consumed
                                    let mut arg_values = [target.as_mut(), val.as_mut()];
                                    let args = &mut arg_values;
                                    self.call_native_fn(
                                        global, caches, lib, setter, *hash_set, args, is_ref_mut,
                                        false, pos, level,
                                    )
                                    .or_else(
                                        |err| match *err {
                                            // Try an indexer if property does not exist
                                            ERR::ErrorDotExpr(..) => {
                                                let idx = &mut name.into();
                                                let new_val = val;
                                                self.call_indexer_set(
                                                    global, caches, lib, target, idx, new_val,
                                                    is_ref_mut, level,
                                                )
                                                .or_else(|e| match *e {
                                                    // If there is no setter, no need to feed it
                                                    // back because the property is read-only
                                                    ERR::ErrorIndexingType(..) => {
                                                        Ok((Dynamic::UNIT, false))
                                                    }
                                                    _ => Err(e),
                                                })
                                            }
                                            _ => Err(err),
                                        },
                                    )?;
                                }

                                Ok((result, may_be_changed))
                            }
                            // xxx.fn_name(arg_expr_list)[expr] | xxx.fn_name(arg_expr_list).expr
                            Expr::MethodCall(ref f, pos) if !f.is_qualified() => {
                                #[cfg(feature = "debugging")]
                                let reset_debugger = self.run_debugger_with_reset(
                                    scope, global, lib, this_ptr, _node, level,
                                )?;

                                let crate::ast::FnCallExpr {
                                    name, hashes, args, ..
                                } = &**f;
                                let rhs_chain = rhs.into();

                                let offset = idx_values.len() - args.len();
                                let call_args = &mut idx_values[offset..];
                                let pos1 = args.get(0).map_or(Position::NONE, Expr::position);

                                let result = self.make_method_call(
                                    global, caches, lib, name, *hashes, target, call_args, pos1,
                                    pos, level,
                                );

                                idx_values.truncate(offset);

                                #[cfg(feature = "debugging")]
                                global.debugger.reset_status(reset_debugger);

                                let (val, _) = &mut result?;
                                let val = &mut val.into();

                                self.eval_dot_index_chain_helper(
                                    global, caches, lib, this_ptr, val, root, rhs, &x.rhs,
                                    *options, idx_values, rhs_chain, level, new_val,
                                )
                                .map_err(|err| err.fill_position(pos))
                            }
                            // xxx.module::fn_name(...) - syntax error
                            Expr::MethodCall(..) => unreachable!(
                                "function call in dot chain should not be namespace-qualified"
                            ),
                            // Others - syntax error
                            ref expr => unreachable!("invalid dot expression: {:?}", expr),
                        }
                    }
                    // Syntax error
                    _ => Err(ERR::ErrorDotExpr("".into(), rhs.start_position()).into()),
                }
            }
        }
    }

    /// Evaluate a dot/index chain.
    pub(crate) fn eval_dot_index_chain(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
        new_val: Option<(Dynamic, OpAssignment)>,
    ) -> RhaiResult {
        let chain_type = ChainType::from(expr);
        let (crate::ast::BinaryExpr { lhs, rhs }, options, op_pos) = match expr {
            #[cfg(not(feature = "no_index"))]
            Expr::Index(x, options, pos) => (&**x, *options, *pos),
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(x, options, pos) => (&**x, *options, *pos),
            expr => unreachable!("Expr::Index or Expr::Dot expected but gets {:?}", expr),
        };

        let idx_values = &mut FnArgsVec::new_const();

        match rhs {
            // Short-circuit for simple property access: {expr}.prop
            #[cfg(not(feature = "no_object"))]
            Expr::Property(..) if chain_type == ChainType::Dotting => (),
            #[cfg(not(feature = "no_object"))]
            Expr::Property(..) => unreachable!("unexpected Expr::Property for indexing"),
            // Short-circuit for simple method call: {expr}.func()
            #[cfg(not(feature = "no_object"))]
            Expr::FnCall(x, ..) if chain_type == ChainType::Dotting && x.args.is_empty() => (),
            // Short-circuit for method call with all literal arguments: {expr}.func(1, 2, 3)
            #[cfg(not(feature = "no_object"))]
            Expr::FnCall(x, ..)
                if chain_type == ChainType::Dotting && x.args.iter().all(Expr::is_constant) =>
            {
                idx_values.extend(x.args.iter().map(|expr| expr.get_literal_value().unwrap()))
            }
            // Short-circuit for indexing with literal: {expr}[1]
            #[cfg(not(feature = "no_index"))]
            _ if chain_type == ChainType::Indexing && rhs.is_constant() => {
                idx_values.push(rhs.get_literal_value().unwrap())
            }
            // All other patterns - evaluate the arguments chain
            _ => {
                self.eval_dot_index_chain_arguments(
                    scope, global, caches, lib, this_ptr, rhs, options, chain_type, idx_values, 0,
                    level,
                )?;
            }
        }

        match lhs {
            // id.??? or id[???]
            Expr::Variable(x, .., var_pos) => {
                #[cfg(feature = "debugging")]
                self.run_debugger(scope, global, lib, this_ptr, lhs, level)?;

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, *var_pos)?;

                let (mut target, ..) =
                    self.search_namespace(scope, global, lib, this_ptr, lhs, level)?;

                let obj_ptr = &mut target;
                let root = (x.3.as_str(), *var_pos);

                self.eval_dot_index_chain_helper(
                    global, caches, lib, &mut None, obj_ptr, root, expr, rhs, options, idx_values,
                    chain_type, level, new_val,
                )
            }
            // {expr}.??? = ??? or {expr}[???] = ???
            _ if new_val.is_some() => unreachable!("cannot assign to an expression"),
            // {expr}.??? or {expr}[???]
            expr => {
                let value = self
                    .eval_expr(scope, global, caches, lib, this_ptr, expr, level)?
                    .flatten();
                let obj_ptr = &mut value.into();
                let root = ("", expr.start_position());

                self.eval_dot_index_chain_helper(
                    global, caches, lib, this_ptr, obj_ptr, root, expr, rhs, options, idx_values,
                    chain_type, level, new_val,
                )
            }
        }
        .map(|(v, ..)| v)
        .map_err(|err| err.fill_position(op_pos))
    }

    /// Evaluate a chain of indexes and store the results in a [`StaticVec`].
    /// [`StaticVec`] is used to avoid an allocation in the overwhelming cases of
    /// just a few levels of indexing.
    fn eval_dot_index_chain_arguments(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        parent_options: ASTFlags,
        _parent_chain_type: ChainType,
        idx_values: &mut FnArgsVec<Dynamic>,
        size: usize,
        level: usize,
    ) -> RhaiResultOf<()> {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, expr.position())?;

        match expr {
            #[cfg(not(feature = "no_object"))]
            Expr::MethodCall(x, ..)
                if _parent_chain_type == ChainType::Dotting && !x.is_qualified() =>
            {
                for arg_expr in &x.args {
                    idx_values.push(
                        self.get_arg_value(scope, global, caches, lib, this_ptr, arg_expr, level)?
                            .0
                            .flatten(),
                    );
                }
            }
            #[cfg(not(feature = "no_object"))]
            Expr::MethodCall(..) if _parent_chain_type == ChainType::Dotting => {
                unreachable!("function call in dot chain should not be namespace-qualified")
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Property(..) if _parent_chain_type == ChainType::Dotting => (),
            Expr::Property(..) => unreachable!("unexpected Expr::Property for indexing"),

            Expr::Index(x, options, ..) | Expr::Dot(x, options, ..)
                if !parent_options.contains(ASTFlags::BREAK) =>
            {
                let crate::ast::BinaryExpr { lhs, rhs, .. } = &**x;

                let mut _arg_values = FnArgsVec::new_const();

                // Evaluate in left-to-right order
                match lhs {
                    #[cfg(not(feature = "no_object"))]
                    Expr::Property(..) if _parent_chain_type == ChainType::Dotting => (),
                    Expr::Property(..) => unreachable!("unexpected Expr::Property for indexing"),

                    #[cfg(not(feature = "no_object"))]
                    Expr::MethodCall(x, ..)
                        if _parent_chain_type == ChainType::Dotting && !x.is_qualified() =>
                    {
                        for arg_expr in &x.args {
                            _arg_values.push(
                                self.get_arg_value(
                                    scope, global, caches, lib, this_ptr, arg_expr, level,
                                )?
                                .0
                                .flatten(),
                            );
                        }
                    }
                    #[cfg(not(feature = "no_object"))]
                    Expr::MethodCall(..) if _parent_chain_type == ChainType::Dotting => {
                        unreachable!("function call in dot chain should not be namespace-qualified")
                    }
                    #[cfg(not(feature = "no_object"))]
                    expr if _parent_chain_type == ChainType::Dotting => {
                        unreachable!("invalid dot expression: {:?}", expr);
                    }
                    #[cfg(not(feature = "no_index"))]
                    _ if _parent_chain_type == ChainType::Indexing => {
                        _arg_values.push(
                            self.eval_expr(scope, global, caches, lib, this_ptr, lhs, level)?
                                .flatten(),
                        );
                    }
                    expr => unreachable!("unknown chained expression: {:?}", expr),
                }

                // Push in reverse order
                let chain_type = expr.into();

                self.eval_dot_index_chain_arguments(
                    scope, global, caches, lib, this_ptr, rhs, *options, chain_type, idx_values,
                    size, level,
                )?;

                if !_arg_values.is_empty() {
                    idx_values.extend(_arg_values);
                }
            }

            #[cfg(not(feature = "no_object"))]
            _ if _parent_chain_type == ChainType::Dotting => {
                unreachable!("invalid dot expression: {:?}", expr);
            }
            #[cfg(not(feature = "no_index"))]
            _ if _parent_chain_type == ChainType::Indexing => idx_values.push(
                self.eval_expr(scope, global, caches, lib, this_ptr, expr, level)?
                    .flatten(),
            ),
            _ => unreachable!("unknown chained expression: {:?}", expr),
        }

        Ok(())
    }

    /// Call a get indexer.
    #[inline(always)]
    fn call_indexer_get(
        &self,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        target: &mut Dynamic,
        idx: &mut Dynamic,
        level: usize,
    ) -> RhaiResultOf<Dynamic> {
        let args = &mut [target, idx];
        let hash = global.hash_idx_get();
        let fn_name = crate::engine::FN_IDX_GET;
        let pos = Position::NONE;
        let level = level + 1;

        self.call_native_fn(
            global, caches, lib, fn_name, hash, args, true, false, pos, level,
        )
        .map(|(r, ..)| r)
    }

    /// Call a set indexer.
    #[inline(always)]
    fn call_indexer_set(
        &self,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        target: &mut Dynamic,
        idx: &mut Dynamic,
        new_val: &mut Dynamic,
        is_ref_mut: bool,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        let hash = global.hash_idx_set();
        let args = &mut [target, idx, new_val];
        let fn_name = crate::engine::FN_IDX_SET;
        let pos = Position::NONE;
        let level = level + 1;

        self.call_native_fn(
            global, caches, lib, fn_name, hash, args, is_ref_mut, false, pos, level,
        )
    }

    /// Get the value at the indexed position of a base type.
    /// [`Position`] in [`EvalAltResult`] may be [`NONE`][Position::NONE] and should be set afterwards.
    fn get_indexed_mut<'t>(
        &self,
        global: &mut GlobalRuntimeState,
        caches: &mut Caches,
        lib: &[&Module],
        target: &'t mut Dynamic,
        mut idx: Dynamic,
        idx_pos: Position,
        _add_if_not_found: bool,
        use_indexers: bool,
        level: usize,
    ) -> RhaiResultOf<Target<'t>> {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, Position::NONE)?;

        match target {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(arr, ..)) => {
                // val_array[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;
                let len = arr.len();
                let arr_idx = super::calc_index(len, index, true, || {
                    ERR::ErrorArrayBounds(len, index, idx_pos).into()
                })?;

                Ok(arr.get_mut(arr_idx).map(Target::from).unwrap())
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Blob(arr, ..)) => {
                // val_blob[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;
                let len = arr.len();
                let arr_idx = super::calc_index(len, index, true, || {
                    ERR::ErrorArrayBounds(len, index, idx_pos).into()
                })?;

                let value = arr.get(arr_idx).map(|&v| (v as crate::INT).into()).unwrap();

                Ok(Target::BlobByte {
                    source: target,
                    value,
                    index: arr_idx,
                })
            }

            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(map, ..)) => {
                // val_map[idx]
                let index = idx.read_lock::<crate::ImmutableString>().ok_or_else(|| {
                    self.make_type_mismatch_err::<crate::ImmutableString>(idx.type_name(), idx_pos)
                })?;

                if _add_if_not_found && (map.is_empty() || !map.contains_key(index.as_str())) {
                    map.insert(index.clone().into(), Dynamic::UNIT);
                }

                if let Some(value) = map.get_mut(index.as_str()) {
                    Ok(Target::from(value))
                } else if self.fail_on_invalid_map_property() {
                    Err(ERR::ErrorPropertyNotFound(index.to_string(), idx_pos).into())
                } else {
                    Ok(Target::from(Dynamic::UNIT))
                }
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Int(value, ..))
                if idx.is::<crate::ExclusiveRange>() || idx.is::<crate::InclusiveRange>() =>
            {
                // val_int[range]
                let (shift, mask) = if let Some(range) = idx.read_lock::<crate::ExclusiveRange>() {
                    let start = range.start;
                    let end = range.end;

                    let start = super::calc_index(crate::INT_BITS, start, false, || {
                        ERR::ErrorBitFieldBounds(crate::INT_BITS, start, idx_pos).into()
                    })?;
                    let end = super::calc_index(crate::INT_BITS, end, false, || {
                        ERR::ErrorBitFieldBounds(crate::INT_BITS, end, idx_pos).into()
                    })?;

                    if end <= start {
                        (0, 0)
                    } else if end == crate::INT_BITS && start == 0 {
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

                    let start = super::calc_index(crate::INT_BITS, start, false, || {
                        ERR::ErrorBitFieldBounds(crate::INT_BITS, start, idx_pos).into()
                    })?;
                    let end = super::calc_index(crate::INT_BITS, end, false, || {
                        ERR::ErrorBitFieldBounds(crate::INT_BITS, end, idx_pos).into()
                    })?;

                    if end < start {
                        (0, 0)
                    } else if end == crate::INT_BITS - 1 && start == 0 {
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
            Dynamic(Union::Int(value, ..)) => {
                // val_int[idx]
                let index = idx
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, idx_pos))?;

                let bit = super::calc_index(crate::INT_BITS, index, true, || {
                    ERR::ErrorBitFieldBounds(crate::INT_BITS, index, idx_pos).into()
                })?;

                let bit_value = (*value & (1 << bit)) != 0;

                Ok(Target::Bit {
                    source: target,
                    value: bit_value.into(),
                    bit: bit as u8,
                })
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Str(s, ..)) => {
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
                } else {
                    let offset = index.unsigned_abs() as usize;
                    (
                        // Count from end if negative
                        s.chars().rev().nth(offset - 1).ok_or_else(|| {
                            let chars_len = s.chars().count();
                            ERR::ErrorStringBounds(chars_len, index, idx_pos)
                        })?,
                        offset,
                    )
                };

                Ok(Target::StringChar {
                    source: target,
                    value: ch.into(),
                    index: offset,
                })
            }

            #[cfg(not(feature = "no_closure"))]
            Dynamic(Union::Shared(..)) => {
                unreachable!("`get_indexed_mut` cannot handle shared values")
            }

            _ if use_indexers => self
                .call_indexer_get(global, caches, lib, target, &mut idx, level)
                .map(Into::into),

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
