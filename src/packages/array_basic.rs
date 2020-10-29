#![cfg(not(feature = "no_index"))]
#![allow(non_snake_case)]

use crate::def_package;
use crate::dynamic::Dynamic;
use crate::engine::Array;
use crate::fn_native::{FnPtr, NativeCallContext};
use crate::plugin::*;
use crate::result::EvalAltResult;
use crate::token::Position;
use crate::utils::ImmutableString;
use crate::INT;

#[cfg(not(feature = "no_object"))]
use crate::engine::Map;

use crate::stdlib::{any::TypeId, boxed::Box, cmp::Ordering, string::ToString};

pub type Unit = ();

macro_rules! gen_array_functions {
    ($root:ident => $($arg_type:ident),+ ) => {
        pub mod $root { $( pub mod $arg_type {
            use super::super::*;

            #[export_module]
            pub mod functions {
                #[rhai_fn(name = "push", name = "+=")]
                pub fn push(list: &mut Array, item: $arg_type) {
                    list.push(Dynamic::from(item));
                }

                pub fn insert(list: &mut Array, position: INT, item: $arg_type) {
                    if position <= 0 {
                        list.insert(0, Dynamic::from(item));
                    } else if (position as usize) >= list.len() - 1 {
                        push(list, item);
                    } else {
                        list.insert(position as usize, Dynamic::from(item));
                    }
                }

                #[rhai_fn(return_raw)]
                pub fn pad(_context: NativeCallContext, list: &mut Array, len: INT, item: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                    // Check if array will be over max size limit
                    #[cfg(not(feature = "unchecked"))]
                    if _context.engine().max_array_size() > 0 && len > 0 && (len as usize) > _context.engine().max_array_size() {
                        return EvalAltResult::ErrorDataTooLarge(
                            "Size of array".to_string(), _context.engine().max_array_size(), len as usize, Position::none(),
                        ).into();
                    }

                    if len > 0 && len as usize > list.len() {
                        list.resize(len as usize, Dynamic::from(item));
                    }

                    Ok(().into())
                }
            }
        })* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        combine_with_exported_module!($mod_name, "array_functions", $root::$arg_type::functions);
    )* }
}

def_package!(crate:BasicArrayPackage:"Basic array utilities.", lib, {
    reg_functions!(lib += basic; INT, bool, char, ImmutableString, FnPtr, Array, Unit);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += numbers; i8, u8, i16, u16, i32, i64, u32, u64);

        #[cfg(not(target_arch = "wasm32"))]
        reg_functions!(lib += num_128; i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    reg_functions!(lib += float; f32, f64);

    #[cfg(not(feature = "no_object"))]
    reg_functions!(lib += map; Map);

    // Merge in the module at the end to override `+=` for arrays
    combine_with_exported_module!(lib, "array", array_functions);

    // Register array iterator
    lib.set_iterable::<Array>();
});

#[export_module]
mod array_functions {
    #[rhai_fn(name = "len", get = "len")]
    pub fn len(list: &mut Array) -> INT {
        list.len() as INT
    }
    #[rhai_fn(name = "append", name = "+=")]
    pub fn append(x: &mut Array, y: Array) {
        x.extend(y);
    }
    #[rhai_fn(name = "+")]
    pub fn concat(mut x: Array, y: Array) -> Array {
        x.extend(y);
        x
    }
    pub fn pop(list: &mut Array) -> Dynamic {
        list.pop().unwrap_or_else(|| ().into())
    }
    pub fn shift(list: &mut Array) -> Dynamic {
        if list.is_empty() {
            ().into()
        } else {
            list.remove(0)
        }
    }
    pub fn remove(list: &mut Array, len: INT) -> Dynamic {
        if len < 0 || (len as usize) >= list.len() {
            ().into()
        } else {
            list.remove(len as usize)
        }
    }
    pub fn clear(list: &mut Array) {
        list.clear();
    }
    pub fn truncate(list: &mut Array, len: INT) {
        if len >= 0 {
            list.truncate(len as usize);
        } else {
            list.clear();
        }
    }
    pub fn chop(list: &mut Array, len: INT) {
        if len as usize >= list.len() {
        } else if len >= 0 {
            list.drain(0..list.len() - len as usize);
        } else {
            list.clear();
        }
    }
    pub fn reverse(list: &mut Array) {
        list.reverse();
    }
    pub fn splice(list: &mut Array, start: INT, len: INT, replace: Array) {
        let start = if start < 0 {
            0
        } else if start as usize >= list.len() {
            list.len() - 1
        } else {
            start as usize
        };

        let len = if len < 0 {
            0
        } else if len as usize > list.len() - start {
            list.len() - start
        } else {
            len as usize
        };

        list.splice(start..start + len, replace.into_iter());
    }
    pub fn extract(list: &mut Array, start: INT, len: INT) -> Array {
        let start = if start < 0 {
            0
        } else if start as usize >= list.len() {
            list.len() - 1
        } else {
            start as usize
        };

        let len = if len < 0 {
            0
        } else if len as usize > list.len() - start {
            list.len() - start
        } else {
            len as usize
        };

        list[start..start + len].iter().cloned().collect()
    }
    #[rhai_fn(name = "extract")]
    pub fn extract_tail(list: &mut Array, start: INT) -> Array {
        let start = if start < 0 {
            0
        } else if start as usize >= list.len() {
            list.len() - 1
        } else {
            start as usize
        };

        list[start..].iter().cloned().collect()
    }
    #[rhai_fn(return_raw)]
    pub fn map(
        context: NativeCallContext,
        list: &mut Array,
        mapper: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut array = Array::with_capacity(list.len());

        for (i, item) in list.iter().enumerate() {
            array.push(
                mapper
                    .call_dynamic(context, None, [item.clone()])
                    .or_else(|err| match *err {
                        EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                            if fn_sig.starts_with(mapper.fn_name()) =>
                        {
                            mapper.call_dynamic(context, None, [item.clone(), (i as INT).into()])
                        }
                        _ => Err(err),
                    })
                    .map_err(|err| {
                        Box::new(EvalAltResult::ErrorInFunctionCall(
                            "map".to_string(),
                            err,
                            Position::none(),
                        ))
                    })?,
            );
        }

        Ok(array.into())
    }
    #[rhai_fn(return_raw)]
    pub fn filter(
        context: NativeCallContext,
        list: &mut Array,
        filter: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut array = Array::with_capacity(list.len());

        for (i, item) in list.iter().enumerate() {
            if filter
                .call_dynamic(context, None, [item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_dynamic(context, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "filter".to_string(),
                        err,
                        Position::none(),
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                array.push(item.clone());
            }
        }

        Ok(array.into())
    }
    #[rhai_fn(return_raw)]
    pub fn some(
        context: NativeCallContext,
        list: &mut Array,
        filter: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        for (i, item) in list.iter().enumerate() {
            if filter
                .call_dynamic(context, None, [item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_dynamic(context, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "filter".to_string(),
                        err,
                        Position::none(),
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(true.into());
            }
        }

        Ok(false.into())
    }
    #[rhai_fn(return_raw)]
    pub fn all(
        context: NativeCallContext,
        list: &mut Array,
        filter: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        for (i, item) in list.iter().enumerate() {
            if !filter
                .call_dynamic(context, None, [item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_dynamic(context, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "filter".to_string(),
                        err,
                        Position::none(),
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(false.into());
            }
        }

        Ok(true.into())
    }
    #[rhai_fn(return_raw)]
    pub fn reduce(
        context: NativeCallContext,
        list: &mut Array,
        reducer: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut result: Dynamic = ().into();

        for (i, item) in list.iter().enumerate() {
            result = reducer
                .call_dynamic(context, None, [result.clone(), item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(reducer.fn_name()) =>
                    {
                        reducer.call_dynamic(
                            context,
                            None,
                            [result, item.clone(), (i as INT).into()],
                        )
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "reduce".to_string(),
                        err,
                        Position::none(),
                    ))
                })?;
        }

        Ok(result)
    }
    #[rhai_fn(name = "reduce", return_raw)]
    pub fn reduce_with_initial(
        context: NativeCallContext,
        list: &mut Array,
        reducer: FnPtr,
        initial: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut result = initial.call_dynamic(context, None, []).map_err(|err| {
            Box::new(EvalAltResult::ErrorInFunctionCall(
                "reduce".to_string(),
                err,
                Position::none(),
            ))
        })?;

        for (i, item) in list.iter().enumerate() {
            result = reducer
                .call_dynamic(context, None, [result.clone(), item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(reducer.fn_name()) =>
                    {
                        reducer.call_dynamic(
                            context,
                            None,
                            [result, item.clone(), (i as INT).into()],
                        )
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "reduce".to_string(),
                        err,
                        Position::none(),
                    ))
                })?;
        }

        Ok(result)
    }
    #[rhai_fn(return_raw)]
    pub fn reduce_rev(
        context: NativeCallContext,
        list: &mut Array,
        reducer: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut result: Dynamic = ().into();

        for (i, item) in list.iter().enumerate().rev() {
            result = reducer
                .call_dynamic(context, None, [result.clone(), item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(reducer.fn_name()) =>
                    {
                        reducer.call_dynamic(
                            context,
                            None,
                            [result, item.clone(), (i as INT).into()],
                        )
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "reduce".to_string(),
                        err,
                        Position::none(),
                    ))
                })?;
        }

        Ok(result)
    }
    #[rhai_fn(name = "reduce_rev", return_raw)]
    pub fn reduce_rev_with_initial(
        context: NativeCallContext,
        list: &mut Array,
        reducer: FnPtr,
        initial: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut result = initial.call_dynamic(context, None, []).map_err(|err| {
            Box::new(EvalAltResult::ErrorInFunctionCall(
                "reduce".to_string(),
                err,
                Position::none(),
            ))
        })?;

        for (i, item) in list.iter().enumerate().rev() {
            result = reducer
                .call_dynamic(context, None, [result.clone(), item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(reducer.fn_name()) =>
                    {
                        reducer.call_dynamic(
                            context,
                            None,
                            [result, item.clone(), (i as INT).into()],
                        )
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "reduce".to_string(),
                        err,
                        Position::none(),
                    ))
                })?;
        }

        Ok(result)
    }
    #[rhai_fn(return_raw)]
    pub fn sort(
        context: NativeCallContext,
        list: &mut Array,
        comparer: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        list.sort_by(|x, y| {
            comparer
                .call_dynamic(context, None, [x.clone(), y.clone()])
                .ok()
                .and_then(|v| v.as_int().ok())
                .map(|v| {
                    if v > 0 {
                        Ordering::Greater
                    } else if v < 0 {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
                .unwrap_or_else(|| {
                    let x_type_id = x.type_id();
                    let y_type_id = y.type_id();

                    if x_type_id > y_type_id {
                        Ordering::Greater
                    } else if x_type_id < y_type_id {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                })
        });

        Ok(().into())
    }
    #[rhai_fn(return_raw)]
    pub fn drain(
        context: NativeCallContext,
        list: &mut Array,
        filter: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut drained = Array::with_capacity(list.len());

        let mut i = list.len();

        while i > 0 {
            i -= 1;

            if filter
                .call_dynamic(context, None, [list[i].clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_dynamic(context, None, [list[i].clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "filter".to_string(),
                        err,
                        Position::none(),
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                drained.push(list.remove(i));
            }
        }

        Ok(drained.into())
    }
    #[rhai_fn(name = "drain")]
    pub fn drain_range(list: &mut Array, start: INT, len: INT) -> Array {
        let start = if start < 0 {
            0
        } else if start as usize >= list.len() {
            list.len() - 1
        } else {
            start as usize
        };

        let len = if len < 0 {
            0
        } else if len as usize > list.len() - start {
            list.len() - start
        } else {
            len as usize
        };

        list.drain(start..start + len - 1).collect()
    }
    #[rhai_fn(return_raw)]
    pub fn retain(
        context: NativeCallContext,
        list: &mut Array,
        filter: FnPtr,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut drained = Array::with_capacity(list.len());

        let mut i = list.len();

        while i > 0 {
            i -= 1;

            if !filter
                .call_dynamic(context, None, [list[i].clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_dynamic(context, None, [list[i].clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(EvalAltResult::ErrorInFunctionCall(
                        "filter".to_string(),
                        err,
                        Position::none(),
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                drained.push(list.remove(i));
            }
        }

        Ok(drained.into())
    }
    #[rhai_fn(name = "retain")]
    pub fn retain_range(list: &mut Array, start: INT, len: INT) -> Array {
        let start = if start < 0 {
            0
        } else if start as usize >= list.len() {
            list.len() - 1
        } else {
            start as usize
        };

        let len = if len < 0 {
            0
        } else if len as usize > list.len() - start {
            list.len() - start
        } else {
            len as usize
        };

        let mut drained = list.drain(start + len..).collect::<Array>();
        drained.extend(list.drain(..start));

        drained
    }
}

gen_array_functions!(basic => INT, bool, char, ImmutableString, FnPtr, Array, Unit);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_array_functions!(numbers => i8, u8, i16, u16, i32, i64, u32, u64);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_array_functions!(num_128 => i128, u128);

#[cfg(not(feature = "no_float"))]
gen_array_functions!(float => f32, f64);

#[cfg(not(feature = "no_object"))]
gen_array_functions!(map => Map);
