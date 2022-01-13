#![cfg(not(feature = "no_index"))]
#![allow(non_snake_case)]

use crate::engine::OP_EQUALS;
use crate::eval::calc_offset_len;
use crate::plugin::*;
use crate::{
    def_package, Array, Dynamic, ExclusiveRange, FnPtr, InclusiveRange, NativeCallContext,
    Position, RhaiResultOf, ERR, INT,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{any::TypeId, cmp::Ordering, mem};

def_package! {
    /// Package of basic array utilities.
    crate::BasicArrayPackage => |lib| {
        lib.standard = true;

        combine_with_exported_module!(lib, "array", array_functions);

        // Register array iterator
        lib.set_iterable::<Array>();
    }
}

#[export_module]
pub mod array_functions {
    #[rhai_fn(name = "len", get = "len", pure)]
    pub fn len(array: &mut Array) -> INT {
        array.len() as INT
    }
    pub fn push(array: &mut Array, item: Dynamic) {
        array.push(item);
    }
    pub fn append(array1: &mut Array, array2: Array) {
        if !array2.is_empty() {
            if array1.is_empty() {
                *array1 = array2;
            } else {
                array1.extend(array2);
            }
        }
    }
    #[rhai_fn(name = "+")]
    pub fn concat(array1: Array, array2: Array) -> Array {
        if !array2.is_empty() {
            if array1.is_empty() {
                array2
            } else {
                let mut array = array1;
                array.extend(array2);
                array
            }
        } else {
            array1
        }
    }
    pub fn insert(array: &mut Array, index: INT, item: Dynamic) {
        if array.is_empty() {
            array.push(item);
            return;
        }

        let (index, _) = calc_offset_len(array.len(), index, 0);

        if index >= array.len() {
            array.push(item);
        } else {
            array.insert(index, item);
        }
    }
    #[rhai_fn(return_raw)]
    pub fn pad(
        ctx: NativeCallContext,
        array: &mut Array,
        len: INT,
        item: Dynamic,
    ) -> RhaiResultOf<()> {
        if len <= 0 || (len as usize) <= array.len() {
            return Ok(());
        }

        let _ctx = ctx;
        let len = len as usize;

        // Check if array will be over max size limit
        #[cfg(not(feature = "unchecked"))]
        if _ctx.engine().max_array_size() > 0 && len > _ctx.engine().max_array_size() {
            return Err(ERR::ErrorDataTooLarge("Size of array".to_string(), Position::NONE).into());
        }

        #[cfg(not(feature = "unchecked"))]
        let check_sizes = match item.0 {
            crate::types::dynamic::Union::Array(_, _, _)
            | crate::types::dynamic::Union::Str(_, _, _) => true,
            #[cfg(not(feature = "no_object"))]
            crate::types::dynamic::Union::Map(_, _, _) => true,
            _ => false,
        };
        #[cfg(feature = "unchecked")]
        let check_sizes = false;

        if check_sizes {
            let mut arr_len = array.len();
            let mut arr = Dynamic::from_array(mem::take(array));

            #[cfg(not(feature = "unchecked"))]
            let (mut a1, mut m1, mut s1) = Engine::calc_data_sizes(&arr, true);
            #[cfg(not(feature = "unchecked"))]
            let (a2, m2, s2) = Engine::calc_data_sizes(&item, true);

            {
                let mut guard = arr.write_lock::<Array>().unwrap();

                while arr_len < len {
                    #[cfg(not(feature = "unchecked"))]
                    {
                        a1 += a2;
                        m1 += m2;
                        s1 += s2;

                        _ctx.engine()
                            .raise_err_if_over_data_size_limit((a1, m1, s1), Position::NONE)?;
                    }
                    guard.push(item.clone());
                    arr_len += 1;
                }
            }

            *array = arr.into_array().unwrap();
        } else {
            array.resize(len, item);
        }

        Ok(())
    }
    pub fn pop(array: &mut Array) -> Dynamic {
        if array.is_empty() {
            Dynamic::UNIT
        } else {
            array.pop().unwrap_or_else(|| Dynamic::UNIT)
        }
    }
    pub fn shift(array: &mut Array) -> Dynamic {
        if array.is_empty() {
            Dynamic::UNIT
        } else {
            array.remove(0)
        }
    }
    pub fn remove(array: &mut Array, index: INT) -> Dynamic {
        if index < 0 || (index as usize) >= array.len() {
            Dynamic::UNIT
        } else {
            array.remove(index as usize)
        }
    }
    pub fn clear(array: &mut Array) {
        if !array.is_empty() {
            array.clear();
        }
    }
    pub fn truncate(array: &mut Array, len: INT) {
        if !array.is_empty() {
            if len >= 0 {
                array.truncate(len as usize);
            } else {
                array.clear();
            }
        }
    }
    pub fn chop(array: &mut Array, len: INT) {
        if !array.is_empty() && len as usize >= array.len() {
            if len >= 0 {
                array.drain(0..array.len() - len as usize);
            } else {
                array.clear();
            }
        }
    }
    pub fn reverse(array: &mut Array) {
        if !array.is_empty() {
            array.reverse();
        }
    }
    #[rhai_fn(name = "splice")]
    pub fn splice_range(array: &mut Array, range: ExclusiveRange, replace: Array) {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        splice(array, start, end - start, replace)
    }
    #[rhai_fn(name = "splice")]
    pub fn splice_inclusive_range(array: &mut Array, range: InclusiveRange, replace: Array) {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        splice(array, start, end - start + 1, replace)
    }
    pub fn splice(array: &mut Array, start: INT, len: INT, replace: Array) {
        if array.is_empty() {
            *array = replace;
            return;
        }

        let (start, len) = calc_offset_len(array.len(), start, len);

        if start >= array.len() {
            array.extend(replace);
        } else {
            array.splice(start..start + len, replace);
        }
    }
    #[rhai_fn(name = "extract")]
    pub fn extract_range(array: &mut Array, range: ExclusiveRange) -> Array {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        extract(array, start, end - start)
    }
    #[rhai_fn(name = "extract")]
    pub fn extract_inclusive_range(array: &mut Array, range: InclusiveRange) -> Array {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        extract(array, start, end - start + 1)
    }
    pub fn extract(array: &mut Array, start: INT, len: INT) -> Array {
        if array.is_empty() || len <= 0 {
            return Array::new();
        }

        let (start, len) = calc_offset_len(array.len(), start, len);

        if len == 0 {
            Array::new()
        } else {
            array[start..start + len].to_vec()
        }
    }
    #[rhai_fn(name = "extract")]
    pub fn extract_tail(array: &mut Array, start: INT) -> Array {
        extract(array, start, INT::MAX)
    }
    #[rhai_fn(name = "split")]
    pub fn split_at(array: &mut Array, start: INT) -> Array {
        if array.is_empty() {
            return Array::new();
        }

        let (start, len) = calc_offset_len(array.len(), start, INT::MAX);

        if start == 0 {
            if len >= array.len() {
                mem::take(array)
            } else {
                let mut result = Array::new();
                result.extend(array.drain(array.len() - len..));
                result
            }
        } else if start >= array.len() {
            Array::new()
        } else {
            let mut result = Array::new();
            result.extend(array.drain(start as usize..));
            result
        }
    }
    #[rhai_fn(return_raw, pure)]
    pub fn map(ctx: NativeCallContext, array: &mut Array, mapper: FnPtr) -> RhaiResultOf<Array> {
        if array.is_empty() {
            return Ok(array.clone());
        }

        let mut ar = Array::with_capacity(array.len());

        for (i, item) in array.iter().enumerate() {
            ar.push(
                mapper
                    .call_raw(&ctx, None, [item.clone()])
                    .or_else(|err| match *err {
                        ERR::ErrorFunctionNotFound(fn_sig, _)
                            if fn_sig.starts_with(mapper.fn_name()) =>
                        {
                            mapper.call_raw(&ctx, None, [item.clone(), (i as INT).into()])
                        }
                        _ => Err(err),
                    })
                    .map_err(|err| {
                        Box::new(ERR::ErrorInFunctionCall(
                            "map".to_string(),
                            ctx.source().unwrap_or("").to_string(),
                            err,
                            Position::NONE,
                        ))
                    })?,
            );
        }

        Ok(ar)
    }
    #[rhai_fn(name = "map", return_raw, pure)]
    pub fn map_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        mapper: &str,
    ) -> RhaiResultOf<Array> {
        map(ctx, array, FnPtr::new(mapper)?)
    }

    #[rhai_fn(return_raw, pure)]
    pub fn filter(ctx: NativeCallContext, array: &mut Array, filter: FnPtr) -> RhaiResultOf<Array> {
        if array.is_empty() {
            return Ok(array.clone());
        }

        let mut ar = Array::new();

        for (i, item) in array.iter().enumerate() {
            if filter
                .call_raw(&ctx, None, [item.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_raw(&ctx, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "filter".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                ar.push(item.clone());
            }
        }

        Ok(ar)
    }
    #[rhai_fn(name = "filter", return_raw, pure)]
    pub fn filter_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        filter_func: &str,
    ) -> RhaiResultOf<Array> {
        filter(ctx, array, FnPtr::new(filter_func)?)
    }
    #[rhai_fn(return_raw, pure)]
    pub fn contains(
        ctx: NativeCallContext,
        array: &mut Array,
        value: Dynamic,
    ) -> RhaiResultOf<bool> {
        if array.is_empty() {
            return Ok(false);
        }

        for item in array.iter_mut() {
            if ctx
                .call_fn_raw(OP_EQUALS, true, false, &mut [item, &mut value.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(ref fn_sig, _) if fn_sig.starts_with(OP_EQUALS) => {
                        if item.type_id() == value.type_id() {
                            // No default when comparing same type
                            Err(err)
                        } else {
                            Ok(Dynamic::FALSE)
                        }
                    }
                    _ => Err(err),
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(true);
            }
        }

        Ok(false)
    }
    #[rhai_fn(return_raw, pure)]
    pub fn index_of(
        ctx: NativeCallContext,
        array: &mut Array,
        value: Dynamic,
    ) -> RhaiResultOf<INT> {
        if array.is_empty() {
            Ok(-1)
        } else {
            index_of_starting_from(ctx, array, value, 0)
        }
    }
    #[rhai_fn(name = "index_of", return_raw, pure)]
    pub fn index_of_starting_from(
        ctx: NativeCallContext,
        array: &mut Array,
        value: Dynamic,
        start: INT,
    ) -> RhaiResultOf<INT> {
        if array.is_empty() {
            return Ok(-1);
        }

        let (start, _) = calc_offset_len(array.len(), start, 0);

        for (i, item) in array.iter_mut().enumerate().skip(start) {
            if ctx
                .call_fn_raw(OP_EQUALS, true, false, &mut [item, &mut value.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(ref fn_sig, _) if fn_sig.starts_with(OP_EQUALS) => {
                        if item.type_id() == value.type_id() {
                            // No default when comparing same type
                            Err(err)
                        } else {
                            Ok(Dynamic::FALSE)
                        }
                    }
                    _ => Err(err),
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(i as INT);
            }
        }

        Ok(-1 as INT)
    }
    #[rhai_fn(name = "index_of", return_raw, pure)]
    pub fn index_of_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: &str,
    ) -> RhaiResultOf<INT> {
        index_of_filter(ctx, array, FnPtr::new(filter)?)
    }
    #[rhai_fn(name = "index_of", return_raw, pure)]
    pub fn index_of_filter(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: FnPtr,
    ) -> RhaiResultOf<INT> {
        if array.is_empty() {
            Ok(-1)
        } else {
            index_of_filter_starting_from(ctx, array, filter, 0)
        }
    }
    #[rhai_fn(name = "index_of", return_raw, pure)]
    pub fn index_of_filter_starting_from(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: FnPtr,
        start: INT,
    ) -> RhaiResultOf<INT> {
        if array.is_empty() {
            return Ok(-1);
        }

        let (start, _) = calc_offset_len(array.len(), start, 0);

        for (i, item) in array.iter().enumerate().skip(start) {
            if filter
                .call_raw(&ctx, None, [item.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_raw(&ctx, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "index_of".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(i as INT);
            }
        }

        Ok(-1 as INT)
    }
    #[rhai_fn(name = "index_of", return_raw, pure)]
    pub fn index_of_with_fn_name_filter_starting_from(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: &str,
        start: INT,
    ) -> RhaiResultOf<INT> {
        index_of_filter_starting_from(ctx, array, FnPtr::new(filter)?, start)
    }
    #[rhai_fn(return_raw, pure)]
    pub fn some(ctx: NativeCallContext, array: &mut Array, filter: FnPtr) -> RhaiResultOf<bool> {
        if array.is_empty() {
            return Ok(false);
        }

        for (i, item) in array.iter().enumerate() {
            if filter
                .call_raw(&ctx, None, [item.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_raw(&ctx, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "some".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(true);
            }
        }

        Ok(false)
    }
    #[rhai_fn(name = "some", return_raw, pure)]
    pub fn some_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: &str,
    ) -> RhaiResultOf<bool> {
        some(ctx, array, FnPtr::new(filter)?)
    }
    #[rhai_fn(return_raw, pure)]
    pub fn all(ctx: NativeCallContext, array: &mut Array, filter: FnPtr) -> RhaiResultOf<bool> {
        if array.is_empty() {
            return Ok(true);
        }

        for (i, item) in array.iter().enumerate() {
            if !filter
                .call_raw(&ctx, None, [item.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_raw(&ctx, None, [item.clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "all".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(false);
            }
        }

        Ok(true)
    }
    #[rhai_fn(name = "all", return_raw, pure)]
    pub fn all_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: &str,
    ) -> RhaiResultOf<bool> {
        all(ctx, array, FnPtr::new(filter)?)
    }
    #[rhai_fn(return_raw)]
    pub fn dedup(ctx: NativeCallContext, array: &mut Array) -> RhaiResultOf<()> {
        dedup_with_fn_name(ctx, array, OP_EQUALS)
    }
    #[rhai_fn(name = "dedup", return_raw)]
    pub fn dedup_by_comparer(
        ctx: NativeCallContext,
        array: &mut Array,
        comparer: FnPtr,
    ) -> RhaiResultOf<()> {
        if array.is_empty() {
            return Ok(());
        }

        array.dedup_by(|x, y| {
            comparer
                .call_raw(&ctx, None, [x.clone(), y.clone()])
                .unwrap_or_else(|_| Dynamic::FALSE)
                .as_bool()
                .unwrap_or(false)
        });

        Ok(())
    }
    #[rhai_fn(name = "dedup", return_raw)]
    fn dedup_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        comparer: &str,
    ) -> RhaiResultOf<()> {
        dedup_by_comparer(ctx, array, FnPtr::new(comparer)?)
    }
    #[rhai_fn(return_raw, pure)]
    pub fn reduce(ctx: NativeCallContext, array: &mut Array, reducer: FnPtr) -> RhaiResult {
        reduce_with_initial(ctx, array, reducer, Dynamic::UNIT)
    }
    #[rhai_fn(name = "reduce", return_raw, pure)]
    pub fn reduce_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        reducer: &str,
    ) -> RhaiResult {
        reduce(ctx, array, FnPtr::new(reducer)?)
    }
    #[rhai_fn(name = "reduce", return_raw, pure)]
    pub fn reduce_with_initial(
        ctx: NativeCallContext,
        array: &mut Array,
        reducer: FnPtr,
        initial: Dynamic,
    ) -> RhaiResult {
        if array.is_empty() {
            return Ok(initial);
        }

        let mut result = initial;

        for (i, item) in array.iter().enumerate() {
            let item = item.clone();

            result = reducer
                .call_raw(&ctx, None, [result.clone(), item.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(reducer.fn_name()) =>
                    {
                        reducer.call_raw(&ctx, None, [result, item, (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "reduce".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?;
        }

        Ok(result)
    }
    #[rhai_fn(name = "reduce", return_raw, pure)]
    pub fn reduce_with_fn_name_with_initial(
        ctx: NativeCallContext,
        array: &mut Array,
        reducer: &str,
        initial: Dynamic,
    ) -> RhaiResult {
        reduce_with_initial(ctx, array, FnPtr::new(reducer)?, initial)
    }
    #[rhai_fn(return_raw, pure)]
    pub fn reduce_rev(ctx: NativeCallContext, array: &mut Array, reducer: FnPtr) -> RhaiResult {
        reduce_rev_with_initial(ctx, array, reducer, Dynamic::UNIT)
    }
    #[rhai_fn(name = "reduce_rev", return_raw, pure)]
    pub fn reduce_rev_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        reducer: &str,
    ) -> RhaiResult {
        reduce_rev(ctx, array, FnPtr::new(reducer)?)
    }
    #[rhai_fn(name = "reduce_rev", return_raw, pure)]
    pub fn reduce_rev_with_initial(
        ctx: NativeCallContext,
        array: &mut Array,
        reducer: FnPtr,
        initial: Dynamic,
    ) -> RhaiResult {
        if array.is_empty() {
            return Ok(initial);
        }

        let mut result = initial;
        let len = array.len();

        for (i, item) in array.iter().rev().enumerate() {
            let item = item.clone();

            result = reducer
                .call_raw(&ctx, None, [result.clone(), item.clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(reducer.fn_name()) =>
                    {
                        reducer.call_raw(&ctx, None, [result, item, ((len - 1 - i) as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "reduce_rev".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?;
        }

        Ok(result)
    }
    #[rhai_fn(name = "reduce_rev", return_raw, pure)]
    pub fn reduce_rev_with_fn_name_with_initial(
        ctx: NativeCallContext,
        array: &mut Array,
        reducer: &str,
        initial: Dynamic,
    ) -> RhaiResult {
        reduce_rev_with_initial(ctx, array, FnPtr::new(reducer)?, initial)
    }
    #[rhai_fn(name = "sort", return_raw)]
    pub fn sort_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        comparer: &str,
    ) -> RhaiResultOf<()> {
        sort(ctx, array, FnPtr::new(comparer)?)
    }
    #[rhai_fn(return_raw)]
    pub fn sort(ctx: NativeCallContext, array: &mut Array, comparer: FnPtr) -> RhaiResultOf<()> {
        if array.len() <= 1 {
            return Ok(());
        }

        array.sort_by(|x, y| {
            comparer
                .call_raw(&ctx, None, [x.clone(), y.clone()])
                .ok()
                .and_then(|v| v.as_int().ok())
                .map(|v| match v {
                    v if v > 0 => Ordering::Greater,
                    v if v < 0 => Ordering::Less,
                    0 => Ordering::Equal,
                    _ => unreachable!("v is {}", v),
                })
                .unwrap_or_else(|| x.type_id().cmp(&y.type_id()))
        });

        Ok(())
    }
    #[rhai_fn(name = "sort", return_raw)]
    pub fn sort_with_builtin(array: &mut Array) -> RhaiResultOf<()> {
        if array.len() <= 1 {
            return Ok(());
        }

        let type_id = array[0].type_id();

        if array.iter().any(|a| a.type_id() != type_id) {
            return Err(ERR::ErrorFunctionNotFound(
                "sort() cannot be called with elements of different types".into(),
                Position::NONE,
            )
            .into());
        }

        if type_id == TypeId::of::<INT>() {
            array.sort_by(|a, b| {
                let a = a.as_int().expect("`INT`");
                let b = b.as_int().expect("`INT`");
                a.cmp(&b)
            });
            return Ok(());
        }
        if type_id == TypeId::of::<char>() {
            array.sort_by(|a, b| {
                let a = a.as_char().expect("char");
                let b = b.as_char().expect("char");
                a.cmp(&b)
            });
            return Ok(());
        }
        #[cfg(not(feature = "no_float"))]
        if type_id == TypeId::of::<crate::FLOAT>() {
            array.sort_by(|a, b| {
                let a = a.as_float().expect("`FLOAT`");
                let b = b.as_float().expect("`FLOAT`");
                a.partial_cmp(&b).unwrap_or(Ordering::Equal)
            });
            return Ok(());
        }
        if type_id == TypeId::of::<ImmutableString>() {
            array.sort_by(|a, b| {
                let a = a.read_lock::<ImmutableString>().expect("`ImmutableString`");
                let b = b.read_lock::<ImmutableString>().expect("`ImmutableString`");
                a.as_str().cmp(b.as_str())
            });
            return Ok(());
        }
        #[cfg(feature = "decimal")]
        if type_id == TypeId::of::<rust_decimal::Decimal>() {
            array.sort_by(|a, b| {
                let a = a.as_decimal().expect("`Decimal`");
                let b = b.as_decimal().expect("`Decimal`");
                a.cmp(&b)
            });
            return Ok(());
        }
        if type_id == TypeId::of::<bool>() {
            array.sort_by(|a, b| {
                let a = a.as_bool().expect("`bool`");
                let b = b.as_bool().expect("`bool`");
                a.cmp(&b)
            });
            return Ok(());
        }
        if type_id == TypeId::of::<()>() {
            return Ok(());
        }

        Ok(())
    }
    #[rhai_fn(return_raw)]
    pub fn drain(ctx: NativeCallContext, array: &mut Array, filter: FnPtr) -> RhaiResultOf<Array> {
        if array.is_empty() {
            return Ok(Array::new());
        }

        let mut drained = Array::with_capacity(array.len());

        let mut i = 0;
        let mut x = 0;

        while x < array.len() {
            if filter
                .call_raw(&ctx, None, [array[x].clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_raw(&ctx, None, [array[x].clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "drain".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                drained.push(array.remove(x));
            } else {
                x += 1;
            }

            i += 1;
        }

        Ok(drained)
    }
    #[rhai_fn(name = "drain", return_raw)]
    pub fn drain_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: &str,
    ) -> RhaiResultOf<Array> {
        drain(ctx, array, FnPtr::new(filter)?)
    }
    #[rhai_fn(name = "drain")]
    pub fn drain_exclusive_range(array: &mut Array, range: ExclusiveRange) -> Array {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        drain_range(array, start, end - start)
    }
    #[rhai_fn(name = "drain")]
    pub fn drain_inclusive_range(array: &mut Array, range: InclusiveRange) -> Array {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        drain_range(array, start, end - start + 1)
    }
    #[rhai_fn(name = "drain")]
    pub fn drain_range(array: &mut Array, start: INT, len: INT) -> Array {
        if array.is_empty() || len <= 0 {
            return Array::new();
        }

        let (start, len) = calc_offset_len(array.len(), start, len);

        if len == 0 {
            Array::new()
        } else {
            array.drain(start..start + len).collect()
        }
    }
    #[rhai_fn(return_raw)]
    pub fn retain(ctx: NativeCallContext, array: &mut Array, filter: FnPtr) -> RhaiResultOf<Array> {
        if array.is_empty() {
            return Ok(Array::new());
        }

        let mut drained = Array::new();

        let mut i = 0;
        let mut x = 0;

        while x < array.len() {
            if !filter
                .call_raw(&ctx, None, [array[x].clone()])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(fn_sig, _)
                        if fn_sig.starts_with(filter.fn_name()) =>
                    {
                        filter.call_raw(&ctx, None, [array[x].clone(), (i as INT).into()])
                    }
                    _ => Err(err),
                })
                .map_err(|err| {
                    Box::new(ERR::ErrorInFunctionCall(
                        "retain".to_string(),
                        ctx.source().unwrap_or("").to_string(),
                        err,
                        Position::NONE,
                    ))
                })?
                .as_bool()
                .unwrap_or(false)
            {
                drained.push(array.remove(x));
            } else {
                x += 1;
            }

            i += 1;
        }

        Ok(drained)
    }
    #[rhai_fn(name = "retain", return_raw)]
    pub fn retain_with_fn_name(
        ctx: NativeCallContext,
        array: &mut Array,
        filter: &str,
    ) -> RhaiResultOf<Array> {
        retain(ctx, array, FnPtr::new(filter)?)
    }
    #[rhai_fn(name = "retain")]
    pub fn retain_exclusive_range(array: &mut Array, range: ExclusiveRange) -> Array {
        let start = INT::max(range.start, 0);
        let end = INT::max(range.end, start);
        retain_range(array, start, end - start)
    }
    #[rhai_fn(name = "retain")]
    pub fn retain_inclusive_range(array: &mut Array, range: InclusiveRange) -> Array {
        let start = INT::max(*range.start(), 0);
        let end = INT::max(*range.end(), start);
        retain_range(array, start, end - start + 1)
    }
    #[rhai_fn(name = "retain")]
    pub fn retain_range(array: &mut Array, start: INT, len: INT) -> Array {
        if array.is_empty() || len <= 0 {
            return Array::new();
        }

        let (start, len) = calc_offset_len(array.len(), start, len);

        if len == 0 {
            Array::new()
        } else {
            let mut drained: Array = array.drain(..start).collect();
            drained.extend(array.drain(len..));

            drained
        }
    }
    #[rhai_fn(name = "==", return_raw, pure)]
    pub fn equals(ctx: NativeCallContext, array1: &mut Array, array2: Array) -> RhaiResultOf<bool> {
        if array1.len() != array2.len() {
            return Ok(false);
        }
        if array1.is_empty() {
            return Ok(true);
        }

        let mut array2 = array2;

        for (a1, a2) in array1.iter_mut().zip(array2.iter_mut()) {
            if !ctx
                .call_fn_raw(OP_EQUALS, true, false, &mut [a1, a2])
                .or_else(|err| match *err {
                    ERR::ErrorFunctionNotFound(ref fn_sig, _) if fn_sig.starts_with(OP_EQUALS) => {
                        if a1.type_id() == a2.type_id() {
                            // No default when comparing same type
                            Err(err)
                        } else {
                            Ok(Dynamic::FALSE)
                        }
                    }
                    _ => Err(err),
                })?
                .as_bool()
                .unwrap_or(false)
            {
                return Ok(false);
            }
        }

        Ok(true)
    }
    #[rhai_fn(name = "!=", return_raw, pure)]
    pub fn not_equals(
        ctx: NativeCallContext,
        array1: &mut Array,
        array2: Array,
    ) -> RhaiResultOf<bool> {
        equals(ctx, array1, array2).map(|r| !r)
    }
}
