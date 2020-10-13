#![cfg(not(feature = "no_index"))]
#![allow(non_snake_case)]

use crate::any::{Dynamic, Variant};
use crate::def_package;
use crate::engine::{Array, Engine};
use crate::fn_native::FnPtr;
use crate::parser::{ImmutableString, INT};
use crate::plugin::*;
use crate::result::EvalAltResult;
use crate::token::Position;

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
            }
        })* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        combine_with_exported_module!($mod_name, "array_functions", $root::$arg_type::functions);

        $mod_name.set_raw_fn("pad",
            &[TypeId::of::<Array>(), TypeId::of::<INT>(), TypeId::of::<$arg_type>()],
            pad::<$arg_type>);
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

    lib.set_raw_fn("map", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], map);
    lib.set_raw_fn("filter", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], filter);
    lib.set_raw_fn("reduce", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], reduce);
    lib.set_raw_fn("reduce_rev", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], reduce_rev);
    lib.set_raw_fn("some", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], some);
    lib.set_raw_fn("all", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], all);
    lib.set_raw_fn("sort", &[TypeId::of::<Array>(), TypeId::of::<FnPtr>()], sort);

    // Merge in the module at the end to override `+=` for arrays
    combine_with_exported_module!(lib, "array", array_functions);

    // Register array iterator
    lib.set_iter(
        TypeId::of::<Array>(),
        |arr| Box::new(arr.cast::<Array>().into_iter()) as Box<dyn Iterator<Item = Dynamic>>,
    );
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
}

fn pad<T: Variant + Clone>(
    _engine: &Engine,
    _: &Module,
    args: &mut [&mut Dynamic],
) -> Result<(), Box<EvalAltResult>> {
    let len = *args[1].read_lock::<INT>().unwrap();

    // Check if array will be over max size limit
    #[cfg(not(feature = "unchecked"))]
    if _engine.limits.max_array_size > 0
        && len > 0
        && (len as usize) > _engine.limits.max_array_size
    {
        return EvalAltResult::ErrorDataTooLarge(
            "Size of array".to_string(),
            _engine.limits.max_array_size,
            len as usize,
            Position::none(),
        )
        .into();
    }

    if len > 0 {
        let item = args[2].clone();
        let mut list = args[0].write_lock::<Array>().unwrap();

        if len as usize > list.len() {
            list.resize(len as usize, item);
        }
    }
    Ok(())
}

fn map(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<Array, Box<EvalAltResult>> {
    let list = args[0].read_lock::<Array>().unwrap();
    let mapper = args[1].read_lock::<FnPtr>().unwrap();

    let mut array = Array::with_capacity(list.len());

    for (i, item) in list.iter().enumerate() {
        array.push(
            mapper
                .call_dynamic(engine, lib, None, [item.clone()])
                .or_else(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(_, _) => {
                        mapper.call_dynamic(engine, lib, None, [item.clone(), (i as INT).into()])
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

    Ok(array)
}

fn filter(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<Array, Box<EvalAltResult>> {
    let list = args[0].read_lock::<Array>().unwrap();
    let filter = args[1].read_lock::<FnPtr>().unwrap();

    let mut array = Array::with_capacity(list.len());

    for (i, item) in list.iter().enumerate() {
        if filter
            .call_dynamic(engine, lib, None, [item.clone()])
            .or_else(|err| match *err {
                EvalAltResult::ErrorFunctionNotFound(_, _) => {
                    filter.call_dynamic(engine, lib, None, [item.clone(), (i as INT).into()])
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

    Ok(array)
}

fn some(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<bool, Box<EvalAltResult>> {
    let list = args[0].read_lock::<Array>().unwrap();
    let filter = args[1].read_lock::<FnPtr>().unwrap();

    for (i, item) in list.iter().enumerate() {
        if filter
            .call_dynamic(engine, lib, None, [item.clone()])
            .or_else(|err| match *err {
                EvalAltResult::ErrorFunctionNotFound(_, _) => {
                    filter.call_dynamic(engine, lib, None, [item.clone(), (i as INT).into()])
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

fn all(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<bool, Box<EvalAltResult>> {
    let list = args[0].read_lock::<Array>().unwrap();
    let filter = args[1].read_lock::<FnPtr>().unwrap();

    for (i, item) in list.iter().enumerate() {
        if !filter
            .call_dynamic(engine, lib, None, [item.clone()])
            .or_else(|err| match *err {
                EvalAltResult::ErrorFunctionNotFound(_, _) => {
                    filter.call_dynamic(engine, lib, None, [item.clone(), (i as INT).into()])
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

fn reduce(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<Dynamic, Box<EvalAltResult>> {
    let list = args[0].read_lock::<Array>().unwrap();
    let reducer = args[1].read_lock::<FnPtr>().unwrap();

    let mut result: Dynamic = ().into();

    for (i, item) in list.iter().enumerate() {
        result = reducer
            .call_dynamic(engine, lib, None, [result.clone(), item.clone()])
            .or_else(|err| match *err {
                EvalAltResult::ErrorFunctionNotFound(_, _) => reducer.call_dynamic(
                    engine,
                    lib,
                    None,
                    [result, item.clone(), (i as INT).into()],
                ),
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

fn reduce_rev(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<Dynamic, Box<EvalAltResult>> {
    let list = args[0].read_lock::<Array>().unwrap();
    let reducer = args[1].read_lock::<FnPtr>().unwrap();

    let mut result: Dynamic = ().into();

    for (i, item) in list.iter().enumerate().rev() {
        result = reducer
            .call_dynamic(engine, lib, None, [result.clone(), item.clone()])
            .or_else(|err| match *err {
                EvalAltResult::ErrorFunctionNotFound(_, _) => reducer.call_dynamic(
                    engine,
                    lib,
                    None,
                    [result, item.clone(), (i as INT).into()],
                ),
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

fn sort(
    engine: &Engine,
    lib: &Module,
    args: &mut [&mut Dynamic],
) -> Result<Dynamic, Box<EvalAltResult>> {
    let comparer = args[1].read_lock::<FnPtr>().unwrap().clone();
    let mut list = args[0].write_lock::<Array>().unwrap();

    list.sort_by(|x, y| {
        comparer
            .call_dynamic(engine, lib, None, [x.clone(), y.clone()])
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
