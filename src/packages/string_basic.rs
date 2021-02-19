#![allow(non_snake_case)]

use crate::engine::{KEYWORD_DEBUG, KEYWORD_PRINT};
use crate::plugin::*;
use crate::stdlib::{
    fmt::{Debug, Display},
    format,
    string::ToString,
};
use crate::{def_package, FnPtr, ImmutableString, INT};

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_object"))]
use crate::Map;

#[cfg(feature = "decimal")]
use rust_decimal::Decimal;

const FUNC_TO_STRING: &'static str = "to_string";
const FUNC_TO_DEBUG: &'static str = "to_debug";

type Unit = ();

macro_rules! gen_functions {
    ($root:ident => $fn_name:ident ( $($arg_type:ident),+ )) => {
        pub mod $root { $(pub mod $arg_type {
            use super::super::*;

            #[export_fn(pure)]
            pub fn to_string_func(x: &mut $arg_type) -> ImmutableString {
                super::super::$fn_name(x)
            }
        })* }
    }
}

macro_rules! reg_print_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        set_exported_fn!($mod_name, FUNC_TO_STRING, $root::$arg_type::to_string_func);
        set_exported_fn!($mod_name, KEYWORD_PRINT, $root::$arg_type::to_string_func);
    )* }
}

macro_rules! reg_debug_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        set_exported_fn!($mod_name, FUNC_TO_DEBUG, $root::$arg_type::to_string_func);
        set_exported_fn!($mod_name, KEYWORD_DEBUG, $root::$arg_type::to_string_func);
    )* }
}

def_package!(crate:BasicStringPackage:"Basic string utilities, including printing.", lib, {
    combine_with_exported_module!(lib, "print_debug", print_debug_functions);

    reg_print_functions!(lib += print_basic; INT, bool, char, FnPtr);
    reg_debug_functions!(lib += debug_basic; INT, bool, Unit, char, ImmutableString);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_print_functions!(lib += print_numbers; i8, u8, i16, u16, i32, u32, i64, u64);
        reg_debug_functions!(lib += debug_numbers; i8, u8, i16, u16, i32, u32, i64, u64);

        #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
        {
            reg_print_functions!(lib += print_num_128; i128, u128);
            reg_debug_functions!(lib += debug_num_128; i128, u128);
        }
    }

    #[cfg(not(feature = "no_float"))]
    {
        reg_print_functions!(lib += print_float_64; f64);
        reg_debug_functions!(lib += print_float_64; f64);
        reg_print_functions!(lib += print_float_32; f32);
        reg_debug_functions!(lib += print_float_32; f32);
    }

    #[cfg(feature = "decimal")]
    {
        reg_print_functions!(lib += print_decimal; Decimal);
        reg_debug_functions!(lib += debug_decimal; Decimal);
    }
});

fn to_string<T: Display>(x: &mut T) -> ImmutableString {
    x.to_string().into()
}
fn to_debug<T: Debug>(x: &mut T) -> ImmutableString {
    format!("{:?}", x).into()
}
#[cfg(not(feature = "no_float"))]
fn print_f64(x: &mut f64) -> ImmutableString {
    #[cfg(feature = "no_std")]
    use num_traits::Float;

    let abs = x.abs();
    if abs > 10000000000000.0 || abs < 0.0000000000001 {
        format!("{:e}", x).into()
    } else {
        x.to_string().into()
    }
}
#[cfg(not(feature = "no_float"))]
fn print_f32(x: &mut f32) -> ImmutableString {
    #[cfg(feature = "no_std")]
    use num_traits::Float;

    let abs = x.abs();
    if abs > 10000000000000.0 || abs < 0.0000000000001 {
        format!("{:e}", x).into()
    } else {
        x.to_string().into()
    }
}

gen_functions!(print_basic => to_string(INT, bool, char, FnPtr));
gen_functions!(debug_basic => to_debug(INT, bool, Unit, char, ImmutableString));

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_functions!(print_numbers => to_string(i8, u8, i16, u16, i32, u32, i64, u64));

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_functions!(debug_numbers => to_debug(i8, u8, i16, u16, i32, u32, i64, u64));

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
gen_functions!(print_num_128 => to_string(i128, u128));

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
gen_functions!(debug_num_128 => to_debug(i128, u128));

#[cfg(not(feature = "no_float"))]
gen_functions!(print_float_64 => print_f64(f64));

#[cfg(not(feature = "no_float"))]
gen_functions!(print_float_32 => print_f32(f32));

#[cfg(feature = "decimal")]
gen_functions!(print_decimal => to_string(Decimal));

#[cfg(feature = "decimal")]
gen_functions!(debug_decimal => to_debug(Decimal));

// Register print and debug

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
#[inline(always)]
fn print_with_func(fn_name: &str, ctx: &NativeCallContext, value: &mut Dynamic) -> ImmutableString {
    match ctx.call_fn_dynamic_raw(fn_name, true, false, &mut [value], None) {
        Ok(result) if result.is::<ImmutableString>() => result.take_immutable_string().unwrap(),
        Ok(result) => ctx.engine().map_type_name(result.type_name()).into(),
        Err(_) => ctx.engine().map_type_name(value.type_name()).into(),
    }
}

#[export_module]
mod print_debug_functions {
    #[rhai_fn(name = "print", name = "debug")]
    pub fn print_empty_string() -> ImmutableString {
        "".to_string().into()
    }
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_unit(_x: ()) -> ImmutableString {
        "".to_string().into()
    }
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_string(s: ImmutableString) -> ImmutableString {
        s
    }
    #[rhai_fn(name = "debug", pure)]
    pub fn debug_fn_ptr(f: &mut FnPtr) -> ImmutableString {
        to_string(f)
    }

    #[cfg(not(feature = "no_index"))]
    pub mod array_functions {
        use super::*;

        #[rhai_fn(
            name = "print",
            name = "to_string",
            name = "to_debug",
            name = "debug",
            pure
        )]
        pub fn format_array(ctx: NativeCallContext, array: &mut Array) -> ImmutableString {
            let mut result = crate::stdlib::string::String::with_capacity(16);
            result.push_str("[");

            let len = array.len();

            array.iter_mut().enumerate().for_each(|(i, x)| {
                result.push_str(&print_with_func(FUNC_TO_DEBUG, &ctx, x));
                if i < len - 1 {
                    result.push_str(", ");
                }
            });

            result.push_str("]");
            result.into()
        }
    }
    #[cfg(not(feature = "no_object"))]
    pub mod map_functions {
        use super::*;

        #[rhai_fn(
            name = "print",
            name = "to_string",
            name = "to_debug",
            name = "debug",
            pure
        )]
        pub fn format_map(ctx: NativeCallContext, map: &mut Map) -> ImmutableString {
            let mut result = crate::stdlib::string::String::with_capacity(16);
            result.push_str("#{");

            let len = map.len();

            map.iter_mut().enumerate().for_each(|(i, (k, v))| {
                result.push_str(&format!(
                    "{:?}: {}{}",
                    k,
                    &print_with_func(FUNC_TO_DEBUG, &ctx, v),
                    if i < len - 1 { ", " } else { "" }
                ));
            });

            result.push_str("}");
            result.into()
        }
    }
}
