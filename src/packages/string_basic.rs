#![allow(non_snake_case)]

use crate::def_package;
use crate::engine::{FN_TO_STRING, KEYWORD_DEBUG, KEYWORD_PRINT};
use crate::fn_native::FnPtr;
use crate::plugin::*;
use crate::utils::ImmutableString;
use crate::INT;

#[cfg(not(feature = "no_index"))]
use crate::engine::Array;

#[cfg(not(feature = "no_object"))]
use crate::engine::Map;

use crate::stdlib::{
    fmt::{Debug, Display},
    format,
    string::ToString,
};

type Unit = ();

macro_rules! gen_functions {
    ($root:ident => $fn_name:ident ( $($arg_type:ident),+ )) => {
        pub mod $root { $(pub mod $arg_type {
            use super::super::*;

            #[export_fn]
            pub fn to_string_func(x: &mut $arg_type) -> ImmutableString {
                super::super::$fn_name(x)
            }
        })* }
    }
}

macro_rules! reg_print_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        set_exported_fn!($mod_name, FN_TO_STRING, $root::$arg_type::to_string_func);
        set_exported_fn!($mod_name, KEYWORD_PRINT, $root::$arg_type::to_string_func);
    )* }
}

macro_rules! reg_debug_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
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

        #[cfg(not(target_arch = "wasm32"))]
        {
            reg_print_functions!(lib += print_num_128; i128, u128);
            reg_debug_functions!(lib += debug_num_128; i128, u128);
        }
    }

    #[cfg(not(feature = "no_float"))]
    {
        reg_print_functions!(lib += print_float; f32, f64);
        reg_debug_functions!(lib += debug_float; f32, f64);
    }

    #[cfg(not(feature = "no_index"))]
    {
        reg_print_functions!(lib += print_array; Array);
        reg_debug_functions!(lib += print_array; Array);
    }
});

fn to_string<T: Display>(x: &mut T) -> ImmutableString {
    x.to_string().into()
}
fn to_debug<T: Debug>(x: &mut T) -> ImmutableString {
    format!("{:?}", x).into()
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
#[cfg(not(target_arch = "wasm32"))]
gen_functions!(print_num_128 => to_string(i128, u128));

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_functions!(debug_num_128 => to_debug(i128, u128));

#[cfg(not(feature = "no_float"))]
gen_functions!(print_float => to_string(f32, f64));

#[cfg(not(feature = "no_float"))]
gen_functions!(debug_float => to_debug(f32, f64));

#[cfg(not(feature = "no_index"))]
gen_functions!(print_array => to_debug(Array));

// Register print and debug
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
    #[rhai_fn(name = "debug")]
    pub fn debug_fn_ptr(f: &mut FnPtr) -> ImmutableString {
        to_string(f)
    }

    #[cfg(not(feature = "no_object"))]
    pub mod map_functions {
        use super::*;

        #[rhai_fn(name = "print", name = "debug", name = "to_string")]
        pub fn format_map(x: &mut Map) -> ImmutableString {
            format!("#{:?}", x).into()
        }
    }
}
