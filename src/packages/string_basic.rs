#![allow(non_snake_case)]

use crate::plugin::*;
use crate::{def_package, FnPtr};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_object"))]
use crate::Map;

pub const FUNC_TO_STRING: &'static str = "to_string";
pub const FUNC_TO_DEBUG: &'static str = "to_debug";

def_package!(crate:BasicStringPackage:"Basic string utilities, including printing.", lib, {
    combine_with_exported_module!(lib, "print_debug", print_debug_functions);
});

// Register print and debug

#[inline(always)]
pub fn print_with_func(
    fn_name: &str,
    ctx: &NativeCallContext,
    value: &mut Dynamic,
) -> crate::ImmutableString {
    match ctx.call_fn_dynamic_raw(fn_name, true, &mut [value]) {
        Ok(result) if result.is::<crate::ImmutableString>() => {
            result.take_immutable_string().unwrap()
        }
        Ok(result) => ctx.engine().map_type_name(result.type_name()).into(),
        Err(_) => ctx.engine().map_type_name(value.type_name()).into(),
    }
}

#[export_module]
mod print_debug_functions {
    use crate::ImmutableString;

    #[rhai_fn(name = "print", pure)]
    pub fn print_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        print_with_func(FUNC_TO_STRING, &ctx, item)
    }
    #[rhai_fn(name = "to_string", pure)]
    pub fn to_string_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        ctx.engine().map_type_name(&item.to_string()).into()
    }
    #[rhai_fn(name = "debug", pure)]
    pub fn debug_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        print_with_func(FUNC_TO_DEBUG, &ctx, item)
    }
    #[rhai_fn(name = "to_debug", pure)]
    pub fn to_debug_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        ctx.engine().map_type_name(&format!("{:?}", item)).into()
    }
    #[rhai_fn(name = "print", name = "debug")]
    pub fn print_empty_string() -> ImmutableString {
        Default::default()
    }
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_string(s: ImmutableString) -> ImmutableString {
        s
    }
    #[rhai_fn(name = "debug", name = "to_debug", pure)]
    pub fn debug_fn_ptr(f: &mut FnPtr) -> ImmutableString {
        f.to_string().into()
    }

    #[cfg(not(feature = "no_float"))]
    pub mod float_functions {
        use crate::ast::FloatWrapper;

        #[rhai_fn(name = "print", name = "to_string")]
        pub fn print_f64(number: f64) -> ImmutableString {
            FloatWrapper::new(number).to_string().into()
        }
        #[rhai_fn(name = "print", name = "to_string")]
        pub fn print_f32(number: f32) -> ImmutableString {
            FloatWrapper::new(number).to_string().into()
        }
        #[rhai_fn(name = "debug", name = "to_debug")]
        pub fn debug_f64(number: f64) -> ImmutableString {
            format!("{:?}", FloatWrapper::new(number)).into()
        }
        #[rhai_fn(name = "debug", name = "to_debug")]
        pub fn debug_f32(number: f32) -> ImmutableString {
            format!("{:?}", FloatWrapper::new(number)).into()
        }
    }

    #[cfg(not(feature = "no_index"))]
    pub mod array_functions {
        use super::*;

        #[rhai_fn(
            name = "print",
            name = "to_string",
            name = "debug",
            name = "to_debug",
            pure
        )]
        pub fn format_array(ctx: NativeCallContext, array: &mut Array) -> ImmutableString {
            let len = array.len();
            let mut result = std::string::String::with_capacity(len * 5 + 2);
            result.push_str("[");

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
            name = "debug",
            name = "to_debug",
            pure
        )]
        pub fn format_map(ctx: NativeCallContext, map: &mut Map) -> ImmutableString {
            let len = map.len();
            let mut result = std::string::String::with_capacity(len * 5 + 3);
            result.push_str("#{");

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
