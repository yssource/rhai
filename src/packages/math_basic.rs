#![allow(non_snake_case)]

use crate::def_package;
use crate::parser::INT;
use crate::plugin::*;
use crate::token::Position;

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

#[cfg(not(feature = "no_float"))]
use crate::result::EvalAltResult;

#[cfg(feature = "no_std")]
#[cfg(not(feature = "no_float"))]
use num_traits::float::Float;

#[cfg(not(feature = "no_float"))]
use crate::stdlib::format;

#[allow(dead_code)]
#[cfg(feature = "only_i32")]
pub const MAX_INT: INT = i32::MAX;
#[allow(dead_code)]
#[cfg(not(feature = "only_i32"))]
pub const MAX_INT: INT = i64::MAX;

macro_rules! gen_conversion_functions {
    ($root:ident => $func_name:ident ( $($arg_type:ident),+ ) -> $result_type:ty) => {
        pub mod $root { $(pub mod $arg_type {
            use super::super::*;

            #[export_fn]
            pub fn $func_name(x: $arg_type) -> $result_type {
                x as $result_type
            }
        })* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident :: $func_name:ident ( $($arg_type:ident),+ ) ) => { $(
        set_exported_fn!($mod_name, stringify!($func_name), $root::$arg_type::$func_name);
    )* }
}

def_package!(crate:BasicMathPackage:"Basic mathematic functions.", lib, {
    #[cfg(not(feature = "no_float"))]
    {
        // Floating point functions
        combine_with_exported_module!(lib, "float", float_functions);

        // Trig functions
        combine_with_exported_module!(lib, "trig", trig_functions);

        reg_functions!(lib += basic_to_float::to_float(INT));

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_functions!(lib += numbers_to_float::to_float(i8, u8, i16, u16, i32, u32, i64, u32));

            #[cfg(not(target_arch = "wasm32"))]
            reg_functions!(lib += num_128_to_float::to_float(i128, u128));
        }
    }

    reg_functions!(lib += basic_to_int::to_int(char));

    set_exported_fn!(lib, "parse_int", parse_int);
    set_exported_fn!(lib, "parse_int", parse_int_radix);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += numbers_to_int::to_int(i8, u8, i16, u16, i32, u32, i64, u64));

        #[cfg(not(target_arch = "wasm32"))]
        reg_functions!(lib += num_128_to_int::to_int(i128, u128));
    }
});

#[cfg(not(feature = "no_float"))]
#[export_module]
mod trig_functions {
    use crate::parser::FLOAT;

    pub fn sin(x: FLOAT) -> FLOAT {
        x.to_radians().sin()
    }
    pub fn cos(x: FLOAT) -> FLOAT {
        x.to_radians().cos()
    }
    pub fn tan(x: FLOAT) -> FLOAT {
        x.to_radians().tan()
    }
    pub fn sinh(x: FLOAT) -> FLOAT {
        x.to_radians().sinh()
    }
    pub fn cosh(x: FLOAT) -> FLOAT {
        x.to_radians().cosh()
    }
    pub fn tanh(x: FLOAT) -> FLOAT {
        x.to_radians().tanh()
    }
    pub fn asin(x: FLOAT) -> FLOAT {
        x.asin().to_degrees()
    }
    pub fn acos(x: FLOAT) -> FLOAT {
        x.acos().to_degrees()
    }
    pub fn atan(x: FLOAT) -> FLOAT {
        x.atan().to_degrees()
    }
    pub fn asinh(x: FLOAT) -> FLOAT {
        x.asinh().to_degrees()
    }
    pub fn acosh(x: FLOAT) -> FLOAT {
        x.acosh().to_degrees()
    }
    pub fn atanh(x: FLOAT) -> FLOAT {
        x.atanh().to_degrees()
    }
}

#[cfg(not(feature = "no_float"))]
#[export_module]
mod float_functions {
    use crate::parser::FLOAT;

    pub fn sqrt(x: FLOAT) -> FLOAT {
        x.sqrt()
    }
    pub fn exp(x: FLOAT) -> FLOAT {
        x.exp()
    }
    pub fn ln(x: FLOAT) -> FLOAT {
        x.ln()
    }
    pub fn log(x: FLOAT, base: FLOAT) -> FLOAT {
        x.log(base)
    }
    pub fn log10(x: FLOAT) -> FLOAT {
        x.log10()
    }
    #[rhai_fn(name = "floor", get = "floor")]
    pub fn floor(x: FLOAT) -> FLOAT {
        x.floor()
    }
    #[rhai_fn(name = "ceiling", get = "ceiling")]
    pub fn ceiling(x: FLOAT) -> FLOAT {
        x.ceil()
    }
    #[rhai_fn(name = "round", get = "round")]
    pub fn round(x: FLOAT) -> FLOAT {
        x.ceil()
    }
    #[rhai_fn(name = "int", get = "int")]
    pub fn int(x: FLOAT) -> FLOAT {
        x.trunc()
    }
    #[rhai_fn(name = "fraction", get = "fraction")]
    pub fn fraction(x: FLOAT) -> FLOAT {
        x.fract()
    }
    #[rhai_fn(name = "is_nan", get = "is_nan")]
    pub fn is_nan(x: FLOAT) -> bool {
        x.is_nan()
    }
    #[rhai_fn(name = "is_finite", get = "is_finite")]
    pub fn is_finite(x: FLOAT) -> bool {
        x.is_finite()
    }
    #[rhai_fn(name = "is_infinite", get = "is_infinite")]
    pub fn is_infinite(x: FLOAT) -> bool {
        x.is_infinite()
    }
    #[rhai_fn(name = "to_int", return_raw)]
    pub fn f32_to_int(x: f32) -> Result<Dynamic, Box<EvalAltResult>> {
        if cfg!(not(feature = "unchecked")) && x > (MAX_INT as f32) {
            EvalAltResult::ErrorArithmetic(
                format!("Integer overflow: to_int({})", x),
                Position::none(),
            )
            .into()
        } else {
            Ok((x.trunc() as INT).into())
        }
    }
    #[rhai_fn(name = "to_int", return_raw)]
    pub fn f64_to_int(x: f64) -> Result<Dynamic, Box<EvalAltResult>> {
        if cfg!(not(feature = "unchecked")) && x > (MAX_INT as f64) {
            EvalAltResult::ErrorArithmetic(
                format!("Integer overflow: to_int({})", x),
                Position::none(),
            )
            .into()
        } else {
            Ok((x.trunc() as INT).into())
        }
    }

    #[rhai_fn(return_raw)]
    pub fn parse_float(s: &str) -> Result<Dynamic, Box<EvalAltResult>> {
        s.trim()
            .parse::<FLOAT>()
            .map(Into::<Dynamic>::into)
            .map_err(|err| {
                EvalAltResult::ErrorArithmetic(
                    format!("Error parsing floating-point number '{}': {}", s, err),
                    Position::none(),
                )
                .into()
            })
    }
}

#[cfg(not(feature = "no_float"))]
gen_conversion_functions!(basic_to_float => to_float (INT) -> FLOAT);

#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_conversion_functions!(numbers_to_float => to_float (i8, u8, i16, u16, i32, u32, i64, u64) -> FLOAT);

#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_conversion_functions!(num_128_to_float => to_float (i128, u128) -> FLOAT);

gen_conversion_functions!(basic_to_int => to_int (char) -> INT);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_conversion_functions!(numbers_to_int => to_int (i8, u8, i16, u16, i32, u32, i64, u64) -> INT);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_conversion_functions!(num_128_to_int => to_int (i128, u128) -> INT);

#[export_fn(return_raw)]
fn parse_int_radix(s: &str, radix: INT) -> Result<Dynamic, Box<EvalAltResult>> {
    if radix < 2 || radix > 36 {
        return EvalAltResult::ErrorArithmetic(
            format!("Invalid radix: '{}'", radix),
            Position::none(),
        )
        .into();
    }

    INT::from_str_radix(s.trim(), radix as u32)
        .map(Into::<Dynamic>::into)
        .map_err(|err| {
            EvalAltResult::ErrorArithmetic(
                format!("Error parsing integer number '{}': {}", s, err),
                Position::none(),
            )
            .into()
        })
}

#[export_fn(return_raw)]
fn parse_int(s: &str) -> Result<Dynamic, Box<EvalAltResult>> {
    parse_int_radix(s, 10)
}
