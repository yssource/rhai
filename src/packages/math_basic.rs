use crate::def_package;
use crate::parser::INT;
use crate::plugin::*;

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

#[cfg(not(feature = "no_float"))]
use crate::{result::EvalAltResult, token::Position};

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

def_package!(crate:BasicMathPackage:"Basic mathematic functions.", lib, {
    #[cfg(not(feature = "no_float"))]
    {
        // Floating point functions
        lib.combine(exported_module!(float_functions));

        // Trig functions
        lib.combine(exported_module!(trig_functions));

        // Register conversion functions
        lib.set_fn_1("to_float", |x: INT| Ok(x as FLOAT));
        lib.set_fn_1("to_float", |x: f32| Ok(x as FLOAT));

        if cfg!(not(feature = "only_i32")) && cfg!(not(feature = "only_i64")) {
            lib.set_fn_1("to_float", |x: i8| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: u8| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: i16| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: u16| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: i32| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: u32| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: i64| Ok(x as FLOAT));
            lib.set_fn_1("to_float", |x: u64| Ok(x as FLOAT));

            if cfg!(not(target_arch = "wasm32")) {
                lib.set_fn_1("to_float", |x: i128| Ok(x as FLOAT));
                lib.set_fn_1("to_float", |x: u128| Ok(x as FLOAT));
            }
        }
    }

    lib.set_fn_1("to_int", |ch: char| Ok(ch as INT));

    if cfg!(not(feature = "only_i32")) && cfg!(not(feature = "only_i64")) {
        lib.set_fn_1("to_int", |x: i8| Ok(x as INT));
        lib.set_fn_1("to_int", |x: u8| Ok(x as INT));
        lib.set_fn_1("to_int", |x: i16| Ok(x as INT));
        lib.set_fn_1("to_int", |x: u16| Ok(x as INT));
    }

    if cfg!(not(feature = "only_i32")) {
        lib.set_fn_1("to_int", |x: i32| Ok(x as INT));
        lib.set_fn_1("to_int", |x: u64| Ok(x as INT));

        if cfg!(feature = "only_i64") {
            lib.set_fn_1("to_int", |x: u32| Ok(x as INT));
        }
    }

    #[cfg(not(feature = "no_float"))]
    {
        if cfg!(not(feature = "unchecked")) {
            lib.set_fn_1("to_int", |x: f32| {
                if x > (MAX_INT as f32) {
                    return EvalAltResult::ErrorArithmetic(
                        format!("Integer overflow: to_int({})", x),
                        Position::none(),
                    )
                    .into();
                }

                Ok(x.trunc() as INT)
            });
            lib.set_fn_1("to_int", |x: FLOAT| {
                if x > (MAX_INT as FLOAT) {
                    return EvalAltResult::ErrorArithmetic(
                        format!("Integer overflow: to_int({})", x),
                        Position::none(),
                    )
                    .into();
                }

                Ok(x.trunc() as INT)
            });
        }

        if cfg!(feature = "unchecked") {
            lib.set_fn_1("to_int", |x: f32| Ok(x as INT));
            lib.set_fn_1("to_int", |x: f64| Ok(x as INT));
        }
    }
});

#[cfg(not(feature = "no_float"))]
#[export_module]
mod trig_functions {
    use crate::parser::FLOAT;

    #[inline(always)]
    pub fn sin(x: FLOAT) -> FLOAT {
        x.to_radians().sin()
    }
    #[inline(always)]
    pub fn cos(x: FLOAT) -> FLOAT {
        x.to_radians().cos()
    }
    #[inline(always)]
    pub fn tan(x: FLOAT) -> FLOAT {
        x.to_radians().tan()
    }
    #[inline(always)]
    pub fn sinh(x: FLOAT) -> FLOAT {
        x.to_radians().sinh()
    }
    #[inline(always)]
    pub fn cosh(x: FLOAT) -> FLOAT {
        x.to_radians().cosh()
    }
    #[inline(always)]
    pub fn tanh(x: FLOAT) -> FLOAT {
        x.to_radians().tanh()
    }
    #[inline(always)]
    pub fn asin(x: FLOAT) -> FLOAT {
        x.asin().to_degrees()
    }
    #[inline(always)]
    pub fn acos(x: FLOAT) -> FLOAT {
        x.acos().to_degrees()
    }
    #[inline(always)]
    pub fn atan(x: FLOAT) -> FLOAT {
        x.atan().to_degrees()
    }
    #[inline(always)]
    pub fn asinh(x: FLOAT) -> FLOAT {
        x.asinh().to_degrees()
    }
    #[inline(always)]
    pub fn acosh(x: FLOAT) -> FLOAT {
        x.acosh().to_degrees()
    }
    #[inline(always)]
    pub fn atanh(x: FLOAT) -> FLOAT {
        x.atanh().to_degrees()
    }
}

#[cfg(not(feature = "no_float"))]
#[export_module]
mod float_functions {
    use crate::parser::FLOAT;

    #[inline(always)]
    pub fn sqrt(x: FLOAT) -> FLOAT {
        x.sqrt()
    }
    #[inline(always)]
    pub fn exp(x: FLOAT) -> FLOAT {
        x.exp()
    }
    #[inline(always)]
    pub fn ln(x: FLOAT) -> FLOAT {
        x.ln()
    }
    #[inline(always)]
    pub fn log(x: FLOAT, base: FLOAT) -> FLOAT {
        x.log(base)
    }
    #[inline(always)]
    pub fn log10(x: FLOAT) -> FLOAT {
        x.log10()
    }
    #[inline(always)]
    pub fn floor(x: FLOAT) -> FLOAT {
        x.floor()
    }
    #[rhai_fn(get = "floor")]
    #[inline(always)]
    pub fn floor_prop(x: FLOAT) -> FLOAT {
        floor(x)
    }
    #[inline(always)]
    pub fn ceiling(x: FLOAT) -> FLOAT {
        x.ceil()
    }
    #[rhai_fn(get = "ceiling")]
    #[inline(always)]
    pub fn ceiling_prop(x: FLOAT) -> FLOAT {
        ceiling(x)
    }
    #[inline(always)]
    pub fn round(x: FLOAT) -> FLOAT {
        x.ceil()
    }
    #[rhai_fn(get = "round")]
    #[inline(always)]
    pub fn round_prop(x: FLOAT) -> FLOAT {
        ceiling(x)
    }
    #[inline(always)]
    pub fn int(x: FLOAT) -> FLOAT {
        x.trunc()
    }
    #[rhai_fn(get = "int")]
    #[inline(always)]
    pub fn int_prop(x: FLOAT) -> FLOAT {
        int(x)
    }
    #[inline(always)]
    pub fn fraction(x: FLOAT) -> FLOAT {
        x.fract()
    }
    #[rhai_fn(get = "fraction")]
    #[inline(always)]
    pub fn fraction_prop(x: FLOAT) -> FLOAT {
        fraction(x)
    }
    #[inline(always)]
    pub fn is_nan(x: FLOAT) -> bool {
        x.is_nan()
    }
    #[rhai_fn(get = "is_nan")]
    #[inline(always)]
    pub fn is_nan_prop(x: FLOAT) -> bool {
        is_nan(x)
    }
    #[inline(always)]
    pub fn is_finite(x: FLOAT) -> bool {
        x.is_finite()
    }
    #[rhai_fn(get = "is_finite")]
    #[inline(always)]
    pub fn is_finite_prop(x: FLOAT) -> bool {
        is_finite(x)
    }
    #[inline(always)]
    pub fn is_infinite(x: FLOAT) -> bool {
        x.is_infinite()
    }
    #[rhai_fn(get = "is_infinite")]
    #[inline(always)]
    pub fn is_infinite_prop(x: FLOAT) -> bool {
        is_infinite(x)
    }
}
