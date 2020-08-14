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
        lib.merge(&exported_module!(float));

        // Floating point properties
        #[cfg(not(feature = "no_object"))]
        {
            lib.set_getter_fn("floor", |x: &mut FLOAT| Ok(x.floor()));
            lib.set_getter_fn("ceiling", |x: &mut FLOAT| Ok(x.ceil()));
            lib.set_getter_fn("round", |x: &mut FLOAT| Ok(x.ceil()));
            lib.set_getter_fn("int", |x: &mut FLOAT| Ok(x.trunc()));
            lib.set_getter_fn("fraction", |x: &mut FLOAT| Ok(x.fract()));
            lib.set_getter_fn("is_nan", |x: &mut FLOAT| Ok(x.is_nan()));
            lib.set_getter_fn("is_finite", |x: &mut FLOAT| Ok(x.is_finite()));
            lib.set_getter_fn("is_infinite", |x: &mut FLOAT| Ok(x.is_infinite()));
        }

        // Trig functions
        lib.merge(&exported_module!(trig));

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
mod trig {
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
mod float {
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
    pub fn floor(x: FLOAT) -> FLOAT {
        x.floor()
    }
    pub fn ceiling(x: FLOAT) -> FLOAT {
        x.ceil()
    }
    pub fn round(x: FLOAT) -> FLOAT {
        x.ceil()
    }
    pub fn int(x: FLOAT) -> FLOAT {
        x.trunc()
    }
    pub fn fraction(x: FLOAT) -> FLOAT {
        x.fract()
    }
    pub fn is_nan(x: FLOAT) -> bool {
        x.is_nan()
    }
    pub fn is_finite(x: FLOAT) -> bool {
        x.is_finite()
    }
    pub fn is_infinite(x: FLOAT) -> bool {
        x.is_infinite()
    }
}
