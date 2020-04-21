use super::{reg_binary, reg_unary};

use crate::def_package;
use crate::fn_register::{map_dynamic as map, map_result as result};
use crate::parser::INT;
use crate::result::EvalAltResult;
use crate::token::Position;

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

use crate::stdlib::{i32, i64};

#[cfg(feature = "only_i32")]
pub const MAX_INT: INT = i32::MAX;
#[cfg(not(feature = "only_i32"))]
pub const MAX_INT: INT = i64::MAX;

def_package!(BasicMathPackage:"Basic mathematic functions.", lib, {
    #[cfg(not(feature = "no_float"))]
    {
        // Advanced math functions
        reg_unary(lib, "sin", |x: FLOAT| x.to_radians().sin(), map);
        reg_unary(lib, "cos", |x: FLOAT| x.to_radians().cos(), map);
        reg_unary(lib, "tan", |x: FLOAT| x.to_radians().tan(), map);
        reg_unary(lib, "sinh", |x: FLOAT| x.to_radians().sinh(), map);
        reg_unary(lib, "cosh", |x: FLOAT| x.to_radians().cosh(), map);
        reg_unary(lib, "tanh", |x: FLOAT| x.to_radians().tanh(), map);
        reg_unary(lib, "asin", |x: FLOAT| x.asin().to_degrees(), map);
        reg_unary(lib, "acos", |x: FLOAT| x.acos().to_degrees(), map);
        reg_unary(lib, "atan", |x: FLOAT| x.atan().to_degrees(), map);
        reg_unary(lib, "asinh", |x: FLOAT| x.asinh().to_degrees(), map);
        reg_unary(lib, "acosh", |x: FLOAT| x.acosh().to_degrees(), map);
        reg_unary(lib, "atanh", |x: FLOAT| x.atanh().to_degrees(), map);
        reg_unary(lib, "sqrt", |x: FLOAT| x.sqrt(), map);
        reg_unary(lib, "exp", |x: FLOAT| x.exp(), map);
        reg_unary(lib, "ln", |x: FLOAT| x.ln(), map);
        reg_binary(lib, "log", |x: FLOAT, base: FLOAT| x.log(base), map);
        reg_unary(lib, "log10", |x: FLOAT| x.log10(), map);
        reg_unary(lib, "floor", |x: FLOAT| x.floor(), map);
        reg_unary(lib, "ceiling", |x: FLOAT| x.ceil(), map);
        reg_unary(lib, "round", |x: FLOAT| x.ceil(), map);
        reg_unary(lib, "int", |x: FLOAT| x.trunc(), map);
        reg_unary(lib, "fraction", |x: FLOAT| x.fract(), map);
        reg_unary(lib, "is_nan", |x: FLOAT| x.is_nan(), map);
        reg_unary(lib, "is_finite", |x: FLOAT| x.is_finite(), map);
        reg_unary(lib, "is_infinite", |x: FLOAT| x.is_infinite(), map);

        // Register conversion functions
        reg_unary(lib, "to_float", |x: INT| x as FLOAT, map);
        reg_unary(lib, "to_float", |x: f32| x as FLOAT, map);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_unary(lib, "to_float", |x: i8| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: u8| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: i16| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: u16| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: i32| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: u32| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: i64| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: u64| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: i128| x as FLOAT, map);
            reg_unary(lib, "to_float", |x: u128| x as FLOAT, map);
        }
    }

    reg_unary(lib, "to_int", |ch: char| ch as INT, map);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_unary(lib, "to_int", |x: i8| x as INT, map);
        reg_unary(lib, "to_int", |x: u8| x as INT, map);
        reg_unary(lib, "to_int", |x: i16| x as INT, map);
        reg_unary(lib, "to_int", |x: u16| x as INT, map);
    }

    #[cfg(not(feature = "only_i32"))]
    {
        reg_unary(lib, "to_int", |x: i32| x as INT, map);
        reg_unary(lib, "to_int", |x: u64| x as INT, map);

        #[cfg(feature = "only_i64")]
        reg_unary(lib, "to_int", |x: u32| x as INT, map);
    }

    #[cfg(not(feature = "no_float"))]
    {
        #[cfg(not(feature = "unchecked"))]
        {
            reg_unary(
                lib,
                "to_int",
                |x: f32| {
                    if x > (MAX_INT as f32) {
                        return Err(EvalAltResult::ErrorArithmetic(
                            format!("Integer overflow: to_int({})", x),
                            Position::none(),
                        ));
                    }

                    Ok(x.trunc() as INT)
                },
                result,
            );
            reg_unary(
                lib,
                "to_int",
                |x: FLOAT| {
                    if x > (MAX_INT as FLOAT) {
                        return Err(EvalAltResult::ErrorArithmetic(
                            format!("Integer overflow: to_int({})", x),
                            Position::none(),
                        ));
                    }

                    Ok(x.trunc() as INT)
                },
                result,
            );
        }

        #[cfg(feature = "unchecked")]
        {
            reg_unary(lib, "to_int", |x: f32| x as INT, map);
            reg_unary(lib, "to_int", |x: f64| x as INT, map);
        }
    }
});
