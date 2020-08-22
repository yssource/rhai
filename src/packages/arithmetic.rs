#![allow(non_snake_case)]

use crate::def_package;
use crate::parser::INT;
use crate::plugin::*;

use crate::{result::EvalAltResult, token::Position};

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

#[cfg(feature = "no_std")]
#[cfg(not(feature = "no_float"))]
use num_traits::float::Float;

use crate::stdlib::format;

macro_rules! gen_arithmetic_functions {
    ($root:ident => $($arg_type:ident),+) => {
        pub mod $root { $(
            pub mod $arg_type {
                use super::super::*;

                #[export_module]
                pub mod functions {
                    #[rhai_fn(name = "+", return_raw)]
                    #[inline]
                    pub fn add(x: $arg_type, y: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            x.checked_add(y).ok_or_else(|| {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Addition overflow: {} + {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            }).map(Dynamic::from)
                        } else {
                            Ok(Dynamic::from(x + y))
                        }
                    }
                    #[rhai_fn(name = "-", return_raw)]
                    #[inline]
                    pub fn subtract(x: $arg_type, y: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            x.checked_sub(y).ok_or_else(|| {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Subtraction overflow: {} - {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            }).map(Dynamic::from)
                        } else {
                            Ok(Dynamic::from(x - y))
                        }
                    }
                    #[rhai_fn(name = "*", return_raw)]
                    #[inline]
                    pub fn multiply(x: $arg_type, y: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            x.checked_mul(y).ok_or_else(|| {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Multiplication overflow: {} * {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            }).map(Dynamic::from)
                        } else {
                            Ok(Dynamic::from(x * y))
                        }
                    }
                    #[rhai_fn(name = "/", return_raw)]
                    #[inline]
                    pub fn divide(x: $arg_type, y: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            // Detect division by zero
                            if y == 0 {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Division by zero: {} / {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else {
                                x.checked_div(y).ok_or_else(|| {
                                    EvalAltResult::ErrorArithmetic(
                                        format!("Division overflow: {} / {}", x, y),
                                        Position::none(),
                                    )
                                    .into()
                                }).map(Dynamic::from)
                            }
                        } else {
                            Ok(Dynamic::from(x / y))
                        }
                    }
                    #[rhai_fn(name = "%", return_raw)]
                    #[inline]
                    pub fn modulo(x: $arg_type, y: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            x.checked_rem(y).ok_or_else(|| {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Modulo division by zero or overflow: {} % {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            }).map(Dynamic::from)
                        } else {
                            Ok(Dynamic::from(x % y))
                        }
                    }
                    #[rhai_fn(name = "~", return_raw)]
                    #[inline]
                    pub fn power(x: INT, y: INT) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            if cfg!(not(feature = "only_i32")) && y > (u32::MAX as INT) {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Integer raised to too large an index: {} ~ {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else if y < 0 {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Integer raised to a negative index: {} ~ {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else {
                                x.checked_pow(y as u32).ok_or_else(|| {
                                    EvalAltResult::ErrorArithmetic(
                                        format!("Power overflow: {} ~ {}", x, y),
                                        Position::none(),
                                    )
                                    .into()
                                }).map(Dynamic::from)
                            }
                        } else {
                            Ok(Dynamic::from(x.pow(y as u32)))
                        }
                    }

                    #[rhai_fn(name = "<<", return_raw)]
                    #[inline]
                    pub fn shift_left(x: $arg_type, y: INT) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            if cfg!(not(feature = "only_i32")) && y > (u32::MAX as INT) {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Left-shift by too many bits: {} << {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else if y < 0 {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Left-shift by a negative number: {} << {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else {
                                x.checked_shl(y as u32).ok_or_else(|| {
                                    EvalAltResult::ErrorArithmetic(
                                        format!("Left-shift by too many bits: {} << {}", x, y),
                                        Position::none(),
                                    )
                                    .into()
                                }).map(Dynamic::from)
                            }
                        } else {
                            Ok(Dynamic::from(x << y))
                        }
                    }
                    #[rhai_fn(name = ">>", return_raw)]
                    #[inline]
                    pub fn shift_right(x: $arg_type, y: INT) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            if cfg!(not(feature = "only_i32")) && y > (u32::MAX as INT) {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Right-shift by too many bits: {} >> {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else if y < 0 {
                                EvalAltResult::ErrorArithmetic(
                                    format!("Right-shift by a negative number: {} >> {}", x, y),
                                    Position::none(),
                                )
                                .into()
                            } else {
                                x.checked_shr(y as u32).ok_or_else(|| {
                                    EvalAltResult::ErrorArithmetic(
                                        format!("Right-shift by too many bits: {} >> {}", x, y),
                                        Position::none(),
                                    )
                                    .into()
                                }).map(Dynamic::from)
                            }
                        } else {
                            Ok(Dynamic::from(x >> y))
                        }
                    }
                    #[rhai_fn(name = "&")]
                    #[inline(always)]
                    fn binary_and(x: $arg_type, y: $arg_type) -> $arg_type {
                        x & y
                    }
                    #[rhai_fn(name = "|")]
                    #[inline(always)]
                    fn binary_or(x: $arg_type, y: $arg_type) -> $arg_type {
                        x | y
                    }
                    #[rhai_fn(name = "^")]
                    #[inline(always)]
                    fn binary_xor(x: $arg_type, y: $arg_type) -> $arg_type {
                        x ^ y
                    }
                }
            }
        )* }
    }
}

macro_rules! gen_signed_functions {
    ($root:ident => $($arg_type:ident),+) => {
        pub mod $root { $(
            pub mod $arg_type {
                use super::super::*;

                #[export_module]
                pub mod functions {
                    #[rhai_fn(name = "-", return_raw)]
                    #[inline]
                    pub fn neg(x: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            x.checked_neg().ok_or_else(|| {
                                EvalAltResult::ErrorArithmetic(format!("Negation overflow: -{}", x), Position::none())
                                    .into()
                            }).map(Dynamic::from)
                        } else {
                            Ok(Dynamic::from(-x))
                        }
                    }
                    #[rhai_fn(return_raw)]
                    #[inline]
                    pub fn abs(x: $arg_type) -> Result<Dynamic, Box<EvalAltResult>> {
                        if cfg!(not(feature = "unchecked")) {
                            x.checked_abs().ok_or_else(|| {
                                EvalAltResult::ErrorArithmetic(format!("Negation overflow: -{}", x), Position::none())
                                    .into()
                            }).map(Dynamic::from)
                        } else {
                            Ok(Dynamic::from(x.abs()))
                        }
                    }
                    #[inline]
                    pub fn sign(x: $arg_type) -> INT {
                        if x == 0 {
                            0
                        } else if x < 0 {
                            -1
                        } else {
                            1
                        }
                    }
                }
            }
        )* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+ ) => {
        $($mod_name.combine_flatten(exported_module!($root::$arg_type::functions));)*
    }
}

def_package!(crate:ArithmeticPackage:"Basic arithmetic", lib, {
    reg_functions!(lib += signed_basic; INT);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += arith_numbers; i8, u8, i16, u16, i32, u32, u64);
        reg_functions!(lib += signed_numbers; i8, i16, i32);

        #[cfg(not(target_arch = "wasm32"))]
        {
            reg_functions!(lib += arith_num_128; i128, u128);
            reg_functions!(lib += signed_num_128; i128);
        }
    }

    // Basic arithmetic for floating-point
    #[cfg(not(feature = "no_float"))]
    lib.combine_flatten(exported_module!(float_functions));
});

gen_arithmetic_functions!(arith_basic => INT);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_arithmetic_functions!(arith_numbers => i8, u8, i16, u16, i32, u32, u64);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_arithmetic_functions!(arith_num_128 => i128, u128);

gen_signed_functions!(signed_basic => INT);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_signed_functions!(signed_numbers => i8, i16, i32);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_signed_functions!(signed_num_128 => i128);

#[cfg(not(feature = "no_float"))]
#[export_module]
mod float_functions {
    #[rhai_fn(name = "+")]
    #[inline(always)]
    pub fn add(x: f32, y: f32) -> f32 {
        x + y
    }
    #[rhai_fn(name = "-")]
    #[inline(always)]
    pub fn subtract(x: f32, y: f32) -> f32 {
        x - y
    }
    #[rhai_fn(name = "*")]
    #[inline(always)]
    pub fn multiply(x: f32, y: f32) -> f32 {
        x * y
    }
    #[rhai_fn(name = "/")]
    #[inline(always)]
    pub fn divide(x: f32, y: f32) -> f32 {
        x / y
    }
    #[rhai_fn(name = "%")]
    #[inline(always)]
    pub fn modulo(x: f32, y: f32) -> f32 {
        x % y
    }
    #[rhai_fn(name = "-")]
    #[inline(always)]
    pub fn neg_f32(x: f32) -> f32 {
        -x
    }
    #[rhai_fn(name = "-")]
    #[inline(always)]
    pub fn neg_f64(x: f64) -> f64 {
        -x
    }
    #[rhai_fn(name = "abs")]
    #[inline(always)]
    pub fn abs_f32(x: f32) -> f32 {
        x.abs()
    }
    #[rhai_fn(name = "abs")]
    #[inline(always)]
    pub fn abs_f64(x: f64) -> f64 {
        x.abs()
    }
    #[rhai_fn(name = "sign")]
    #[inline]
    pub fn sign_f32(x: f32) -> INT {
        if x == 0.0 {
            0
        } else if x < 0.0 {
            -1
        } else {
            1
        }
    }
    #[rhai_fn(name = "sign")]
    #[inline]
    pub fn sign_f64(x: f64) -> INT {
        if x == 0.0 {
            0
        } else if x < 0.0 {
            -1
        } else {
            1
        }
    }
    #[rhai_fn(name = "~", return_raw)]
    #[inline(always)]
    pub fn pow_f_f(x: FLOAT, y: FLOAT) -> Result<Dynamic, Box<EvalAltResult>> {
        Ok(x.powf(y).into())
    }
    #[rhai_fn(name = "~", return_raw)]
    #[inline]
    pub fn pow_f_i(x: FLOAT, y: INT) -> Result<Dynamic, Box<EvalAltResult>> {
        if cfg!(not(feature = "unchecked")) && y > (i32::MAX as INT) {
            EvalAltResult::ErrorArithmetic(
                format!("Number raised to too large an index: {} ~ {}", x, y),
                Position::none(),
            )
            .into()
        } else {
            Ok(x.powi(y as i32).into())
        }
    }
}
