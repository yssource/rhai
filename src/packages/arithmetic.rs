use super::{reg_binary, reg_unary};

use crate::def_package;
use crate::fn_register::{map_dynamic as map, map_result as result};
use crate::parser::INT;
use crate::result::EvalAltResult;
use crate::token::Position;

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

use num_traits::{
    identities::Zero, CheckedAdd, CheckedDiv, CheckedMul, CheckedNeg, CheckedRem, CheckedShl,
    CheckedShr, CheckedSub,
};

use crate::stdlib::{
    fmt::Display,
    format,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Shl, Shr, Sub},
    {i32, i64, u32},
};

// Checked add
fn add<T: Display + CheckedAdd>(x: T, y: T) -> Result<T, EvalAltResult> {
    x.checked_add(&y).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Addition overflow: {} + {}", x, y),
            Position::none(),
        )
    })
}
// Checked subtract
fn sub<T: Display + CheckedSub>(x: T, y: T) -> Result<T, EvalAltResult> {
    x.checked_sub(&y).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Subtraction underflow: {} - {}", x, y),
            Position::none(),
        )
    })
}
// Checked multiply
fn mul<T: Display + CheckedMul>(x: T, y: T) -> Result<T, EvalAltResult> {
    x.checked_mul(&y).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Multiplication overflow: {} * {}", x, y),
            Position::none(),
        )
    })
}
// Checked divide
fn div<T>(x: T, y: T) -> Result<T, EvalAltResult>
where
    T: Display + CheckedDiv + PartialEq + Zero,
{
    // Detect division by zero
    if y == T::zero() {
        return Err(EvalAltResult::ErrorArithmetic(
            format!("Division by zero: {} / {}", x, y),
            Position::none(),
        ));
    }

    x.checked_div(&y).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Division overflow: {} / {}", x, y),
            Position::none(),
        )
    })
}
// Checked negative - e.g. -(i32::MIN) will overflow i32::MAX
fn neg<T: Display + CheckedNeg>(x: T) -> Result<T, EvalAltResult> {
    x.checked_neg().ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(format!("Negation overflow: -{}", x), Position::none())
    })
}
// Checked absolute
fn abs<T: Display + CheckedNeg + PartialOrd + Zero>(x: T) -> Result<T, EvalAltResult> {
    // FIX - We don't use Signed::abs() here because, contrary to documentation, it panics
    //       when the number is ::MIN instead of returning ::MIN itself.
    if x >= <T as Zero>::zero() {
        Ok(x)
    } else {
        x.checked_neg().ok_or_else(|| {
            EvalAltResult::ErrorArithmetic(format!("Negation overflow: -{}", x), Position::none())
        })
    }
}
// Unchecked add - may panic on overflow
fn add_u<T: Add>(x: T, y: T) -> <T as Add>::Output {
    x + y
}
// Unchecked subtract - may panic on underflow
fn sub_u<T: Sub>(x: T, y: T) -> <T as Sub>::Output {
    x - y
}
// Unchecked multiply - may panic on overflow
fn mul_u<T: Mul>(x: T, y: T) -> <T as Mul>::Output {
    x * y
}
// Unchecked divide - may panic when dividing by zero
fn div_u<T: Div>(x: T, y: T) -> <T as Div>::Output {
    x / y
}
// Unchecked negative - may panic on overflow
fn neg_u<T: Neg>(x: T) -> <T as Neg>::Output {
    -x
}
// Unchecked absolute - may panic on overflow
fn abs_u<T>(x: T) -> <T as Neg>::Output
where
    T: Neg + PartialOrd + Default + Into<<T as Neg>::Output>,
{
    // Numbers should default to zero
    if x < Default::default() {
        -x
    } else {
        x.into()
    }
}
// Bit operators
fn binary_and<T: BitAnd>(x: T, y: T) -> <T as BitAnd>::Output {
    x & y
}
fn binary_or<T: BitOr>(x: T, y: T) -> <T as BitOr>::Output {
    x | y
}
fn binary_xor<T: BitXor>(x: T, y: T) -> <T as BitXor>::Output {
    x ^ y
}
// Checked left-shift
fn shl<T: Display + CheckedShl>(x: T, y: INT) -> Result<T, EvalAltResult> {
    // Cannot shift by a negative number of bits
    if y < 0 {
        return Err(EvalAltResult::ErrorArithmetic(
            format!("Left-shift by a negative number: {} << {}", x, y),
            Position::none(),
        ));
    }

    CheckedShl::checked_shl(&x, y as u32).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Left-shift by too many bits: {} << {}", x, y),
            Position::none(),
        )
    })
}
// Checked right-shift
fn shr<T: Display + CheckedShr>(x: T, y: INT) -> Result<T, EvalAltResult> {
    // Cannot shift by a negative number of bits
    if y < 0 {
        return Err(EvalAltResult::ErrorArithmetic(
            format!("Right-shift by a negative number: {} >> {}", x, y),
            Position::none(),
        ));
    }

    CheckedShr::checked_shr(&x, y as u32).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Right-shift by too many bits: {} % {}", x, y),
            Position::none(),
        )
    })
}
// Unchecked left-shift - may panic if shifting by a negative number of bits
fn shl_u<T: Shl<T>>(x: T, y: T) -> <T as Shl<T>>::Output {
    x.shl(y)
}
// Unchecked right-shift - may panic if shifting by a negative number of bits
fn shr_u<T: Shr<T>>(x: T, y: T) -> <T as Shr<T>>::Output {
    x.shr(y)
}
// Checked modulo
fn modulo<T: Display + CheckedRem>(x: T, y: T) -> Result<T, EvalAltResult> {
    x.checked_rem(&y).ok_or_else(|| {
        EvalAltResult::ErrorArithmetic(
            format!("Modulo division by zero or overflow: {} % {}", x, y),
            Position::none(),
        )
    })
}
// Unchecked modulo - may panic if dividing by zero
fn modulo_u<T: Rem>(x: T, y: T) -> <T as Rem>::Output {
    x % y
}
// Checked power
fn pow_i_i(x: INT, y: INT) -> Result<INT, EvalAltResult> {
    #[cfg(not(feature = "only_i32"))]
    {
        if y > (u32::MAX as INT) {
            Err(EvalAltResult::ErrorArithmetic(
                format!("Integer raised to too large an index: {} ~ {}", x, y),
                Position::none(),
            ))
        } else if y < 0 {
            Err(EvalAltResult::ErrorArithmetic(
                format!("Integer raised to a negative index: {} ~ {}", x, y),
                Position::none(),
            ))
        } else {
            x.checked_pow(y as u32).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Power overflow: {} ~ {}", x, y),
                    Position::none(),
                )
            })
        }
    }

    #[cfg(feature = "only_i32")]
    {
        if y < 0 {
            Err(EvalAltResult::ErrorArithmetic(
                format!("Integer raised to a negative index: {} ~ {}", x, y),
                Position::none(),
            ))
        } else {
            x.checked_pow(y as u32).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Power overflow: {} ~ {}", x, y),
                    Position::none(),
                )
            })
        }
    }
}
// Unchecked integer power - may panic on overflow or if the power index is too high (> u32::MAX)
fn pow_i_i_u(x: INT, y: INT) -> INT {
    x.pow(y as u32)
}
// Floating-point power - always well-defined
#[cfg(not(feature = "no_float"))]
fn pow_f_f(x: FLOAT, y: FLOAT) -> FLOAT {
    x.powf(y)
}
// Checked power
#[cfg(not(feature = "no_float"))]
fn pow_f_i(x: FLOAT, y: INT) -> Result<FLOAT, EvalAltResult> {
    // Raise to power that is larger than an i32
    if y > (i32::MAX as INT) {
        return Err(EvalAltResult::ErrorArithmetic(
            format!("Number raised to too large an index: {} ~ {}", x, y),
            Position::none(),
        ));
    }

    Ok(x.powi(y as i32))
}
// Unchecked power - may be incorrect if the power index is too high (> i32::MAX)
#[cfg(feature = "unchecked")]
#[cfg(not(feature = "no_float"))]
fn pow_f_i_u(x: FLOAT, y: INT) -> FLOAT {
    x.powi(y as i32)
}

macro_rules! reg_unary_x { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_unary($lib, $op, $func::<$par>, result);)* };
}
macro_rules! reg_unary { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_unary($lib, $op, $func::<$par>, map);)* };
}
macro_rules! reg_op_x { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_binary($lib, $op, $func::<$par>, result);)* };
}
macro_rules! reg_op { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_binary($lib, $op, $func::<$par>, map);)* };
}

def_package!(ArithmeticPackage:"Basic arithmetic", lib, {
    // Checked basic arithmetic
    #[cfg(not(feature = "unchecked"))]
    {
        reg_op_x!(lib, "+", add, INT);
        reg_op_x!(lib, "-", sub, INT);
        reg_op_x!(lib, "*", mul, INT);
        reg_op_x!(lib, "/", div, INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_op_x!(lib, "+", add, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op_x!(lib, "-", sub, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op_x!(lib, "*", mul, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op_x!(lib, "/", div, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        }
    }

    // Unchecked basic arithmetic
    #[cfg(feature = "unchecked")]
    {
        reg_op!(lib, "+", add_u, INT);
        reg_op!(lib, "-", sub_u, INT);
        reg_op!(lib, "*", mul_u, INT);
        reg_op!(lib, "/", div_u, INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_op!(lib, "+", add_u, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op!(lib, "-", sub_u, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op!(lib, "*", mul_u, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op!(lib, "/", div_u, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        }
    }

    // Basic arithmetic for floating-point - no need to check
    #[cfg(not(feature = "no_float"))]
    {
        reg_op!(lib, "+", add_u, f32, f64);
        reg_op!(lib, "-", sub_u, f32, f64);
        reg_op!(lib, "*", mul_u, f32, f64);
        reg_op!(lib, "/", div_u, f32, f64);
    }

    // Bit operations
    reg_op!(lib, "|", binary_or, INT);
    reg_op!(lib, "&", binary_and, INT);
    reg_op!(lib, "^", binary_xor, INT);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_op!(lib, "|", binary_or, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        reg_op!(lib, "&", binary_and, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        reg_op!(lib, "^", binary_xor, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
    }

    // Checked bit shifts
    #[cfg(not(feature = "unchecked"))]
    {
        reg_op_x!(lib, "<<", shl, INT);
        reg_op_x!(lib, ">>", shr, INT);
        reg_op_x!(lib, "%", modulo, INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_op_x!(lib, "<<", shl, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op_x!(lib, ">>", shr, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op_x!(lib, "%", modulo, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        }
    }

    // Unchecked bit shifts
    #[cfg(feature = "unchecked")]
    {
        reg_op!(lib, "<<", shl_u, INT, INT);
        reg_op!(lib, ">>", shr_u, INT, INT);
        reg_op!(lib, "%", modulo_u, INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_op!(lib, "<<", shl_u, i64, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op!(lib, ">>", shr_u, i64, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            reg_op!(lib, "%", modulo_u, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        }
    }

    // Checked power
    #[cfg(not(feature = "unchecked"))]
    {
        reg_binary(lib, "~", pow_i_i, result);

        #[cfg(not(feature = "no_float"))]
        reg_binary(lib, "~", pow_f_i, result);
    }

    // Unchecked power
    #[cfg(feature = "unchecked")]
    {
        reg_binary(lib, "~", pow_i_i_u, map);

        #[cfg(not(feature = "no_float"))]
        reg_binary(lib, "~", pow_f_i_u, map);
    }

    // Floating-point modulo and power
    #[cfg(not(feature = "no_float"))]
    {
        reg_op!(lib, "%", modulo_u, f32, f64);
        reg_binary(lib, "~", pow_f_f, map);
    }

    // Checked unary
    #[cfg(not(feature = "unchecked"))]
    {
        reg_unary_x!(lib, "-", neg, INT);
        reg_unary_x!(lib, "abs", abs, INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_unary_x!(lib, "-", neg, i8, i16, i32, i64, i128);
            reg_unary_x!(lib, "abs", abs, i8, i16, i32, i64, i128);
        }
    }

    // Unchecked unary
    #[cfg(feature = "unchecked")]
    {
        reg_unary!(lib, "-", neg_u, INT);
        reg_unary!(lib, "abs", abs_u, INT);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_unary!(lib, "-", neg_u, i8, i16, i32, i64, i128);
            reg_unary!(lib, "abs", abs_u, i8, i16, i32, i64, i128);
        }
    }

    // Floating-point unary
    #[cfg(not(feature = "no_float"))]
    {
        reg_unary!(lib, "-", neg_u, f32, f64);
        reg_unary!(lib, "abs", abs_u, f32, f64);
    }
});
