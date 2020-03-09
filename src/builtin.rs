//! Helper module that allows registration of the _core library_ and
//! _standard library_ of utility functions.

use crate::any::Any;
use crate::engine::{Array, Engine};
use crate::fn_register::RegisterFn;
use std::fmt::{Debug, Display};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Range, Rem, Sub};
use std::{i32, i64, u32};

#[cfg(feature = "unchecked")]
use std::ops::{Shl, Shr};

#[cfg(not(feature = "unchecked"))]
use crate::{parser::Position, result::EvalAltResult, RegisterResultFn};

#[cfg(not(feature = "unchecked"))]
use std::convert::TryFrom;

#[cfg(not(feature = "unchecked"))]
use num_traits::{
    CheckedAdd, CheckedDiv, CheckedMul, CheckedNeg, CheckedRem, CheckedShl, CheckedShr, CheckedSub,
};

macro_rules! reg_op {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y, y: $y)->$y);
        )*
    )
}

#[cfg(not(feature = "unchecked"))]
macro_rules! reg_op_result {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_result_fn($x, $op as fn(x: $y, y: $y)->Result<$y,EvalAltResult>);
        )*
    )
}

#[cfg(not(feature = "unchecked"))]
macro_rules! reg_op_result1 {
    ($self:expr, $x:expr, $op:expr, $v:ty, $( $y:ty ),*) => (
        $(
            $self.register_result_fn($x, $op as fn(x: $y, y: $v)->Result<$y,EvalAltResult>);
        )*
    )
}

macro_rules! reg_un {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y)->$y);
        )*
    )
}

#[cfg(not(feature = "unchecked"))]
macro_rules! reg_un_result {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_result_fn($x, $op as fn(x: $y)->Result<$y,EvalAltResult>);
        )*
    )
}
macro_rules! reg_cmp {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y, y: $y)->bool);
        )*
    )
}

macro_rules! reg_func1 {
    ($self:expr, $x:expr, $op:expr, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y)->$r);
        )*
    )
}

#[cfg(not(feature = "no_stdlib"))]
macro_rules! reg_func2x {
    ($self:expr, $x:expr, $op:expr, $v:ty, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $v, y: $y)->$r);
        )*
    )
}

#[cfg(not(feature = "no_stdlib"))]
macro_rules! reg_func2y {
    ($self:expr, $x:expr, $op:expr, $v:ty, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(y: $y, x: $v)->$r);
        )*
    )
}

#[cfg(not(feature = "no_stdlib"))]
macro_rules! reg_func3 {
    ($self:expr, $x:expr, $op:expr, $v:ty, $w:ty, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $v, y: $w, z: $y)->$r);
        )*
    )
}

impl Engine<'_> {
    /// Register the core built-in library.
    pub(crate) fn register_core_lib(&mut self) {
        #[cfg(not(feature = "unchecked"))]
        fn add<T: Display + CheckedAdd>(x: T, y: T) -> Result<T, EvalAltResult> {
            x.checked_add(&y).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Addition overflow: {} + {}", x, y),
                    Position::none(),
                )
            })
        }
        #[cfg(not(feature = "unchecked"))]
        fn sub<T: Display + CheckedSub>(x: T, y: T) -> Result<T, EvalAltResult> {
            x.checked_sub(&y).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Subtraction underflow: {} - {}", x, y),
                    Position::none(),
                )
            })
        }
        #[cfg(not(feature = "unchecked"))]
        fn mul<T: Display + CheckedMul>(x: T, y: T) -> Result<T, EvalAltResult> {
            x.checked_mul(&y).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Multiplication overflow: {} * {}", x, y),
                    Position::none(),
                )
            })
        }
        #[cfg(not(feature = "unchecked"))]
        fn div<T>(x: T, y: T) -> Result<T, EvalAltResult>
        where
            T: Display + CheckedDiv + PartialEq + TryFrom<i8>,
        {
            if y == <T as TryFrom<i8>>::try_from(0)
                .map_err(|_| ())
                .expect("zero should always succeed")
            {
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
        #[cfg(not(feature = "unchecked"))]
        fn neg<T: Display + CheckedNeg>(x: T) -> Result<T, EvalAltResult> {
            x.checked_neg().ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Negation overflow: -{}", x),
                    Position::none(),
                )
            })
        }
        #[cfg(not(feature = "unchecked"))]
        fn abs<T: Display + CheckedNeg + PartialOrd + From<i8>>(x: T) -> Result<T, EvalAltResult> {
            if x >= 0.into() {
                Ok(x)
            } else {
                x.checked_neg().ok_or_else(|| {
                    EvalAltResult::ErrorArithmetic(
                        format!("Negation overflow: -{}", x),
                        Position::none(),
                    )
                })
            }
        }
        fn add_u<T: Add>(x: T, y: T) -> <T as Add>::Output {
            x + y
        }
        fn sub_u<T: Sub>(x: T, y: T) -> <T as Sub>::Output {
            x - y
        }
        fn mul_u<T: Mul>(x: T, y: T) -> <T as Mul>::Output {
            x * y
        }
        fn div_u<T: Div>(x: T, y: T) -> <T as Div>::Output {
            x / y
        }
        fn neg_u<T: Neg>(x: T) -> <T as Neg>::Output {
            -x
        }
        fn abs_u<T: Neg + PartialOrd + From<i8>>(x: T) -> T
        where
            <T as Neg>::Output: Into<T>,
        {
            if x < 0.into() {
                (-x).into()
            } else {
                x
            }
        }
        fn lt<T: PartialOrd>(x: T, y: T) -> bool {
            x < y
        }
        fn lte<T: PartialOrd>(x: T, y: T) -> bool {
            x <= y
        }
        fn gt<T: PartialOrd>(x: T, y: T) -> bool {
            x > y
        }
        fn gte<T: PartialOrd>(x: T, y: T) -> bool {
            x >= y
        }
        fn eq<T: PartialEq>(x: T, y: T) -> bool {
            x == y
        }
        fn ne<T: PartialEq>(x: T, y: T) -> bool {
            x != y
        }
        fn and(x: bool, y: bool) -> bool {
            x && y
        }
        fn or(x: bool, y: bool) -> bool {
            x || y
        }
        fn not(x: bool) -> bool {
            !x
        }
        fn binary_and<T: BitAnd>(x: T, y: T) -> <T as BitAnd>::Output {
            x & y
        }
        fn binary_or<T: BitOr>(x: T, y: T) -> <T as BitOr>::Output {
            x | y
        }
        fn binary_xor<T: BitXor>(x: T, y: T) -> <T as BitXor>::Output {
            x ^ y
        }
        #[cfg(not(feature = "unchecked"))]
        fn shl<T: Display + CheckedShl>(x: T, y: i64) -> Result<T, EvalAltResult> {
            if y < 0 {
                return Err(EvalAltResult::ErrorArithmetic(
                    format!("Left-shift by a negative number: {} << {}", x, y),
                    Position::none(),
                ));
            }

            CheckedShl::checked_shl(&x, y as u32).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Left-shift overflow: {} << {}", x, y),
                    Position::none(),
                )
            })
        }
        #[cfg(not(feature = "unchecked"))]
        fn shr<T: Display + CheckedShr>(x: T, y: i64) -> Result<T, EvalAltResult> {
            if y < 0 {
                return Err(EvalAltResult::ErrorArithmetic(
                    format!("Right-shift by a negative number: {} >> {}", x, y),
                    Position::none(),
                ));
            }

            CheckedShr::checked_shr(&x, y as u32).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Right-shift overflow: {} % {}", x, y),
                    Position::none(),
                )
            })
        }
        #[cfg(feature = "unchecked")]
        fn shl_u<T: Shl<T>>(x: T, y: T) -> <T as Shl<T>>::Output {
            x.shl(y)
        }
        #[cfg(feature = "unchecked")]
        fn shr_u<T: Shr<T>>(x: T, y: T) -> <T as Shr<T>>::Output {
            x.shr(y)
        }
        #[cfg(not(feature = "unchecked"))]
        fn modulo<T: Display + CheckedRem>(x: T, y: T) -> Result<T, EvalAltResult> {
            x.checked_rem(&y).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Modulo division overflow: {} % {}", x, y),
                    Position::none(),
                )
            })
        }
        fn modulo_u<T: Rem>(x: T, y: T) -> <T as Rem>::Output {
            x % y
        }
        #[cfg(not(feature = "unchecked"))]
        fn pow_i64_i64_u(x: i64, y: i64) -> Result<i64, EvalAltResult> {
            if y > (u32::MAX as i64) {
                return Err(EvalAltResult::ErrorArithmetic(
                    format!("Power overflow: {} ~ {}", x, y),
                    Position::none(),
                ));
            }

            x.checked_pow(y as u32).ok_or_else(|| {
                EvalAltResult::ErrorArithmetic(
                    format!("Power overflow: {} ~ {}", x, y),
                    Position::none(),
                )
            })
        }
        #[cfg(feature = "unchecked")]
        fn pow_i64_i64(x: i64, y: i64) -> i64 {
            x.powi(y as u32)
        }
        fn pow_f64_f64(x: f64, y: f64) -> f64 {
            x.powf(y)
        }
        #[cfg(not(feature = "unchecked"))]
        fn pow_f64_i64_u(x: f64, y: i64) -> Result<f64, EvalAltResult> {
            if y > (i32::MAX as i64) {
                return Err(EvalAltResult::ErrorArithmetic(
                    format!("Power overflow: {} ~ {}", x, y),
                    Position::none(),
                ));
            }

            Ok(x.powi(y as i32))
        }
        #[cfg(feature = "unchecked")]
        fn pow_f64_i64(x: f64, y: i64) -> f64 {
            x.powi(y as i32)
        }

        #[cfg(not(feature = "unchecked"))]
        {
            reg_op_result!(self, "+", add, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op_result!(self, "-", sub, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op_result!(self, "*", mul, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op_result!(self, "/", div, i8, u8, i16, u16, i32, i64, u32, u64);
        }

        #[cfg(feature = "unchecked")]
        {
            reg_op!(self, "+", add_u, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op!(self, "-", sub_u, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op!(self, "*", mul_u, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op!(self, "/", div_u, i8, u8, i16, u16, i32, i64, u32, u64);
        }

        reg_op!(self, "+", add_u, f32, f64);
        reg_op!(self, "-", sub_u, f32, f64);
        reg_op!(self, "*", mul_u, f32, f64);
        reg_op!(self, "/", div_u, f32, f64);

        reg_cmp!(self, "<", lt, i8, u8, i16, u16, i32, i64, u32, u64, f32, f64, String, char);
        reg_cmp!(self, "<=", lte, i8, u8, i16, u16, i32, i64, u32, u64, f32, f64, String, char);
        reg_cmp!(self, ">", gt, i8, u8, i16, u16, i32, i64, u32, u64, f32, f64, String, char);
        reg_cmp!(self, ">=", gte, i8, u8, i16, u16, i32, i64, u32, u64, f32, f64, String, char);
        reg_cmp!(
            self, "==", eq, i8, u8, i16, u16, i32, i64, u32, u64, bool, f32, f64, String, char
        );
        reg_cmp!(
            self, "!=", ne, i8, u8, i16, u16, i32, i64, u32, u64, bool, f32, f64, String, char
        );

        //reg_op!(self, "||", or, bool);
        //reg_op!(self, "&&", and, bool);
        reg_op!(self, "|", binary_or, i8, u8, i16, u16, i32, i64, u32, u64);
        reg_op!(self, "|", or, bool);
        reg_op!(self, "&", binary_and, i8, u8, i16, u16, i32, i64, u32, u64);
        reg_op!(self, "&", and, bool);
        reg_op!(self, "^", binary_xor, i8, u8, i16, u16, i32, i64, u32, u64);

        #[cfg(not(feature = "unchecked"))]
        {
            reg_op_result1!(self, "<<", shl, i64, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op_result1!(self, ">>", shr, i64, i8, u8, i16, u16);
            reg_op_result1!(self, ">>", shr, i64, i32, i64, u32, u64);
            reg_op_result!(self, "%", modulo, i8, u8, i16, u16, i32, i64, u32, u64);
        }

        #[cfg(feature = "unchecked")]
        {
            reg_op!(self, "<<", shl_u, i64, i8, u8, i16, u16, i32, i64, u32, u64);
            reg_op!(self, ">>", shr_u, i64, i8, u8, i16, u16);
            reg_op!(self, ">>", shr_u, i64, i32, i64, u32, u64);
            reg_op!(self, "%", modulo_u, i8, u8, i16, u16, i32, i64, u32, u64);
        }

        reg_op!(self, "%", modulo_u, f32, f64);

        self.register_fn("~", pow_f64_f64);

        #[cfg(not(feature = "unchecked"))]
        {
            self.register_result_fn("~", pow_i64_i64_u);
            self.register_result_fn("~", pow_f64_i64_u);
        }

        #[cfg(feature = "unchecked")]
        {
            self.register_fn("~", pow_i64_i64);
            self.register_fn("~", pow_f64_i64);
        }

        #[cfg(not(feature = "unchecked"))]
        {
            reg_un_result!(self, "-", neg, i8, i16, i32, i64);
            reg_un_result!(self, "abs", abs, i8, i16, i32, i64);
        }

        #[cfg(feature = "unchecked")]
        {
            reg_un!(self, "-", neg_u, i8, i16, i32, i64);
            reg_un!(self, "abs", abs_u, i8, i16, i32, i64);
        }

        reg_un!(self, "-", neg_u, f32, f64);
        reg_un!(self, "abs", abs_u, f32, f64);
        reg_un!(self, "!", not, bool);

        self.register_fn("+", |x: String, y: String| x + &y); // String + String
        self.register_fn("==", |_: (), _: ()| true); // () == ()

        // Register print and debug
        fn print_debug<T: Debug>(x: T) -> String {
            format!("{:?}", x)
        }
        fn print<T: Display>(x: T) -> String {
            format!("{}", x)
        }

        reg_func1!(self, "print", print, String, i8, u8, i16, u16);
        reg_func1!(self, "print", print, String, i32, i64, u32, u64);
        reg_func1!(self, "print", print, String, f32, f64, bool, char, String);
        reg_func1!(self, "print", print_debug, String, Array);
        self.register_fn("print", || "".to_string());
        self.register_fn("print", |_: ()| "".to_string());

        reg_func1!(self, "debug", print_debug, String, i8, u8, i16, u16);
        reg_func1!(self, "debug", print_debug, String, i32, i64, u32, u64);
        reg_func1!(self, "debug", print_debug, String, f32, f64, bool, char);
        reg_func1!(self, "debug", print_debug, String, String, Array, ());

        // Register array iterator
        self.register_iterator::<Array, _>(|a| {
            Box::new(a.downcast_ref::<Array>().unwrap().clone().into_iter())
        });

        // Register range function
        self.register_iterator::<Range<i64>, _>(|a| {
            Box::new(
                a.downcast_ref::<Range<i64>>()
                    .unwrap()
                    .clone()
                    .map(|n| n.into_dynamic()),
            )
        });

        self.register_fn("range", |i1: i64, i2: i64| (i1..i2));
    }

    /// Register the built-in library.
    #[cfg(not(feature = "no_stdlib"))]
    pub(crate) fn register_stdlib(&mut self) {
        use crate::fn_register::RegisterDynamicFn;

        // Advanced math functions
        self.register_fn("sin", |x: f64| x.to_radians().sin());
        self.register_fn("cos", |x: f64| x.to_radians().cos());
        self.register_fn("tan", |x: f64| x.to_radians().tan());
        self.register_fn("sinh", |x: f64| x.to_radians().sinh());
        self.register_fn("cosh", |x: f64| x.to_radians().cosh());
        self.register_fn("tanh", |x: f64| x.to_radians().tanh());
        self.register_fn("asin", |x: f64| x.asin().to_degrees());
        self.register_fn("acos", |x: f64| x.acos().to_degrees());
        self.register_fn("atan", |x: f64| x.atan().to_degrees());
        self.register_fn("asinh", |x: f64| x.asinh().to_degrees());
        self.register_fn("acosh", |x: f64| x.acosh().to_degrees());
        self.register_fn("atanh", |x: f64| x.atanh().to_degrees());
        self.register_fn("sqrt", |x: f64| x.sqrt());
        self.register_fn("exp", |x: f64| x.exp());
        self.register_fn("ln", |x: f64| x.ln());
        self.register_fn("log", |x: f64, base: f64| x.log(base));
        self.register_fn("log10", |x: f64| x.log10());
        self.register_fn("floor", |x: f64| x.floor());
        self.register_fn("ceiling", |x: f64| x.ceil());
        self.register_fn("round", |x: f64| x.ceil());
        self.register_fn("int", |x: f64| x.trunc());
        self.register_fn("fraction", |x: f64| x.fract());
        self.register_fn("is_nan", |x: f64| x.is_nan());
        self.register_fn("is_finite", |x: f64| x.is_finite());
        self.register_fn("is_infinite", |x: f64| x.is_infinite());

        // Register conversion functions
        self.register_fn("to_float", |x: i8| x as f64);
        self.register_fn("to_float", |x: u8| x as f64);
        self.register_fn("to_float", |x: i16| x as f64);
        self.register_fn("to_float", |x: u16| x as f64);
        self.register_fn("to_float", |x: i32| x as f64);
        self.register_fn("to_float", |x: u32| x as f64);
        self.register_fn("to_float", |x: i64| x as f64);
        self.register_fn("to_float", |x: u64| x as f64);
        self.register_fn("to_float", |x: f32| x as f64);

        self.register_fn("to_int", |x: i8| x as i64);
        self.register_fn("to_int", |x: u8| x as i64);
        self.register_fn("to_int", |x: i16| x as i64);
        self.register_fn("to_int", |x: u16| x as i64);
        self.register_fn("to_int", |x: i32| x as i64);
        self.register_fn("to_int", |x: u32| x as i64);
        self.register_fn("to_int", |x: u64| x as i64);
        self.register_fn("to_int", |ch: char| ch as i64);

        #[cfg(not(feature = "unchecked"))]
        {
            self.register_result_fn("to_int", |x: f32| {
                if x > (i64::MAX as f32) {
                    return Err(EvalAltResult::ErrorArithmetic(
                        format!("Integer overflow: to_int({})", x),
                        Position::none(),
                    ));
                }

                Ok(x.trunc() as i64)
            });
            self.register_result_fn("to_int", |x: f64| {
                if x > (i64::MAX as f64) {
                    return Err(EvalAltResult::ErrorArithmetic(
                        format!("Integer overflow: to_int({})", x),
                        Position::none(),
                    ));
                }

                Ok(x.trunc() as i64)
            });
        }

        #[cfg(feature = "unchecked")]
        {
            self.register_fn("to_int", |x: f32| x as i64);
            self.register_fn("to_int", |x: f64| x as i64);
        }

        // Register array utility functions
        fn push<T: Any>(list: &mut Array, item: T) {
            list.push(Box::new(item));
        }
        fn pad<T: Any + Clone>(list: &mut Array, len: i64, item: T) {
            if len >= 0 {
                while list.len() < len as usize {
                    push(list, item.clone());
                }
            }
        }

        reg_func2x!(self, "push", push, &mut Array, (), i8, u8, i16, u16);
        reg_func2x!(self, "push", push, &mut Array, (), i32, i64, u32, u64);
        reg_func2x!(self, "push", push, &mut Array, (), f32, f64, bool, char);
        reg_func2x!(self, "push", push, &mut Array, (), String, Array, ());
        reg_func3!(self, "pad", pad, &mut Array, i64, (), i8, u8, i16, u16);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), i32, u32, f32);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), i64, u64, f64);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), bool, char);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), String, Array, ());

        self.register_dynamic_fn("pop", |list: &mut Array| {
            list.pop().unwrap_or_else(|| ().into_dynamic())
        });
        self.register_dynamic_fn("shift", |list: &mut Array| match list.len() {
            0 => ().into_dynamic(),
            _ => list.remove(0),
        });
        self.register_fn("len", |list: &mut Array| list.len() as i64);
        self.register_fn("clear", |list: &mut Array| list.clear());
        self.register_fn("truncate", |list: &mut Array, len: i64| {
            if len >= 0 {
                list.truncate(len as usize);
            }
        });

        // Register string concatenate functions
        fn prepend<T: Display>(x: T, y: String) -> String {
            format!("{}{}", x, y)
        }
        fn append<T: Display>(x: String, y: T) -> String {
            format!("{}{}", x, y)
        }

        reg_func2x!(
            self, "+", append, String, String, i8, u8, i16, u16, i32, i64, u32, u64, f32, f64,
            bool, char
        );
        self.register_fn("+", |x: String, y: Array| format!("{}{:?}", x, y));
        self.register_fn("+", |x: String, _: ()| format!("{}", x));

        reg_func2y!(
            self, "+", prepend, String, String, i8, u8, i16, u16, i32, i64, u32, u64, f32, f64,
            bool, char
        );
        self.register_fn("+", |x: Array, y: String| format!("{:?}{}", x, y));
        self.register_fn("+", |_: (), y: String| format!("{}", y));

        // Register string utility functions
        self.register_fn("len", |s: &mut String| s.chars().count() as i64);
        self.register_fn("contains", |s: &mut String, ch: char| s.contains(ch));
        self.register_fn("contains", |s: &mut String, find: String| s.contains(&find));
        self.register_fn("clear", |s: &mut String| s.clear());
        self.register_fn("append", |s: &mut String, ch: char| s.push(ch));
        self.register_fn("append", |s: &mut String, add: String| s.push_str(&add));
        self.register_fn("truncate", |s: &mut String, len: i64| {
            if len >= 0 {
                let chars: Vec<_> = s.chars().take(len as usize).collect();
                s.clear();
                chars.iter().for_each(|&ch| s.push(ch));
            } else {
                s.clear();
            }
        });
        self.register_fn("pad", |s: &mut String, len: i64, ch: char| {
            for _ in 0..s.chars().count() - len as usize {
                s.push(ch);
            }
        });
        self.register_fn("replace", |s: &mut String, find: String, sub: String| {
            let new_str = s.replace(&find, &sub);
            s.clear();
            s.push_str(&new_str);
        });
        self.register_fn("trim", |s: &mut String| {
            let trimmed = s.trim();

            if trimmed.len() < s.len() {
                let chars: Vec<_> = trimmed.chars().collect();
                s.clear();
                chars.iter().for_each(|&ch| s.push(ch));
            }
        });
    }
}
