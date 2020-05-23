use crate::def_package;
use crate::module::FuncReturn;
use crate::parser::INT;

use crate::stdlib::string::String;

// Comparison operators
pub fn lt<T: PartialOrd>(x: T, y: T) -> FuncReturn<bool> {
    Ok(x < y)
}
pub fn lte<T: PartialOrd>(x: T, y: T) -> FuncReturn<bool> {
    Ok(x <= y)
}
pub fn gt<T: PartialOrd>(x: T, y: T) -> FuncReturn<bool> {
    Ok(x > y)
}
pub fn gte<T: PartialOrd>(x: T, y: T) -> FuncReturn<bool> {
    Ok(x >= y)
}
pub fn eq<T: PartialEq>(x: T, y: T) -> FuncReturn<bool> {
    Ok(x == y)
}
pub fn ne<T: PartialEq>(x: T, y: T) -> FuncReturn<bool> {
    Ok(x != y)
}

// Logic operators
fn and(x: bool, y: bool) -> FuncReturn<bool> {
    Ok(x && y)
}
fn or(x: bool, y: bool) -> FuncReturn<bool> {
    Ok(x || y)
}
fn not(x: bool) -> FuncReturn<bool> {
    Ok(!x)
}

macro_rules! reg_op {
    ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
        $( $lib.set_fn_2($op, $func::<$par>); )*
    };
}

def_package!(crate:LogicPackage:"Logical operators.", lib, {
    // reg_op!(lib, "<", lt, INT, char);
    // reg_op!(lib, "<=", lte, INT, char);
    // reg_op!(lib, ">", gt, INT, char);
    // reg_op!(lib, ">=", gte, INT, char);
    // reg_op!(lib, "==", eq, INT, char, bool, ());
    // reg_op!(lib, "!=", ne, INT, char, bool, ());

    // Special versions for strings - at least avoid copying the first string
    // lib.set_fn_2_mut("<", |x: &mut String, y: String| Ok(*x < y));
    // lib.set_fn_2_mut("<=", |x: &mut String, y: String| Ok(*x <= y));
    // lib.set_fn_2_mut(">", |x: &mut String, y: String| Ok(*x > y));
    // lib.set_fn_2_mut(">=", |x: &mut String, y: String| Ok(*x >= y));
    // lib.set_fn_2_mut("==", |x: &mut String, y: String| Ok(*x == y));
    // lib.set_fn_2_mut("!=", |x: &mut String, y: String| Ok(*x != y));

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        // reg_op!(lib, "<", lt, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        // reg_op!(lib, "<=", lte, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        // reg_op!(lib, ">", gt, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        // reg_op!(lib, ">=", gte, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        // reg_op!(lib, "==", eq, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        // reg_op!(lib, "!=", ne, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        reg_op!(lib, "<", lt, i8, u8, i16, u16, i32, u32, u64, i128, u128);
        reg_op!(lib, "<=", lte, i8, u8, i16, u16, i32, u32, u64, i128, u128);
        reg_op!(lib, ">", gt, i8, u8, i16, u16, i32, u32, u64, i128, u128);
        reg_op!(lib, ">=", gte, i8, u8, i16, u16, i32, u32, u64, i128, u128);
        reg_op!(lib, "==", eq, i8, u8, i16, u16, i32, u32, u64, i128, u128);
        reg_op!(lib, "!=", ne, i8, u8, i16, u16, i32, u32, u64, i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    {
        // reg_op!(lib, "<", lt, f32, f64);
        // reg_op!(lib, "<=", lte, f32, f64);
        // reg_op!(lib, ">", gt, f32, f64);
        // reg_op!(lib, ">=", gte, f32, f64);
        // reg_op!(lib, "==", eq, f32, f64);
        // reg_op!(lib, "!=", ne, f32, f64);
        reg_op!(lib, "<", lt, f32);
        reg_op!(lib, "<=", lte, f32);
        reg_op!(lib, ">", gt, f32);
        reg_op!(lib, ">=", gte, f32);
        reg_op!(lib, "==", eq, f32);
        reg_op!(lib, "!=", ne, f32);
    }

    // `&&` and `||` are treated specially as they short-circuit.
    // They are implemented as special `Expr` instances, not function calls.
    //reg_op!(lib, "||", or, bool);
    //reg_op!(lib, "&&", and, bool);

    // lib.set_fn_2("|", or);
    // lib.set_fn_2("&", and);
    lib.set_fn_1("!", not);
});
