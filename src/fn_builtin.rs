//! Built-in implementations for common operators.

use crate::fn_native::{FnCallArgs, NativeCallContext};
use crate::stdlib::{any::TypeId, format, string::ToString};
use crate::{Dynamic, ImmutableString, RhaiResult, INT};

#[cfg(not(feature = "no_float"))]
use crate::FLOAT;

#[cfg(feature = "decimal")]
use rust_decimal::Decimal;

#[cfg(feature = "no_std")]
#[cfg(not(feature = "no_float"))]
use num_traits::float::Float;

/// Is the type a numeric type?
#[inline(always)]
fn is_numeric(type_id: TypeId) -> bool {
    let result = type_id == TypeId::of::<u8>()
        || type_id == TypeId::of::<u16>()
        || type_id == TypeId::of::<u32>()
        || type_id == TypeId::of::<u64>()
        || type_id == TypeId::of::<i8>()
        || type_id == TypeId::of::<i16>()
        || type_id == TypeId::of::<i32>()
        || type_id == TypeId::of::<i64>();

    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    let result = result || type_id == TypeId::of::<u128>() || type_id == TypeId::of::<i128>();

    #[cfg(not(feature = "no_float"))]
    let result = result || type_id == TypeId::of::<f32>() || type_id == TypeId::of::<f64>();

    #[cfg(feature = "decimal")]
    let result = result || type_id == TypeId::of::<rust_decimal::Decimal>();

    result
}

/// Build in common binary operator implementations to avoid the cost of calling a registered function.
pub fn get_builtin_binary_op_fn(
    op: &str,
    x: &Dynamic,
    y: &Dynamic,
) -> Option<fn(NativeCallContext, &mut FnCallArgs) -> RhaiResult> {
    let type1 = x.type_id();
    let type2 = y.type_id();

    // One of the operands is a custom type, so it is never built-in
    if x.is_variant() || y.is_variant() {
        if is_numeric(type1) && is_numeric(type2) {
            // Disallow comparisons between different numeric types
            return None;
        }

        // If the types are not the same, default to not compare
        if type1 != type2 {
            return match op {
                "!=" => Some(|_, _| Ok(Dynamic::TRUE)),
                "==" | ">" | ">=" | "<" | "<=" => Some(|_, _| Ok(Dynamic::FALSE)),
                _ => None,
            };
        }

        // Disallow comparisons between the same type
        return None;
    }

    let types_pair = (type1, type2);

    macro_rules! impl_op {
        ($xx:ident $op:tt $yy:ident) => {
            return Some(|_, args| {
                let x = &*args[0].read_lock::<$xx>().unwrap();
                let y = &*args[1].read_lock::<$yy>().unwrap();
                Ok((x $op y).into())
            })
        };
        ($func:ident ( $op:tt )) => {
            return Some(|_, args| {
                let (x, y) = $func(args);
                Ok((x $op y).into())
            })
        };
        ($base:ty => $xx:ident $op:tt $yy:ident) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap() as $base;
                let y = args[1].$yy().unwrap() as $base;
                Ok((x $op y).into())
            })
        };
        ($base:ty => $xx:ident . $func:ident ( $yy:ident as $yyy:ty)) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap() as $base;
                let y = args[1].$yy().unwrap() as $base;
                Ok(x.$func(y as $yyy).into())
            })
        };
        ($base:ty => $func:ident ( $xx:ident, $yy:ident )) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap() as $base;
                let y = args[1].$yy().unwrap() as $base;
                $func(x, y)
            })
        };
        (from $base:ty => $xx:ident $op:tt $yy:ident) => {
            return Some(|_, args| {
                let x = <$base>::from(args[0].$xx().unwrap());
                let y = <$base>::from(args[1].$yy().unwrap());
                Ok((x $op y).into())
            })
        };
        (from $base:ty => $xx:ident . $func:ident ( $yy:ident )) => {
            return Some(|_, args| {
                let x = <$base>::from(args[0].$xx().unwrap());
                let y = <$base>::from(args[1].$yy().unwrap());
                Ok(x.$func(y).into())
            })
        };
        (from $base:ty => $func:ident ( $xx:ident, $yy:ident )) => {
            return Some(|_, args| {
                let x = <$base>::from(args[0].$xx().unwrap());
                let y = <$base>::from(args[1].$yy().unwrap());
                $func(x, y)
            })
        };
    }

    macro_rules! impl_float {
        ($x:ty, $xx:ident, $y:ty, $yy:ident) => {
            #[cfg(not(feature = "no_float"))]
            if types_pair == (TypeId::of::<$x>(), TypeId::of::<$y>()) {
                match op {
                    "+" => impl_op!(FLOAT => $xx + $yy),
                    "-" => impl_op!(FLOAT => $xx - $yy),
                    "*" => impl_op!(FLOAT => $xx * $yy),
                    "/" => impl_op!(FLOAT => $xx / $yy),
                    "%" => impl_op!(FLOAT => $xx % $yy),
                    "**" => impl_op!(FLOAT => $xx.powf($yy as FLOAT)),
                    "==" => impl_op!(FLOAT => $xx == $yy),
                    "!=" => impl_op!(FLOAT => $xx != $yy),
                    ">" => impl_op!(FLOAT => $xx > $yy),
                    ">=" => impl_op!(FLOAT => $xx >= $yy),
                    "<" => impl_op!(FLOAT => $xx < $yy),
                    "<=" => impl_op!(FLOAT => $xx <= $yy),
                    _ => return None,
                }
            }
        };
    }

    impl_float!(FLOAT, as_float, FLOAT, as_float);
    impl_float!(FLOAT, as_float, INT, as_int);
    impl_float!(INT, as_int, FLOAT, as_float);

    macro_rules! impl_decimal {
        ($x:ty, $xx:ident, $y:ty, $yy:ident) => {
            #[cfg(feature = "decimal")]
            if types_pair == (TypeId::of::<$x>(), TypeId::of::<$y>()) {
                if cfg!(not(feature = "unchecked")) {
                    use crate::packages::arithmetic::decimal_functions::*;

                    match op {
                        "+" => impl_op!(from Decimal => add($xx, $yy)),
                        "-" => impl_op!(from Decimal => subtract($xx, $yy)),
                        "*" => impl_op!(from Decimal => multiply($xx, $yy)),
                        "/" => impl_op!(from Decimal => divide($xx, $yy)),
                        "%" => impl_op!(from Decimal => modulo($xx, $yy)),
                        _ => ()
                    }
                } else {
                    match op {
                        "+" => impl_op!(from Decimal => $xx + $yy),
                        "-" => impl_op!(from Decimal => $xx - $yy),
                        "*" => impl_op!(from Decimal => $xx * $yy),
                        "/" => impl_op!(from Decimal => $xx / $yy),
                        "%" => impl_op!(from Decimal => $xx % $yy),
                        _ => ()
                    }
                }

                match op {
                    "==" => impl_op!(from Decimal => $xx == $yy),
                    "!=" => impl_op!(from Decimal => $xx != $yy),
                    ">" => impl_op!(from Decimal => $xx > $yy),
                    ">=" => impl_op!(from Decimal => $xx >= $yy),
                    "<" => impl_op!(from Decimal => $xx < $yy),
                    "<=" => impl_op!(from Decimal => $xx <= $yy),
                    _ => return None
                }
            }
        };
    }

    impl_decimal!(Decimal, as_decimal, Decimal, as_decimal);
    impl_decimal!(Decimal, as_decimal, INT, as_int);
    impl_decimal!(INT, as_int, Decimal, as_decimal);

    // char op string
    if types_pair == (TypeId::of::<char>(), TypeId::of::<ImmutableString>()) {
        #[inline(always)]
        fn get_s1s2(args: &FnCallArgs) -> ([char; 2], [char; 2]) {
            let x = args[0].as_char().unwrap();
            let y = &*args[1].read_lock::<ImmutableString>().unwrap();
            let s1 = [x, '\0'];
            let mut y = y.chars();
            let s2 = [y.next().unwrap_or('\0'), y.next().unwrap_or('\0')];
            (s1, s2)
        }

        match op {
            "+" => {
                return Some(|_, args| {
                    let x = args[0].as_char().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok(format!("{}{}", x, y).into())
                })
            }
            "==" => impl_op!(get_s1s2(==)),
            "!=" => impl_op!(get_s1s2(!=)),
            ">" => impl_op!(get_s1s2(>)),
            ">=" => impl_op!(get_s1s2(>=)),
            "<" => impl_op!(get_s1s2(<)),
            "<=" => impl_op!(get_s1s2(<=)),
            _ => return None,
        }
    }
    // string op char
    if types_pair == (TypeId::of::<ImmutableString>(), TypeId::of::<char>()) {
        #[inline(always)]
        fn get_s1s2(args: &FnCallArgs) -> ([char; 2], [char; 2]) {
            let x = &*args[0].read_lock::<ImmutableString>().unwrap();
            let y = args[1].as_char().unwrap();
            let mut x = x.chars();
            let s1 = [x.next().unwrap_or('\0'), x.next().unwrap_or('\0')];
            let s2 = [y, '\0'];
            (s1, s2)
        }

        match op {
            "+" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = args[1].as_char().unwrap();
                    Ok((x + y).into())
                })
            }
            "-" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = args[1].as_char().unwrap();
                    Ok((x - y).into())
                })
            }
            "==" => impl_op!(get_s1s2(==)),
            "!=" => impl_op!(get_s1s2(!=)),
            ">" => impl_op!(get_s1s2(>)),
            ">=" => impl_op!(get_s1s2(>=)),
            "<" => impl_op!(get_s1s2(<)),
            "<=" => impl_op!(get_s1s2(<=)),
            _ => return None,
        }
    }

    // Default comparison operators for different types
    if type2 != type1 {
        return match op {
            "!=" => Some(|_, _| Ok(Dynamic::TRUE)),
            "==" | ">" | ">=" | "<" | "<=" => Some(|_, _| Ok(Dynamic::FALSE)),
            _ => None,
        };
    }

    // Beyond here, type1 == type2

    if type1 == TypeId::of::<INT>() {
        if cfg!(not(feature = "unchecked")) {
            use crate::packages::arithmetic::arith_basic::INT::functions::*;

            match op {
                "+" => impl_op!(INT => add(as_int, as_int)),
                "-" => impl_op!(INT => subtract(as_int, as_int)),
                "*" => impl_op!(INT => multiply(as_int, as_int)),
                "/" => impl_op!(INT => divide(as_int, as_int)),
                "%" => impl_op!(INT => modulo(as_int, as_int)),
                "**" => impl_op!(INT => power(as_int, as_int)),
                ">>" => impl_op!(INT => shift_right(as_int, as_int)),
                "<<" => impl_op!(INT => shift_left(as_int, as_int)),
                _ => (),
            }
        } else {
            match op {
                "+" => impl_op!(INT => as_int + as_int),
                "-" => impl_op!(INT => as_int - as_int),
                "*" => impl_op!(INT => as_int * as_int),
                "/" => impl_op!(INT => as_int / as_int),
                "%" => impl_op!(INT => as_int % as_int),
                "**" => impl_op!(INT => as_int.pow(as_int as u32)),
                ">>" => impl_op!(INT => as_int >> as_int),
                "<<" => impl_op!(INT => as_int << as_int),
                _ => (),
            }
        }

        match op {
            "==" => impl_op!(INT => as_int == as_int),
            "!=" => impl_op!(INT => as_int != as_int),
            ">" => impl_op!(INT => as_int > as_int),
            ">=" => impl_op!(INT => as_int >= as_int),
            "<" => impl_op!(INT => as_int < as_int),
            "<=" => impl_op!(INT => as_int <= as_int),
            "&" => impl_op!(INT => as_int & as_int),
            "|" => impl_op!(INT => as_int | as_int),
            "^" => impl_op!(INT => as_int ^ as_int),
            _ => return None,
        }
    }

    if type1 == TypeId::of::<bool>() {
        match op {
            "==" => impl_op!(bool => as_bool == as_bool),
            "!=" => impl_op!(bool => as_bool != as_bool),
            ">" => impl_op!(bool => as_bool > as_bool),
            ">=" => impl_op!(bool => as_bool >= as_bool),
            "<" => impl_op!(bool => as_bool < as_bool),
            "<=" => impl_op!(bool => as_bool <= as_bool),
            "&" => impl_op!(bool => as_bool & as_bool),
            "|" => impl_op!(bool => as_bool | as_bool),
            "^" => impl_op!(bool => as_bool ^ as_bool),
            _ => return None,
        }
    }

    if type1 == TypeId::of::<ImmutableString>() {
        match op {
            "+" => impl_op!(ImmutableString + ImmutableString),
            "-" => impl_op!(ImmutableString - ImmutableString),
            "==" => impl_op!(ImmutableString == ImmutableString),
            "!=" => impl_op!(ImmutableString != ImmutableString),
            ">" => impl_op!(ImmutableString > ImmutableString),
            ">=" => impl_op!(ImmutableString >= ImmutableString),
            "<" => impl_op!(ImmutableString < ImmutableString),
            "<=" => impl_op!(ImmutableString <= ImmutableString),
            _ => return None,
        }
    }

    if type1 == TypeId::of::<char>() {
        match op {
            "+" => {
                return Some(|_, args| {
                    let x = args[0].as_char().unwrap();
                    let y = args[1].as_char().unwrap();
                    Ok(format!("{}{}", x, y).into())
                })
            }
            "==" => impl_op!(char => as_char == as_char),
            "!=" => impl_op!(char => as_char != as_char),
            ">" => impl_op!(char => as_char > as_char),
            ">=" => impl_op!(char => as_char >= as_char),
            "<" => impl_op!(char => as_char < as_char),
            "<=" => impl_op!(char => as_char <= as_char),
            _ => return None,
        }
    }

    if type1 == TypeId::of::<()>() {
        match op {
            "==" => return Some(|_, _| Ok(Dynamic::TRUE)),
            "!=" | ">" | ">=" | "<" | "<=" => return Some(|_, _| Ok(Dynamic::FALSE)),
            _ => return None,
        }
    }

    None
}

/// Build in common operator assignment implementations to avoid the cost of calling a registered function.
pub fn get_builtin_op_assignment_fn(
    op: &str,
    x: &Dynamic,
    y: &Dynamic,
) -> Option<fn(NativeCallContext, &mut FnCallArgs) -> RhaiResult> {
    let type1 = x.type_id();
    let type2 = y.type_id();

    let types_pair = (type1, type2);

    macro_rules! impl_op {
        ($x:ty = x $op:tt $yy:ident) => {
            return Some(|_, args| {
                let x = args[0].$yy().unwrap();
                let y = args[1].$yy().unwrap() as $x;
                Ok((*args[0].write_lock::<$x>().unwrap() = x $op y).into())
            })
        };
        ($x:ident $op:tt $yy:ident) => {
            return Some(|_, args| {
                let y = args[1].$yy().unwrap() as $x;
                Ok((*args[0].write_lock::<$x>().unwrap() $op y).into())
            })
        };
        ($x:ident $op:tt $yy:ident as $yyy:ty) => {
            return Some(|_, args| {
                let y = args[1].$yy().unwrap() as $yyy;
                Ok((*args[0].write_lock::<$x>().unwrap() $op y).into())
            })
        };
        ($x:ty => $xx:ident . $func:ident ( $yy:ident as $yyy:ty )) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap();
                let y = args[1].$yy().unwrap() as $x;
                Ok((*args[0].write_lock::<$x>().unwrap() = x.$func(y as $yyy)).into())
            })
        };
        ($x:ty => $func:ident ( $xx:ident, $yy:ident )) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap();
                let y = args[1].$yy().unwrap() as $x;
                Ok((*args[0].write_lock().unwrap() = $func(x, y)?).into())
            })
        };
        (from $x:ident $op:tt $yy:ident) => {
            return Some(|_, args| {
                let y = <$x>::from(args[1].$yy().unwrap());
                Ok((*args[0].write_lock::<$x>().unwrap() $op y).into())
            })
        };
        (from $x:ty => $xx:ident . $func:ident ( $yy:ident )) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap();
                let y = <$x>::from(args[1].$yy().unwrap());
                Ok((*args[0].write_lock::<$x>().unwrap() = x.$func(y)).into())
            })
        };
        (from $x:ty => $func:ident ( $xx:ident, $yy:ident )) => {
            return Some(|_, args| {
                let x = args[0].$xx().unwrap();
                let y = <$x>::from(args[1].$yy().unwrap());
                Ok((*args[0].write_lock().unwrap() = $func(x, y)?).into())
            })
        };
    }

    macro_rules! impl_float {
        ($x:ident, $xx:ident, $y:ty, $yy:ident) => {
            #[cfg(not(feature = "no_float"))]
            if types_pair == (TypeId::of::<$x>(), TypeId::of::<$y>()) {
                match op {
                    "+=" => impl_op!($x += $yy),
                    "-=" => impl_op!($x -= $yy),
                    "*=" => impl_op!($x *= $yy),
                    "/=" => impl_op!($x /= $yy),
                    "%=" => impl_op!($x %= $yy),
                    "**=" => impl_op!($x => $xx.powf($yy as $x)),
                    _ => return None,
                }
            }
        }
    }

    impl_float!(FLOAT, as_float, FLOAT, as_float);
    impl_float!(FLOAT, as_float, INT, as_int);

    macro_rules! impl_decimal {
        ($x:ident, $xx:ident, $y:ty, $yy:ident) => {
            #[cfg(feature = "decimal")]
            if types_pair == (TypeId::of::<$x>(), TypeId::of::<$y>()) {
                if cfg!(not(feature = "unchecked")) {
                    use crate::packages::arithmetic::decimal_functions::*;

                    match op {
                        "+=" => impl_op!(from $x => add($xx, $yy)),
                        "-=" => impl_op!(from $x => subtract($xx, $yy)),
                        "*=" => impl_op!(from $x => multiply($xx, $yy)),
                        "/=" => impl_op!(from $x => divide($xx, $yy)),
                        "%=" => impl_op!(from $x => modulo($xx, $yy)),
                        _ => return None,
                    }
                } else {
                    match op {
                        "+=" => impl_op!(from $x += $yy),
                        "-=" => impl_op!(from $x -= $yy),
                        "*=" => impl_op!(from $x *= $yy),
                        "/=" => impl_op!(from $x /= $yy),
                        "%=" => impl_op!(from $x %= $yy),
                        _ => return None,
                    }
                }
            }
        };
    }

    impl_decimal!(Decimal, as_decimal, Decimal, as_decimal);
    impl_decimal!(Decimal, as_decimal, INT, as_int);

    // string op= char
    if types_pair == (TypeId::of::<ImmutableString>(), TypeId::of::<char>()) {
        match op {
            "+=" => impl_op!(ImmutableString += as_char as char),
            "-=" => impl_op!(ImmutableString -= as_char as char),
            _ => return None,
        }
    }
    // char op= string
    if types_pair == (TypeId::of::<char>(), TypeId::of::<ImmutableString>()) {
        match op {
            "+=" => {
                return Some(|_, args| {
                    let mut ch = args[0].as_char().unwrap().to_string();
                    ch.push_str(args[1].read_lock::<ImmutableString>().unwrap().as_str());

                    let mut x = args[0].write_lock::<Dynamic>().unwrap();
                    Ok((*x = ch.into()).into())
                })
            }
            _ => return None,
        }
    }

    // No built-in op-assignments for different types.
    if type2 != type1 {
        return None;
    }

    // Beyond here, type1 == type2
    if type1 == TypeId::of::<INT>() {
        if cfg!(not(feature = "unchecked")) {
            use crate::packages::arithmetic::arith_basic::INT::functions::*;

            match op {
                "+=" => impl_op!(INT => add(as_int, as_int)),
                "-=" => impl_op!(INT => subtract(as_int, as_int)),
                "*=" => impl_op!(INT => multiply(as_int, as_int)),
                "/=" => impl_op!(INT => divide(as_int, as_int)),
                "%=" => impl_op!(INT => modulo(as_int, as_int)),
                "**=" => impl_op!(INT => power(as_int, as_int)),
                ">>=" => impl_op!(INT => shift_right(as_int, as_int)),
                "<<=" => impl_op!(INT => shift_left(as_int, as_int)),
                _ => (),
            }
        } else {
            match op {
                "+=" => impl_op!(INT += as_int),
                "-=" => impl_op!(INT -= as_int),
                "*=" => impl_op!(INT *= as_int),
                "/=" => impl_op!(INT /= as_int),
                "%=" => impl_op!(INT %= as_int),
                "**=" => impl_op!(INT => as_int.pow(as_int as u32)),
                ">>=" => impl_op!(INT >>= as_int),
                "<<=" => impl_op!(INT <<= as_int),
                _ => (),
            }
        }

        match op {
            "&=" => impl_op!(INT &= as_int),
            "|=" => impl_op!(INT |= as_int),
            "^=" => impl_op!(INT ^= as_int),
            _ => (),
        }
    }

    if type1 == TypeId::of::<bool>() {
        match op {
            "&=" => impl_op!(bool = x && as_bool),
            "|=" => impl_op!(bool = x || as_bool),
            _ => return None,
        }
    }

    if type1 == TypeId::of::<char>() {
        match op {
            "+=" => {
                return Some(|_, args| {
                    let y = args[1].as_char().unwrap();
                    let mut x = args[0].write_lock::<Dynamic>().unwrap();
                    Ok((*x = format!("{}{}", *x, y).into()).into())
                })
            }
            _ => return None,
        }
    }

    if type1 == TypeId::of::<ImmutableString>() {
        match op {
            "+=" => {
                return Some(|_, args| {
                    let (first, second) = args.split_first_mut().unwrap();
                    let mut x = first.write_lock::<ImmutableString>().unwrap();
                    let y = &*second[0].read_lock::<ImmutableString>().unwrap();
                    Ok((*x += y).into())
                })
            }
            "-=" => {
                return Some(|_, args| {
                    let (first, second) = args.split_first_mut().unwrap();
                    let mut x = first.write_lock::<ImmutableString>().unwrap();
                    let y = &*second[0].read_lock::<ImmutableString>().unwrap();
                    Ok((*x -= y).into())
                })
            }
            _ => return None,
        }
    }

    None
}
