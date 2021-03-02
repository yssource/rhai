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

/// Build in common binary operator implementations to avoid the cost of calling a registered function.
pub fn get_builtin_binary_op_fn(
    op: &str,
    x: &Dynamic,
    y: &Dynamic,
) -> Option<fn(NativeCallContext, &mut FnCallArgs) -> RhaiResult> {
    let type1 = x.type_id();
    let type2 = y.type_id();

    if x.is_variant() || y.is_variant() {
        // One of the operands is a custom type, so it is never built-in
        return match op {
            "!=" if type1 != type2 => Some(|_, _| Ok(Dynamic::TRUE)),
            "==" | ">" | ">=" | "<" | "<=" if type1 != type2 => Some(|_, _| Ok(Dynamic::FALSE)),
            _ => None,
        };
    }

    let types_pair = (type1, type2);

    #[cfg(not(feature = "no_float"))]
    if types_pair == (TypeId::of::<FLOAT>(), TypeId::of::<FLOAT>())
        || types_pair == (TypeId::of::<FLOAT>(), TypeId::of::<INT>())
        || types_pair == (TypeId::of::<INT>(), TypeId::of::<FLOAT>())
    {
        #[inline(always)]
        fn get_xy(args: &FnCallArgs) -> (FLOAT, FLOAT) {
            let type1 = args[0].type_id();
            let type2 = args[1].type_id();

            let types_pair = (type1, type2);

            if types_pair == (TypeId::of::<FLOAT>(), TypeId::of::<FLOAT>()) {
                // FLOAT op FLOAT
                (
                    args[0].clone().cast::<FLOAT>(),
                    args[1].clone().cast::<FLOAT>(),
                )
            } else if types_pair == (TypeId::of::<FLOAT>(), TypeId::of::<INT>()) {
                // FLOAT op INT
                (
                    args[0].clone().cast::<FLOAT>(),
                    args[1].clone().cast::<INT>() as FLOAT,
                )
            } else if types_pair == (TypeId::of::<INT>(), TypeId::of::<FLOAT>()) {
                // INT op FLOAT
                (
                    args[0].clone().cast::<INT>() as FLOAT,
                    args[1].clone().cast::<FLOAT>(),
                )
            } else {
                unreachable!()
            }
        }

        match op {
            "+" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x + y).into())
                })
            }
            "-" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x - y).into())
                })
            }
            "*" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x * y).into())
                })
            }
            "/" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x / y).into())
                })
            }
            "%" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x % y).into())
                })
            }
            "**" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok(x.powf(y).into())
                })
            }
            "==" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x == y).into())
                })
            }
            "!=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x != y).into())
                })
            }
            ">" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x > y).into())
                })
            }
            ">=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x >= y).into())
                })
            }
            "<" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x < y).into())
                })
            }
            "<=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x <= y).into())
                })
            }
            _ => return None,
        }
    }

    #[cfg(feature = "decimal")]
    if types_pair == (TypeId::of::<Decimal>(), TypeId::of::<Decimal>())
        || types_pair == (TypeId::of::<Decimal>(), TypeId::of::<INT>())
        || types_pair == (TypeId::of::<INT>(), TypeId::of::<Decimal>())
    {
        #[inline(always)]
        fn get_xy(args: &FnCallArgs) -> (Decimal, Decimal) {
            let type1 = args[0].type_id();
            let type2 = args[1].type_id();

            let types_pair = (type1, type2);

            if types_pair == (TypeId::of::<Decimal>(), TypeId::of::<Decimal>()) {
                // Decimal op Decimal
                (
                    args[0].clone().cast::<Decimal>(),
                    args[1].clone().cast::<Decimal>(),
                )
            } else if types_pair == (TypeId::of::<Decimal>(), TypeId::of::<INT>()) {
                // Decimal op INT
                (
                    args[0].clone().cast::<Decimal>(),
                    Decimal::from(args[1].clone().cast::<INT>()),
                )
            } else if types_pair == (TypeId::of::<INT>(), TypeId::of::<Decimal>()) {
                // INT op Decimal
                (
                    Decimal::from(args[0].clone().cast::<INT>()),
                    args[1].clone().cast::<Decimal>(),
                )
            } else {
                unreachable!()
            }
        }

        if cfg!(not(feature = "unchecked")) {
            use crate::packages::arithmetic::decimal_functions::*;

            match op {
                "+" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        add(x, y)
                    })
                }
                "-" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        subtract(x, y)
                    })
                }
                "*" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        multiply(x, y)
                    })
                }
                "/" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        divide(x, y)
                    })
                }
                "%" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        modulo(x, y)
                    })
                }
                _ => (),
            }
        } else {
            match op {
                "+" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x + y).into())
                    })
                }
                "-" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x - y).into())
                    })
                }
                "*" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x * y).into())
                    })
                }
                "/" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x / y).into())
                    })
                }
                "%" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x % y).into())
                    })
                }
                _ => (),
            }
        }

        match op {
            "==" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x == y).into())
                })
            }
            "!=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x != y).into())
                })
            }
            ">" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x > y).into())
                })
            }
            ">=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x >= y).into())
                })
            }
            "<" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x < y).into())
                })
            }
            "<=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x <= y).into())
                })
            }
            _ => return None,
        }
    }

    // char op string
    if types_pair == (TypeId::of::<char>(), TypeId::of::<ImmutableString>()) {
        match op {
            "+" => {
                return Some(|_, args| {
                    let x = args[0].clone().cast::<char>();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok(format!("{}{}", x, y).into())
                })
            }
            "==" | "!=" | ">" | ">=" | "<" | "<=" => {
                #[inline(always)]
                fn get_s1s2(args: &FnCallArgs) -> ([char; 2], [char; 2]) {
                    let x = args[0].clone().cast::<char>();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    let s1 = [x, '\0'];
                    let mut y = y.chars();
                    let s2 = [y.next().unwrap_or('\0'), y.next().unwrap_or('\0')];
                    (s1, s2)
                }

                match op {
                    "==" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 == s2).into())
                        })
                    }
                    "!=" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 != s2).into())
                        })
                    }
                    ">" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 > s2).into())
                        })
                    }
                    ">=" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 >= s2).into())
                        })
                    }
                    "<" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 < s2).into())
                        })
                    }
                    "<=" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 <= s2).into())
                        })
                    }
                    _ => unreachable!(),
                }
            }
            _ => return None,
        }
    }
    // string op char
    if types_pair == (TypeId::of::<ImmutableString>(), TypeId::of::<char>()) {
        match op {
            "+" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = args[1].clone().cast::<char>();
                    Ok((x + y).into())
                })
            }
            "-" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = args[1].clone().cast::<char>();
                    Ok((x - y).into())
                })
            }
            "==" | "!=" | ">" | ">=" | "<" | "<=" => {
                #[inline(always)]
                fn get_s1s2(args: &FnCallArgs) -> ([char; 2], [char; 2]) {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = args[1].clone().cast::<char>();
                    let mut x = x.chars();
                    let s1 = [x.next().unwrap_or('\0'), x.next().unwrap_or('\0')];
                    let s2 = [y, '\0'];
                    (s1, s2)
                }

                match op {
                    "==" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 == s2).into())
                        })
                    }
                    "!=" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 != s2).into())
                        })
                    }
                    ">" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 > s2).into())
                        })
                    }
                    ">=" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 >= s2).into())
                        })
                    }
                    "<" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 < s2).into())
                        })
                    }
                    "<=" => {
                        return Some(|_, args| {
                            let (s1, s2) = get_s1s2(args);
                            Ok((s1 <= s2).into())
                        })
                    }
                    _ => unreachable!(),
                }
            }
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
        #[inline(always)]
        fn get_xy(args: &FnCallArgs) -> (INT, INT) {
            let x = args[0].clone().cast::<INT>();
            let y = args[1].clone().cast::<INT>();
            (x, y)
        }

        if cfg!(not(feature = "unchecked")) {
            use crate::packages::arithmetic::arith_basic::INT::functions::*;

            match op {
                "+" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        add(x, y)
                    })
                }
                "-" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        subtract(x, y)
                    })
                }
                "*" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        multiply(x, y)
                    })
                }
                "/" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        divide(x, y)
                    })
                }
                "%" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        modulo(x, y)
                    })
                }
                "**" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        power(x, y)
                    })
                }
                ">>" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        shift_right(x, y)
                    })
                }
                "<<" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        shift_left(x, y)
                    })
                }
                _ => (),
            }
        } else {
            match op {
                "+" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x + y).into())
                    })
                }
                "-" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x - y).into())
                    })
                }
                "*" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x * y).into())
                    })
                }
                "/" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x / y).into())
                    })
                }
                "%" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x % y).into())
                    })
                }
                "**" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok(x.pow(y as u32).into())
                    })
                }
                ">>" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x >> y).into())
                    })
                }
                "<<" => {
                    return Some(|_, args| {
                        let (x, y) = get_xy(args);
                        Ok((x << y).into())
                    })
                }
                _ => (),
            }
        }

        match op {
            "==" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x == y).into())
                })
            }
            "!=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x != y).into())
                })
            }
            ">" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x > y).into())
                })
            }
            ">=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x >= y).into())
                })
            }
            "<" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x < y).into())
                })
            }
            "<=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x <= y).into())
                })
            }
            "&" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x & y).into())
                })
            }
            "|" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x | y).into())
                })
            }
            "^" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x ^ y).into())
                })
            }
            _ => return None,
        }
    }

    if type1 == TypeId::of::<bool>() {
        #[inline(always)]
        fn get_xy(args: &FnCallArgs) -> (bool, bool) {
            let x = args[0].clone().cast::<bool>();
            let y = args[1].clone().cast::<bool>();
            (x, y)
        }

        match op {
            "&" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x && y).into())
                })
            }
            "|" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x || y).into())
                })
            }
            "^" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x ^ y).into())
                })
            }
            "==" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x == y).into())
                })
            }
            "!=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x != y).into())
                })
            }
            _ => return None,
        }
    }

    if type1 == TypeId::of::<ImmutableString>() {
        match op {
            "+" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x + y).into())
                })
            }
            "-" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x - y).into())
                })
            }
            "==" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x == y).into())
                })
            }
            "!=" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x != y).into())
                })
            }
            ">" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x > y).into())
                })
            }
            ">=" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x >= y).into())
                })
            }
            "<" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x < y).into())
                })
            }
            "<=" => {
                return Some(|_, args| {
                    let x = &*args[0].read_lock::<ImmutableString>().unwrap();
                    let y = &*args[1].read_lock::<ImmutableString>().unwrap();
                    Ok((x <= y).into())
                })
            }
            _ => return None,
        }
    }

    if type1 == TypeId::of::<char>() {
        #[inline(always)]
        fn get_xy(args: &FnCallArgs) -> (char, char) {
            let x = args[0].clone().cast::<char>();
            let y = args[1].clone().cast::<char>();
            (x, y)
        }

        match op {
            "+" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok(format!("{}{}", x, y).into())
                })
            }
            "==" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x == y).into())
                })
            }
            "!=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x != y).into())
                })
            }
            ">" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x > y).into())
                })
            }
            ">=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x >= y).into())
                })
            }
            "<" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x < y).into())
                })
            }
            "<=" => {
                return Some(|_, args| {
                    let (x, y) = get_xy(args);
                    Ok((x <= y).into())
                })
            }
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

    #[cfg(not(feature = "no_float"))]
    if types_pair == (TypeId::of::<FLOAT>(), TypeId::of::<FLOAT>())
        || types_pair == (TypeId::of::<FLOAT>(), TypeId::of::<INT>())
    {
        fn get_y(args: &FnCallArgs) -> FLOAT {
            let type2 = args[1].type_id();

            if type2 == TypeId::of::<FLOAT>() {
                // FLOAT op= FLOAT
                args[1].clone().cast::<FLOAT>()
            } else if type2 == TypeId::of::<INT>() {
                // FLOAT op= INT
                args[1].clone().cast::<INT>() as FLOAT
            } else {
                unreachable!();
            }
        }

        match op {
            "+=" => {
                return Some(|_, args| {
                    let y = get_y(args);
                    Ok((*args[0].write_lock::<FLOAT>().unwrap() += y).into())
                })
            }
            "-=" => {
                return Some(|_, args| {
                    let y = get_y(args);
                    Ok((*args[0].write_lock::<FLOAT>().unwrap() -= y).into())
                })
            }
            "*=" => {
                return Some(|_, args| {
                    let y = get_y(args);
                    Ok((*args[0].write_lock::<FLOAT>().unwrap() *= y).into())
                })
            }
            "/=" => {
                return Some(|_, args| {
                    let y = get_y(args);
                    Ok((*args[0].write_lock::<FLOAT>().unwrap() /= y).into())
                })
            }
            "%=" => {
                return Some(|_, args| {
                    let y = get_y(args);
                    Ok((*args[0].write_lock::<FLOAT>().unwrap() %= y).into())
                })
            }
            "**=" => {
                return Some(|_, args| {
                    let y = get_y(args);
                    let mut x = args[0].write_lock::<FLOAT>().unwrap();
                    Ok((*x = x.powf(y)).into())
                })
            }
            _ => return None,
        }
    }

    #[cfg(feature = "decimal")]
    if types_pair == (TypeId::of::<Decimal>(), TypeId::of::<Decimal>())
        || types_pair == (TypeId::of::<Decimal>(), TypeId::of::<INT>())
    {
        fn get_y(args: &FnCallArgs) -> Decimal {
            let type2 = args[1].type_id();

            if type2 == TypeId::of::<Decimal>() {
                // Decimal op= Decimal
                args[1].clone().cast::<Decimal>()
            } else if type2 == TypeId::of::<INT>() {
                // Decimal op= INT
                Decimal::from(args[1].clone().cast::<INT>())
            } else {
                unreachable!();
            }
        }

        if cfg!(not(feature = "unchecked")) {
            use crate::packages::arithmetic::decimal_functions::*;

            match op {
                "+=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<Decimal>();
                        let mut x = args[0].write_lock::<Decimal>().unwrap();
                        Ok((*x = add(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "-=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<Decimal>();
                        let mut x = args[0].write_lock::<Decimal>().unwrap();
                        Ok((*x = subtract(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "*=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<Decimal>();
                        let mut x = args[0].write_lock::<Decimal>().unwrap();
                        Ok((*x = multiply(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "/=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<Decimal>();
                        let mut x = args[0].write_lock::<Decimal>().unwrap();
                        Ok((*x = divide(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "%=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<Decimal>();
                        let mut x = args[0].write_lock::<Decimal>().unwrap();
                        Ok((*x = modulo(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                _ => (),
            }
        } else {
            match op {
                "+=" => {
                    return Some(|_, args| {
                        let y = get_y(args);
                        Ok((*args[0].write_lock::<Decimal>().unwrap() += y).into())
                    })
                }
                "-=" => {
                    return Some(|_, args| {
                        let y = get_y(args);
                        Ok((*args[0].write_lock::<Decimal>().unwrap() -= y).into())
                    })
                }
                "*=" => {
                    return Some(|_, args| {
                        let y = get_y(args);
                        Ok((*args[0].write_lock::<Decimal>().unwrap() *= y).into())
                    })
                }
                "/=" => {
                    return Some(|_, args| {
                        let y = get_y(args);
                        Ok((*args[0].write_lock::<Decimal>().unwrap() /= y).into())
                    })
                }
                "%=" => {
                    return Some(|_, args| {
                        let y = get_y(args);
                        Ok((*args[0].write_lock::<Decimal>().unwrap() %= y).into())
                    })
                }
                _ => return None,
            }
        }
    }

    // string op= char
    if types_pair == (TypeId::of::<ImmutableString>(), TypeId::of::<char>()) {
        match op {
            "+=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<char>();
                    Ok((*args[0].write_lock::<ImmutableString>().unwrap() += y).into())
                })
            }
            "-=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<char>();
                    Ok((*args[0].write_lock::<ImmutableString>().unwrap() -= y).into())
                })
            }
            _ => return None,
        }
    }
    // char op= string
    if types_pair == (TypeId::of::<char>(), TypeId::of::<ImmutableString>()) {
        match op {
            "+=" => {
                return Some(|_, args| {
                    let mut ch = args[0].read_lock::<char>().unwrap().to_string();
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
                "+=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = add(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "-=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = subtract(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "*=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = multiply(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "/=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = divide(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "%=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = modulo(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "**=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = power(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                ">>=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = shift_right(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                "<<=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = shift_left(*x, y)?.as_int().unwrap().into()).into())
                    })
                }
                _ => (),
            }
        } else {
            match op {
                "+=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() += y).into())
                    })
                }
                "-=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() -= y).into())
                    })
                }
                "*=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() *= y).into())
                    })
                }
                "/=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() /= y).into())
                    })
                }
                "%=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() %= y).into())
                    })
                }
                "**=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        let mut x = args[0].write_lock::<INT>().unwrap();
                        Ok((*x = x.pow(y as u32)).into())
                    })
                }
                ">>=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() >>= y).into())
                    })
                }
                "<<=" => {
                    return Some(|_, args| {
                        let y = args[1].clone().cast::<INT>();
                        Ok((*args[0].write_lock::<INT>().unwrap() <<= y).into())
                    })
                }
                _ => (),
            }
        }

        match op {
            "&=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<INT>();
                    Ok((*args[0].write_lock::<INT>().unwrap() &= y).into())
                })
            }
            "|=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<INT>();
                    Ok((*args[0].write_lock::<INT>().unwrap() |= y).into())
                })
            }
            "^=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<INT>();
                    Ok((*args[0].write_lock::<INT>().unwrap() ^= y).into())
                })
            }
            _ => return None,
        }
    }

    if type1 == TypeId::of::<bool>() {
        match op {
            "&=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<bool>();
                    let mut x = args[0].write_lock::<bool>().unwrap();
                    Ok((*x = *x && y).into())
                })
            }
            "|=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<bool>();
                    let mut x = args[0].write_lock::<bool>().unwrap();
                    Ok((*x = *x || y).into())
                })
            }
            _ => return None,
        }
    }

    if type1 == TypeId::of::<char>() {
        match op {
            "+=" => {
                return Some(|_, args| {
                    let y = args[1].clone().cast::<char>();
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
