use super::{reg_binary, reg_binary_mut, reg_trinary_mut, reg_unary_mut};

use crate::def_package;
use crate::engine::Array;
use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

use crate::stdlib::fmt::Display;

fn prepend<T: Display>(x: T, y: String) -> String {
    format!("{}{}", x, y)
}
fn append<T: Display>(x: String, y: T) -> String {
    format!("{}{}", x, y)
}
fn sub_string(s: &mut String, start: INT, len: INT) -> String {
    let offset = if s.is_empty() || len <= 0 {
        return "".to_string();
    } else if start < 0 {
        0
    } else if (start as usize) >= s.chars().count() {
        return "".to_string();
    } else {
        start as usize
    };

    let chars: Vec<_> = s.chars().collect();

    let len = if offset + (len as usize) > chars.len() {
        chars.len() - offset
    } else {
        len as usize
    };

    chars[offset..][..len].into_iter().collect::<String>()
}
fn crop_string(s: &mut String, start: INT, len: INT) {
    let offset = if s.is_empty() || len <= 0 {
        s.clear();
        return;
    } else if start < 0 {
        0
    } else if (start as usize) >= s.chars().count() {
        s.clear();
        return;
    } else {
        start as usize
    };

    let chars: Vec<_> = s.chars().collect();

    let len = if offset + (len as usize) > chars.len() {
        chars.len() - offset
    } else {
        len as usize
    };

    s.clear();

    chars[offset..][..len]
        .into_iter()
        .for_each(|&ch| s.push(ch));
}

macro_rules! reg_op { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_binary($lib, $op, $func::<$par>, map);)* };
}

def_package!(MoreStringPackage:"Additional string utilities, including string building.", lib, {
    reg_op!(lib, "+", append, INT, bool, char);
    reg_binary_mut(lib, "+", |x: &mut String, _: ()| x.clone(), map);

    reg_op!(lib, "+", prepend, INT, bool, char);
    reg_binary(lib, "+", |_: (), y: String| y, map);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_op!(lib, "+", append, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        reg_op!(lib, "+", prepend, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    {
        reg_op!(lib, "+", append, f32, f64);
        reg_op!(lib, "+", prepend, f32, f64);
    }

    #[cfg(not(feature = "no_index"))]
    {
        reg_binary(lib, "+", |x: String, y: Array| format!("{}{:?}", x, y), map);
        reg_binary(lib, "+", |x: Array, y: String| format!("{:?}{}", x, y), map);
    }

    reg_unary_mut(lib, "len", |s: &mut String| s.chars().count() as INT, map);
    reg_binary_mut(
        lib,
        "contains",
        |s: &mut String, ch: char| s.contains(ch),
        map,
    );
    reg_binary_mut(
        lib,
        "contains",
        |s: &mut String, find: String| s.contains(&find),
        map,
    );
    reg_trinary_mut(
        lib,
        "index_of",
        |s: &mut String, ch: char, start: INT| {
            let start = if start < 0 {
                0
            } else if (start as usize) >= s.chars().count() {
                return -1 as INT;
            } else {
                s.chars().take(start as usize).collect::<String>().len()
            };

            s[start..]
                .find(ch)
                .map(|index| s[0..start + index].chars().count() as INT)
                .unwrap_or(-1 as INT)
        },
        map,
    );
    reg_binary_mut(
        lib,
        "index_of",
        |s: &mut String, ch: char| {
            s.find(ch)
                .map(|index| s[0..index].chars().count() as INT)
                .unwrap_or(-1 as INT)
        },
        map,
    );
    reg_trinary_mut(
        lib,
        "index_of",
        |s: &mut String, find: String, start: INT| {
            let start = if start < 0 {
                0
            } else if (start as usize) >= s.chars().count() {
                return -1 as INT;
            } else {
                s.chars().take(start as usize).collect::<String>().len()
            };

            s[start..]
                .find(&find)
                .map(|index| s[0..start + index].chars().count() as INT)
                .unwrap_or(-1 as INT)
        },
        map,
    );
    reg_binary_mut(
        lib,
        "index_of",
        |s: &mut String, find: String| {
            s.find(&find)
                .map(|index| s[0..index].chars().count() as INT)
                .unwrap_or(-1 as INT)
        },
        map,
    );
    reg_unary_mut(lib, "clear", |s: &mut String| s.clear(), map);
    reg_binary_mut(lib, "append", |s: &mut String, ch: char| s.push(ch), map);
    reg_binary_mut(
        lib,
        "append",
        |s: &mut String, add: String| s.push_str(&add),
        map,
    );
    reg_trinary_mut(lib, "sub_string", sub_string, map);
    reg_binary_mut(
        lib,
        "sub_string",
        |s: &mut String, start: INT| sub_string(s, start, s.len() as INT),
        map,
    );
    reg_trinary_mut(lib, "crop", crop_string, map);
    reg_binary_mut(
        lib,
        "crop",
        |s: &mut String, start: INT| crop_string(s, start, s.len() as INT),
        map,
    );
    reg_binary_mut(
        lib,
        "truncate",
        |s: &mut String, len: INT| {
            if len >= 0 {
                let chars: Vec<_> = s.chars().take(len as usize).collect();
                s.clear();
                chars.into_iter().for_each(|ch| s.push(ch));
            } else {
                s.clear();
            }
        },
        map,
    );
    reg_trinary_mut(
        lib,
        "pad",
        |s: &mut String, len: INT, ch: char| {
            for _ in 0..s.chars().count() - len as usize {
                s.push(ch);
            }
        },
        map,
    );
    reg_trinary_mut(
        lib,
        "replace",
        |s: &mut String, find: String, sub: String| {
            let new_str = s.replace(&find, &sub);
            s.clear();
            s.push_str(&new_str);
        },
        map,
    );
    reg_unary_mut(
        lib,
        "trim",
        |s: &mut String| {
            let trimmed = s.trim();

            if trimmed.len() < s.len() {
                *s = trimmed.to_string();
            }
        },
        map,
    );
});
