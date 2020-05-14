use crate::def_package;
use crate::module::FuncReturn;
use crate::parser::INT;

#[cfg(not(feature = "no_index"))]
use crate::engine::Array;

use crate::stdlib::{
    fmt::Display,
    format,
    string::{String, ToString},
    vec::Vec,
};

fn prepend<T: Display>(x: T, y: String) -> FuncReturn<String> {
    Ok(format!("{}{}", x, y))
}
fn append<T: Display>(x: String, y: T) -> FuncReturn<String> {
    Ok(format!("{}{}", x, y))
}
fn sub_string(s: &mut String, start: INT, len: INT) -> FuncReturn<String> {
    let offset = if s.is_empty() || len <= 0 {
        return Ok("".to_string());
    } else if start < 0 {
        0
    } else if (start as usize) >= s.chars().count() {
        return Ok("".to_string());
    } else {
        start as usize
    };

    let chars: Vec<_> = s.chars().collect();

    let len = if offset + (len as usize) > chars.len() {
        chars.len() - offset
    } else {
        len as usize
    };

    Ok(chars[offset..][..len].into_iter().collect())
}
fn crop_string(s: &mut String, start: INT, len: INT) -> FuncReturn<()> {
    let offset = if s.is_empty() || len <= 0 {
        s.clear();
        return Ok(());
    } else if start < 0 {
        0
    } else if (start as usize) >= s.chars().count() {
        s.clear();
        return Ok(());
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

    Ok(())
}

macro_rules! reg_op {
    ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
        $( $lib.set_fn_2($op, $func::<$par>); )*
    };
}

def_package!(crate:MoreStringPackage:"Additional string utilities, including string building.", lib, {
    reg_op!(lib, "+", append, INT, bool, char);
    lib.set_fn_2_mut( "+", |x: &mut String, _: ()| Ok(x.clone()));

    reg_op!(lib, "+", prepend, INT, bool, char);
    lib.set_fn_2("+", |_: (), y: String| Ok(y));

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
        lib.set_fn_2("+", |x: String, y: Array| Ok(format!("{}{:?}", x, y)));
        lib.set_fn_2("+", |x: Array, y: String| Ok(format!("{:?}{}", x, y)));
    }

    lib.set_fn_1_mut("len", |s: &mut String| Ok(s.chars().count() as INT));
    lib.set_fn_2_mut(
        "contains",
        |s: &mut String, ch: char| Ok(s.contains(ch)),
    );
    lib.set_fn_2_mut(
        "contains",
        |s: &mut String, find: String| Ok(s.contains(&find)),
    );
    lib.set_fn_3_mut(
        "index_of",
        |s: &mut String, ch: char, start: INT| {
            let start = if start < 0 {
                0
            } else if (start as usize) >= s.chars().count() {
                return Ok(-1 as INT);
            } else {
                s.chars().take(start as usize).collect::<String>().len()
            };

            Ok(s[start..]
                .find(ch)
                .map(|index| s[0..start + index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_2_mut(
        "index_of",
        |s: &mut String, ch: char| {
            Ok(s.find(ch)
                .map(|index| s[0..index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_3_mut(
        "index_of",
        |s: &mut String, find: String, start: INT| {
            let start = if start < 0 {
                0
            } else if (start as usize) >= s.chars().count() {
                return Ok(-1 as INT);
            } else {
                s.chars().take(start as usize).collect::<String>().len()
            };

            Ok(s[start..]
                .find(&find)
                .map(|index| s[0..start + index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_2_mut(
        "index_of",
        |s: &mut String, find: String| {
            Ok(s.find(&find)
                .map(|index| s[0..index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_1_mut("clear", |s: &mut String| {
        s.clear();
        Ok(())
    });
    lib.set_fn_2_mut( "append", |s: &mut String, ch: char| {
        s.push(ch);
        Ok(())
    });
    lib.set_fn_2_mut(
        "append",
        |s: &mut String, add: String| {
            s.push_str(&add);
            Ok(())
        }
    );
    lib.set_fn_3_mut( "sub_string", sub_string);
    lib.set_fn_2_mut(
        "sub_string",
        |s: &mut String, start: INT| sub_string(s, start, s.len() as INT),
    );
    lib.set_fn_3_mut( "crop", crop_string);
    lib.set_fn_2_mut(
        "crop",
        |s: &mut String, start: INT| crop_string(s, start, s.len() as INT),
    );
    lib.set_fn_2_mut(
        "truncate",
        |s: &mut String, len: INT| {
            if len >= 0 {
                let chars: Vec<_> = s.chars().take(len as usize).collect();
                s.clear();
                chars.into_iter().for_each(|ch| s.push(ch));
            } else {
                s.clear();
            }
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "pad",
        |s: &mut String, len: INT, ch: char| {
            for _ in 0..s.chars().count() - len as usize {
                s.push(ch);
            }
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut String, find: String, sub: String| {
            let new_str = s.replace(&find, &sub);
            s.clear();
            s.push_str(&new_str);
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut String, find: String, sub: char| {
            let new_str = s.replace(&find, &sub.to_string());
            s.clear();
            s.push_str(&new_str);
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut String, find: char, sub: String| {
            let new_str = s.replace(&find.to_string(), &sub);
            s.clear();
            s.push_str(&new_str);
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut String, find: char, sub: char| {
            let new_str = s.replace(&find.to_string(), &sub.to_string());
            s.clear();
            s.push_str(&new_str);
            Ok(())
        },
    );
    lib.set_fn_1_mut(
        "trim",
        |s: &mut String| {
            let trimmed = s.trim();

            if trimmed.len() < s.len() {
                *s = trimmed.to_string();
            }
            Ok(())
        },
    );
});
