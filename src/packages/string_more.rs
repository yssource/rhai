use crate::def_package;
use crate::module::FuncReturn;
use crate::parser::{ImmutableString, INT};
use crate::utils::StaticVec;

#[cfg(not(feature = "no_index"))]
use crate::engine::Array;

use crate::stdlib::{
    fmt::Display,
    format,
    string::{String, ToString},
};

fn prepend<T: Display>(x: T, y: ImmutableString) -> FuncReturn<ImmutableString> {
    Ok(format!("{}{}", x, y).into())
}
fn append<T: Display>(x: ImmutableString, y: T) -> FuncReturn<ImmutableString> {
    Ok(format!("{}{}", x, y).into())
}
fn sub_string(s: ImmutableString, start: INT, len: INT) -> FuncReturn<ImmutableString> {
    let offset = if s.is_empty() || len <= 0 {
        return Ok("".to_string().into());
    } else if start < 0 {
        0
    } else if (start as usize) >= s.chars().count() {
        return Ok("".to_string().into());
    } else {
        start as usize
    };

    let chars: StaticVec<_> = s.chars().collect();

    let len = if offset + (len as usize) > chars.len() {
        chars.len() - offset
    } else {
        len as usize
    };

    Ok(chars
        .iter()
        .skip(offset)
        .take(len)
        .cloned()
        .collect::<String>()
        .into())
}
fn crop_string(s: &mut ImmutableString, start: INT, len: INT) -> FuncReturn<()> {
    let offset = if s.is_empty() || len <= 0 {
        s.make_mut().clear();
        return Ok(());
    } else if start < 0 {
        0
    } else if (start as usize) >= s.chars().count() {
        s.make_mut().clear();
        return Ok(());
    } else {
        start as usize
    };

    let chars: StaticVec<_> = s.chars().collect();

    let len = if offset + (len as usize) > chars.len() {
        chars.len() - offset
    } else {
        len as usize
    };

    let copy = s.make_mut();
    copy.clear();
    copy.extend(chars.iter().skip(offset).take(len));

    Ok(())
}

macro_rules! reg_op {
    ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
        $( $lib.set_fn_2($op, $func::<$par>); )*
    };
}

def_package!(crate:MoreStringPackage:"Additional string utilities, including string building.", lib, {
    reg_op!(lib, "+", append, INT, bool, char);
    lib.set_fn_2( "+", |x: ImmutableString, _: ()| Ok(x));

    reg_op!(lib, "+", prepend, INT, bool, char);
    lib.set_fn_2("+", |_: (), y: ImmutableString| Ok(y));

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
        lib.set_fn_2("+", |x: ImmutableString, y: Array| Ok(format!("{}{:?}", x, y)));
        lib.set_fn_2_mut("+", |x: &mut Array, y: ImmutableString| Ok(format!("{:?}{}", x, y)));
    }

    lib.set_fn_1("len", |s: ImmutableString| Ok(s.chars().count() as INT));
    lib.set_fn_2(
        "contains",
        |s: ImmutableString, ch: char| Ok(s.contains(ch)),
    );
    lib.set_fn_2(
        "contains",
        |s: ImmutableString, find: ImmutableString| Ok(s.contains(find.as_str())),
    );
    lib.set_fn_3(
        "index_of",
        |s: ImmutableString, ch: char, start: INT| {
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
    lib.set_fn_2(
        "index_of",
        |s: ImmutableString, ch: char| {
            Ok(s.find(ch)
                .map(|index| s[0..index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_3(
        "index_of",
        |s: ImmutableString, find: ImmutableString, start: INT| {
            let start = if start < 0 {
                0
            } else if (start as usize) >= s.chars().count() {
                return Ok(-1 as INT);
            } else {
                s.chars().take(start as usize).collect::<String>().len()
            };

            Ok(s[start..]
                .find(find.as_str())
                .map(|index| s[0..start + index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_2(
        "index_of",
        |s: ImmutableString, find: ImmutableString| {
            Ok(s.find(find.as_str())
                .map(|index| s[0..index].chars().count() as INT)
                .unwrap_or(-1 as INT))
        },
    );
    lib.set_fn_1_mut("clear", |s: &mut ImmutableString| {
        s.make_mut().clear();
        Ok(())
    });
    lib.set_fn_2_mut("append", |s: &mut ImmutableString, ch: char| {
        s.make_mut().push(ch);
        Ok(())
    });
    lib.set_fn_2_mut(
        "append",
        |s: &mut ImmutableString, add: ImmutableString| {
            s.make_mut().push_str(add.as_str());
            Ok(())
        }
    );
    lib.set_fn_3("sub_string", sub_string);
    lib.set_fn_2(
        "sub_string",
        |s: ImmutableString, start: INT| {
            let len = s.len() as INT;
            sub_string(s, start, len)
        },
    );
    lib.set_fn_3_mut("crop", crop_string);
    lib.set_fn_2_mut(
        "crop",
        |s: &mut ImmutableString, start: INT| crop_string(s, start, s.len() as INT),
    );
    lib.set_fn_2_mut(
        "truncate",
        |s: &mut ImmutableString, len: INT| {
            if len > 0 {
                let chars: StaticVec<_> = s.chars().collect();
                let copy = s.make_mut();
                copy.clear();
                copy.extend(chars.into_iter().take(len as usize));
            } else {
                s.make_mut().clear();
            }
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "pad",
        |s: &mut ImmutableString, len: INT, ch: char| {
            let copy = s.make_mut();
            for _ in 0..copy.chars().count() - len as usize {
                copy.push(ch);
            }
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut ImmutableString, find: ImmutableString, sub: ImmutableString| {
            *s = s.replace(find.as_str(), sub.as_str()).into();
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut ImmutableString, find: ImmutableString, sub: char| {
            *s = s.replace(find.as_str(), &sub.to_string()).into();
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut ImmutableString, find: char, sub: ImmutableString| {
            *s = s.replace(&find.to_string(), sub.as_str()).into();
            Ok(())
        },
    );
    lib.set_fn_3_mut(
        "replace",
        |s: &mut ImmutableString, find: char, sub: char| {
            *s = s.replace(&find.to_string(), &sub.to_string()).into();
            Ok(())
        },
    );
    lib.set_fn_1_mut(
        "trim",
        |s: &mut ImmutableString| {
            let trimmed = s.trim();

            if trimmed.len() < s.len() {
                *s = trimmed.to_string().into();
            }
            Ok(())
        },
    );
});
