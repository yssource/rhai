#![allow(non_snake_case)]

use crate::plugin::*;
use crate::stdlib::{
    any::TypeId, boxed::Box, format, mem, string::String, string::ToString, vec::Vec,
};
use crate::{def_package, Dynamic, FnPtr, ImmutableString, StaticVec, INT};

macro_rules! gen_concat_functions {
    ($root:ident => $($arg_type:ident),+ ) => {
        pub mod $root { $( pub mod $arg_type {
            use super::super::*;

            #[export_module]
            pub mod functions {
                #[rhai_fn(name = "+")]
                pub fn append_func(x: &str, y: $arg_type) -> String {
                    format!("{}{}", x, y)
                }

                #[rhai_fn(name = "+")]
                pub fn prepend_func(x: &mut $arg_type, y: &str) -> String {
                    format!("{}{}", x, y)
                }
            }
        } )* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        combine_with_exported_module!($mod_name, "strings_concat", $root::$arg_type::functions);
    )* }
}

def_package!(crate:MoreStringPackage:"Additional string utilities, including string building.", lib, {
    reg_functions!(lib += basic; INT, bool,  FnPtr);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += numbers; i8, u8, i16, u16, i32, i64, u32, u64);

        #[cfg(not(target_arch = "wasm32"))]
        reg_functions!(lib += num_128; i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    reg_functions!(lib += float; f32, f64);

    combine_with_exported_module!(lib, "string", string_functions);

    // Register string iterator
    lib.set_iter(
        TypeId::of::<ImmutableString>(),
        |s: Dynamic| Box::new(s.cast::<ImmutableString>().chars().collect::<Vec<_>>().into_iter().map(Into::into))
    );
});

gen_concat_functions!(basic => INT, bool, char, FnPtr);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_concat_functions!(numbers => i8, u8, i16, u16, i32, i64, u32, u64);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
gen_concat_functions!(num_128 => i128, u128);

#[cfg(not(feature = "no_float"))]
gen_concat_functions!(float => f32, f64);

#[export_module]
mod string_functions {
    #[rhai_fn(name = "+")]
    pub fn add_append_unit(s: ImmutableString, _x: ()) -> ImmutableString {
        s
    }
    #[rhai_fn(name = "+")]
    pub fn add_prepend_unit(_x: (), s: ImmutableString) -> ImmutableString {
        s
    }

    #[rhai_fn(name = "len", get = "len")]
    pub fn len(s: &str) -> INT {
        s.chars().count() as INT
    }

    pub fn clear(s: &mut ImmutableString) {
        s.make_mut().clear();
    }
    pub fn truncate(s: &mut ImmutableString, len: INT) {
        if len > 0 {
            let chars: StaticVec<_> = s.chars().collect();
            let copy = s.make_mut();
            copy.clear();
            copy.extend(chars.into_iter().take(len as usize));
        } else {
            s.make_mut().clear();
        }
    }
    pub fn trim(s: &mut ImmutableString) {
        let trimmed = s.trim();

        if trimmed.len() < s.len() {
            *s = trimmed.to_string().into();
        }
    }

    #[rhai_fn(name = "contains")]
    pub fn contains_char(s: &str, ch: char) -> bool {
        s.contains(ch)
    }
    pub fn contains(s: &str, find: ImmutableString) -> bool {
        s.contains(find.as_str())
    }

    #[rhai_fn(name = "index_of")]
    pub fn index_of_char_starting_from(s: &str, ch: char, start: INT) -> INT {
        let start = if start < 0 {
            0
        } else if start as usize >= s.chars().count() {
            return -1 as INT;
        } else {
            s.chars().take(start as usize).collect::<String>().len()
        };

        s[start..]
            .find(ch)
            .map(|index| s[0..start + index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[rhai_fn(name = "index_of")]
    pub fn index_of_char(s: &str, ch: char) -> INT {
        s.find(ch)
            .map(|index| s[0..index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[rhai_fn(name = "index_of")]
    pub fn index_of_string_starting_from(s: &str, find: ImmutableString, start: INT) -> INT {
        let start = if start < 0 {
            0
        } else if start as usize >= s.chars().count() {
            return -1 as INT;
        } else {
            s.chars().take(start as usize).collect::<String>().len()
        };

        s[start..]
            .find(find.as_str())
            .map(|index| s[0..start + index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[rhai_fn(name = "index_of")]
    pub fn index_of(s: &str, find: ImmutableString) -> INT {
        s.find(find.as_str())
            .map(|index| s[0..index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }

    pub fn sub_string(s: &str, start: INT, len: INT) -> ImmutableString {
        let offset = if s.is_empty() || len <= 0 {
            return "".to_string().into();
        } else if start < 0 {
            0
        } else if start as usize >= s.chars().count() {
            return "".to_string().into();
        } else {
            start as usize
        };

        let chars: StaticVec<_> = s.chars().collect();

        let len = if offset + len as usize > chars.len() {
            chars.len() - offset
        } else {
            len as usize
        };

        chars
            .iter()
            .skip(offset)
            .take(len)
            .cloned()
            .collect::<String>()
            .into()
    }
    #[rhai_fn(name = "sub_string")]
    pub fn sub_string_starting_from(s: &str, start: INT) -> ImmutableString {
        let len = s.len() as INT;
        sub_string(s, start, len)
    }

    #[rhai_fn(name = "crop")]
    pub fn crop(s: &mut ImmutableString, start: INT, len: INT) {
        let offset = if s.is_empty() || len <= 0 {
            s.make_mut().clear();
            return;
        } else if start < 0 {
            0
        } else if start as usize >= s.chars().count() {
            s.make_mut().clear();
            return;
        } else {
            start as usize
        };

        let chars: StaticVec<_> = s.chars().collect();

        let len = if offset + len as usize > chars.len() {
            chars.len() - offset
        } else {
            len as usize
        };

        let copy = s.make_mut();
        copy.clear();
        copy.extend(chars.iter().skip(offset).take(len));
    }
    #[rhai_fn(name = "crop")]
    pub fn crop_string_starting_from(s: &mut ImmutableString, start: INT) {
        crop(s, start, s.len() as INT);
    }

    #[rhai_fn(name = "replace")]
    pub fn replace(s: &mut ImmutableString, find: ImmutableString, sub: ImmutableString) {
        *s = s.replace(find.as_str(), sub.as_str()).into();
    }
    #[rhai_fn(name = "replace")]
    pub fn replace_string_with_char(s: &mut ImmutableString, find: ImmutableString, sub: char) {
        *s = s.replace(find.as_str(), &sub.to_string()).into();
    }
    #[rhai_fn(name = "replace")]
    pub fn replace_char_with_string(s: &mut ImmutableString, find: char, sub: ImmutableString) {
        *s = s.replace(&find.to_string(), sub.as_str()).into();
    }
    #[rhai_fn(name = "replace")]
    pub fn replace_char(s: &mut ImmutableString, find: char, sub: char) {
        *s = s.replace(&find.to_string(), &sub.to_string()).into();
    }

    #[rhai_fn(return_raw)]
    pub fn pad(
        _ctx: NativeCallContext,
        s: &mut ImmutableString,
        len: INT,
        ch: char,
    ) -> Result<Dynamic, Box<crate::EvalAltResult>> {
        // Check if string will be over max size limit
        #[cfg(not(feature = "unchecked"))]
        if _ctx.engine().max_string_size() > 0 && len as usize > _ctx.engine().max_string_size() {
            return crate::EvalAltResult::ErrorDataTooLarge(
                "Length of string".to_string(),
                crate::Position::NONE,
            )
            .into();
        }

        if len > 0 {
            let orig_len = s.chars().count();

            if len as usize > orig_len {
                let p = s.make_mut();

                for _ in 0..(len as usize - orig_len) {
                    p.push(ch);
                }

                #[cfg(not(feature = "unchecked"))]
                if _ctx.engine().max_string_size() > 0 && s.len() > _ctx.engine().max_string_size()
                {
                    return crate::EvalAltResult::ErrorDataTooLarge(
                        "Length of string".to_string(),
                        crate::Position::NONE,
                    )
                    .into();
                }
            }
        }

        Ok(Dynamic::UNIT)
    }
    #[rhai_fn(name = "pad", return_raw)]
    pub fn pad_with_string(
        _ctx: NativeCallContext,
        s: &mut ImmutableString,
        len: INT,
        padding: &str,
    ) -> Result<Dynamic, Box<crate::EvalAltResult>> {
        // Check if string will be over max size limit
        #[cfg(not(feature = "unchecked"))]
        if _ctx.engine().max_string_size() > 0 && len as usize > _ctx.engine().max_string_size() {
            return crate::EvalAltResult::ErrorDataTooLarge(
                "Length of string".to_string(),
                crate::Position::NONE,
            )
            .into();
        }

        if len > 0 {
            let mut str_len = s.chars().count();
            let padding_len = padding.chars().count();

            if len as usize > str_len {
                let p = s.make_mut();

                while str_len < len as usize {
                    if str_len + padding_len <= len as usize {
                        p.push_str(padding);
                        str_len += padding_len;
                    } else {
                        p.extend(padding.chars().take(len as usize - str_len));
                        str_len = len as usize;
                    }
                }

                #[cfg(not(feature = "unchecked"))]
                if _ctx.engine().max_string_size() > 0 && s.len() > _ctx.engine().max_string_size()
                {
                    return crate::EvalAltResult::ErrorDataTooLarge(
                        "Length of string".to_string(),
                        crate::Position::NONE,
                    )
                    .into();
                }
            }
        }

        Ok(Dynamic::UNIT)
    }

    #[cfg(not(feature = "no_index"))]
    pub mod arrays {
        use crate::Array;

        #[rhai_fn(name = "+")]
        pub fn append(x: &str, y: Array) -> String {
            format!("{}{:?}", x, y)
        }
        #[rhai_fn(name = "+")]
        pub fn prepend(x: &mut Array, y: &str) -> String {
            format!("{:?}{}", x, y)
        }
        pub fn split(s: &str, delimiter: ImmutableString) -> Array {
            s.split(delimiter.as_str())
                .map(Into::<Dynamic>::into)
                .collect()
        }
        #[rhai_fn(name = "split")]
        pub fn split_char(s: &str, delimiter: char) -> Array {
            s.split(delimiter).map(Into::<Dynamic>::into).collect()
        }
    }

    #[cfg(not(feature = "no_object"))]
    pub mod maps {
        use crate::Map;

        #[rhai_fn(name = "+")]
        pub fn append(x: &str, y: Map) -> String {
            format!("{}#{:?}", x, y)
        }
        #[rhai_fn(name = "+")]
        pub fn prepend(x: &mut Map, y: &str) -> String {
            format!("#{:?}{}", x, y)
        }
    }
}
