#![allow(non_snake_case)]

use crate::any::Dynamic;
use crate::def_package;
use crate::engine::{make_getter, Engine};
use crate::fn_native::FnPtr;
use crate::parser::{ImmutableString, INT};
use crate::plugin::*;
use crate::utils::StaticVec;

#[cfg(not(feature = "unchecked"))]
use crate::{result::EvalAltResult, token::Position};

use crate::stdlib::{
    any::TypeId, boxed::Box, fmt::Display, format, mem, string::ToString, vec::Vec,
};

macro_rules! gen_concat_functions {
    ($root:ident => $($arg_type:ident),+ ) => {
        pub mod $root { $(
            pub mod $arg_type {
                use super::super::*;

                #[export_fn]
                pub fn append_func(x: ImmutableString, y: $arg_type) -> String {
                    super::super::append(x, y)
                }

                #[export_fn]
                pub fn prepend_func(x: $arg_type, y: ImmutableString) -> String {
                    super::super::prepend(x, y)
                }
            }
        )* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => {
        $(set_exported_fn!($mod_name, "+", $root::$arg_type::append_func);)*
        $(set_exported_fn!($mod_name, "+", $root::$arg_type::prepend_func);)*
    }
}

def_package!(crate:MoreStringPackage:"Additional string utilities, including string building.", lib, {
    reg_functions!(lib += basic; INT, bool, char, FnPtr);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += numbers; i8, u8, i16, u16, i32, i64, u32, u64);

        #[cfg(not(target_arch = "wasm32"))]
        reg_functions!(lib += num_128; i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    reg_functions!(lib += float; f32, f64);

    #[cfg(not(feature = "no_index"))]
    {
        set_exported_fn!(lib, "+", string_funcs_array::append_array);
        set_exported_fn!(lib, "+", string_funcs_array::prepend_array);
    }

    lib.combine(exported_module!(string_functions));
    set_exported_fn!(lib, "contains", string_funcs::contains_char);
    set_exported_fn!(lib, "contains", string_funcs::contains_string);
    set_exported_fn!(lib, "index_of", string_funcs::index_of_char);
    set_exported_fn!(lib, "index_of", string_funcs::index_of_char_starting_from);
    set_exported_fn!(lib, "index_of", string_funcs::index_of_string);
    set_exported_fn!(lib, "index_of", string_funcs::index_of_string_starting_from);
    set_exported_fn!(lib, "append", string_funcs::append_char);
    set_exported_fn!(lib, "append", string_funcs::append_string);
    set_exported_fn!(lib, "sub_string", string_funcs::sub_string);
    set_exported_fn!(lib, "sub_string", string_funcs::sub_string_starting_from);
    set_exported_fn!(lib, "crop", string_funcs::crop_string);
    set_exported_fn!(lib, "crop", string_funcs::crop_string_starting_from);
    set_exported_fn!(lib, "replace", string_funcs::replace_string);
    set_exported_fn!(lib, "replace", string_funcs::replace_char);
    set_exported_fn!(lib, "replace", string_funcs::replace_string_with_char);
    set_exported_fn!(lib, "replace", string_funcs::replace_char_with_string);

    lib.set_raw_fn(
        "pad",
        &[TypeId::of::<ImmutableString>(), TypeId::of::<INT>(), TypeId::of::<char>()],
        |_engine: &Engine, _: &Module, args: &mut [&mut Dynamic]| {
            let len = *args[1].read_lock::<INT>().unwrap();

            // Check if string will be over max size limit
            #[cfg(not(feature = "unchecked"))]
            if _engine.limits.max_string_size > 0 && len > 0 && (len as usize) > _engine.limits.max_string_size {
                return EvalAltResult::ErrorDataTooLarge(
                    "Length of string".to_string(),
                    _engine.limits.max_string_size,
                    len as usize,
                    Position::none(),
                ).into();
            }

            if len > 0 {
                let ch = mem::take(args[2]).cast::<char>();
                let mut s = args[0].write_lock::<ImmutableString>().unwrap();

                let orig_len = s.chars().count();

                if len as usize > orig_len  {
                    let p = s.make_mut();

                    for _ in 0..(len as usize - orig_len) {
                        p.push(ch);
                    }

                    #[cfg(not(feature = "unchecked"))]
                    if _engine.limits.max_string_size > 0 && s.len() > _engine.limits.max_string_size {
                        return EvalAltResult::ErrorDataTooLarge(
                            "Length of string".to_string(),
                            _engine.limits.max_string_size,
                            s.len(),
                            Position::none(),
                        ).into();
                    }
                }
            }

            Ok(())
        },
    );

    #[cfg(not(feature = "no_object"))]
    set_exported_fn!(lib, make_getter("len"), string_funcs::len);

    // Register string iterator
    lib.set_iter(
        TypeId::of::<ImmutableString>(),
        |arr| Box::new(
            arr.cast::<ImmutableString>().chars().collect::<Vec<_>>().into_iter().map(Into::into)
        ) as Box<dyn Iterator<Item = Dynamic>>,
    );
});

fn prepend<T: Display>(x: T, y: ImmutableString) -> String {
    format!("{}{}", x, y)
}
fn append<T: Display>(x: ImmutableString, y: T) -> String {
    format!("{}{}", x, y)
}

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
    pub fn len(s: &mut ImmutableString) -> INT {
        string_funcs::len(s)
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
}

#[cfg(not(feature = "no_index"))]
mod string_funcs_array {
    use crate::engine::Array;
    use crate::plugin::*;
    use crate::utils::ImmutableString;

    #[export_fn]
    pub fn append_array(x: &mut ImmutableString, y: Array) -> String {
        format!("{}{:?}", x, y)
    }
    #[export_fn]
    pub fn prepend_array(x: &mut Array, y: ImmutableString) -> String {
        format!("{:?}{}", x, y)
    }
}

mod string_funcs {
    use crate::parser::INT;
    use crate::plugin::*;
    use crate::utils::{ImmutableString, StaticVec};

    #[export_fn]
    pub fn append_unit(s: ImmutableString, _x: ()) -> ImmutableString {
        s
    }
    #[export_fn]
    pub fn prepend_unit(_x: (), s: ImmutableString) -> ImmutableString {
        s
    }
    #[export_fn]
    pub fn len(s: &mut ImmutableString) -> INT {
        s.chars().count() as INT
    }
    #[export_fn]
    pub fn contains_char(s: &mut ImmutableString, ch: char) -> bool {
        s.contains(ch)
    }
    #[export_fn]
    pub fn contains_string(s: &mut ImmutableString, find: ImmutableString) -> bool {
        s.contains(find.as_str())
    }
    #[export_fn]
    pub fn index_of_char_starting_from(s: &mut ImmutableString, ch: char, start: INT) -> INT {
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
    }
    #[export_fn]
    pub fn index_of_char(s: &mut ImmutableString, ch: char) -> INT {
        s.find(ch)
            .map(|index| s[0..index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[export_fn]
    pub fn index_of_string_starting_from(
        s: &mut ImmutableString,
        find: ImmutableString,
        start: INT,
    ) -> INT {
        let start = if start < 0 {
            0
        } else if (start as usize) >= s.chars().count() {
            return -1 as INT;
        } else {
            s.chars().take(start as usize).collect::<String>().len()
        };

        s[start..]
            .find(find.as_str())
            .map(|index| s[0..start + index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[export_fn]
    pub fn index_of_string(s: &mut ImmutableString, find: ImmutableString) -> INT {
        s.find(find.as_str())
            .map(|index| s[0..index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[export_fn]
    pub fn append_char(s: &mut ImmutableString, ch: char) {
        s.make_mut().push(ch);
    }
    #[export_fn]
    pub fn append_string(s: &mut ImmutableString, add: ImmutableString) {
        s.make_mut().push_str(add.as_str());
    }
    #[export_fn]
    pub fn sub_string(s: ImmutableString, start: INT, len: INT) -> ImmutableString {
        let offset = if s.is_empty() || len <= 0 {
            return "".to_string().into();
        } else if start < 0 {
            0
        } else if (start as usize) >= s.chars().count() {
            return "".to_string().into();
        } else {
            start as usize
        };

        let chars: StaticVec<_> = s.chars().collect();

        let len = if offset + (len as usize) > chars.len() {
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
    #[export_fn]
    pub fn sub_string_starting_from(s: ImmutableString, start: INT) -> ImmutableString {
        let len = s.len() as INT;
        sub_string(s, start, len)
    }
    #[export_fn]
    fn crop_string(s: &mut ImmutableString, start: INT, len: INT) {
        let offset = if s.is_empty() || len <= 0 {
            s.make_mut().clear();
            return;
        } else if start < 0 {
            0
        } else if (start as usize) >= s.chars().count() {
            s.make_mut().clear();
            return;
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
    }
    #[export_fn]
    pub fn crop_string_starting_from(s: &mut ImmutableString, start: INT) {
        crop_string(s, start, s.len() as INT);
    }
    #[export_fn]
    pub fn replace_string(s: &mut ImmutableString, find: ImmutableString, sub: ImmutableString) {
        *s = s.replace(find.as_str(), sub.as_str()).into();
    }
    #[export_fn]
    pub fn replace_string_with_char(s: &mut ImmutableString, find: ImmutableString, sub: char) {
        *s = s.replace(find.as_str(), &sub.to_string()).into();
    }
    #[export_fn]
    pub fn replace_char_with_string(s: &mut ImmutableString, find: char, sub: ImmutableString) {
        *s = s.replace(&find.to_string(), sub.as_str()).into();
    }
    #[export_fn]
    pub fn replace_char(s: &mut ImmutableString, find: char, sub: char) {
        *s = s.replace(&find.to_string(), &sub.to_string()).into();
    }
}
