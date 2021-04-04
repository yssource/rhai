#![allow(non_snake_case)]

use crate::plugin::*;
use crate::stdlib::{
    any::TypeId, boxed::Box, format, mem, string::String, string::ToString, vec::Vec,
};
use crate::{def_package, Dynamic, ImmutableString, StaticVec, INT};

use super::string_basic::{print_with_func, FUNC_TO_STRING};

def_package!(crate:MoreStringPackage:"Additional string utilities, including string building.", lib, {
    combine_with_exported_module!(lib, "string", string_functions);

    // Register string iterator
    lib.set_iter(
        TypeId::of::<ImmutableString>(),
        |string| Box::new(string.cast::<ImmutableString>().chars().collect::<Vec<_>>().into_iter().map(Into::into))
    );
});

#[export_module]
mod string_functions {
    use crate::ImmutableString;

    #[rhai_fn(name = "+", name = "append")]
    pub fn add_append(
        ctx: NativeCallContext,
        string: ImmutableString,
        mut item: Dynamic,
    ) -> ImmutableString {
        let s = print_with_func(FUNC_TO_STRING, &ctx, &mut item);

        if s.is_empty() {
            string
        } else {
            format!("{}{}", string, s).into()
        }
    }
    #[rhai_fn(name = "+", pure)]
    pub fn add_prepend(
        ctx: NativeCallContext,
        item: &mut Dynamic,
        string: &str,
    ) -> ImmutableString {
        let mut s = print_with_func(FUNC_TO_STRING, &ctx, item);

        if string.is_empty() {
            s
        } else {
            s.make_mut().push_str(string);
            s.into()
        }
    }

    #[rhai_fn(name = "+", name = "append")]
    pub fn add_append_str(string1: ImmutableString, string2: ImmutableString) -> ImmutableString {
        string1 + string2
    }

    #[rhai_fn(name = "+", name = "append")]
    pub fn add_append_unit(string: ImmutableString, _item: ()) -> ImmutableString {
        string
    }
    #[rhai_fn(name = "+")]
    pub fn add_prepend_unit(_item: (), string: ImmutableString) -> ImmutableString {
        string
    }

    #[rhai_fn(name = "len", get = "len")]
    pub fn len(string: &str) -> INT {
        string.chars().count() as INT
    }
    #[rhai_fn(name = "bytes", get = "bytes")]
    pub fn bytes(string: &str) -> INT {
        string.len() as INT
    }
    pub fn remove(string: &mut ImmutableString, sub_string: ImmutableString) {
        *string -= sub_string;
    }
    #[rhai_fn(name = "remove")]
    pub fn remove_char(string: &mut ImmutableString, character: char) {
        *string -= character;
    }
    pub fn clear(string: &mut ImmutableString) {
        string.make_mut().clear();
    }
    pub fn truncate(string: &mut ImmutableString, len: INT) {
        if len > 0 {
            let chars: StaticVec<_> = string.chars().collect();
            let copy = string.make_mut();
            copy.clear();
            copy.extend(chars.into_iter().take(len as usize));
        } else {
            string.make_mut().clear();
        }
    }
    pub fn trim(string: &mut ImmutableString) {
        let trimmed = string.trim();

        if trimmed.len() < string.len() {
            *string = trimmed.to_string().into();
        }
    }

    #[rhai_fn(name = "index_of")]
    pub fn index_of_char_starting_from(string: &str, character: char, start: INT) -> INT {
        let start = if start < 0 {
            0
        } else if start as usize >= string.chars().count() {
            return -1 as INT;
        } else {
            string
                .chars()
                .take(start as usize)
                .collect::<String>()
                .len()
        };

        string[start..]
            .find(character)
            .map(|index| string[0..start + index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[rhai_fn(name = "index_of")]
    pub fn index_of_char(string: &str, character: char) -> INT {
        string
            .find(character)
            .map(|index| string[0..index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[rhai_fn(name = "index_of")]
    pub fn index_of_string_starting_from(string: &str, find_string: &str, start: INT) -> INT {
        let start = if start < 0 {
            0
        } else if start as usize >= string.chars().count() {
            return -1 as INT;
        } else {
            string
                .chars()
                .take(start as usize)
                .collect::<String>()
                .len()
        };

        string[start..]
            .find(find_string)
            .map(|index| string[0..start + index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }
    #[rhai_fn(name = "index_of")]
    pub fn index_of(string: &str, find_string: &str) -> INT {
        string
            .find(find_string)
            .map(|index| string[0..index].chars().count() as INT)
            .unwrap_or(-1 as INT)
    }

    pub fn sub_string(
        ctx: NativeCallContext,
        string: &str,
        start: INT,
        len: INT,
    ) -> ImmutableString {
        let offset = if string.is_empty() || len <= 0 {
            return ctx.engine().empty_string.clone().into();
        } else if start < 0 {
            0
        } else if start as usize >= string.chars().count() {
            return ctx.engine().empty_string.clone().into();
        } else {
            start as usize
        };

        let chars: StaticVec<_> = string.chars().collect();

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
    pub fn sub_string_starting_from(
        ctx: NativeCallContext,
        string: &str,
        start: INT,
    ) -> ImmutableString {
        let len = string.len() as INT;
        sub_string(ctx, string, start, len)
    }

    #[rhai_fn(name = "crop")]
    pub fn crop(string: &mut ImmutableString, start: INT, len: INT) {
        let offset = if string.is_empty() || len <= 0 {
            string.make_mut().clear();
            return;
        } else if start < 0 {
            0
        } else if start as usize >= string.chars().count() {
            string.make_mut().clear();
            return;
        } else {
            start as usize
        };

        let chars: StaticVec<_> = string.chars().collect();

        let len = if offset + len as usize > chars.len() {
            chars.len() - offset
        } else {
            len as usize
        };

        let copy = string.make_mut();
        copy.clear();
        copy.extend(chars.iter().skip(offset).take(len));
    }
    #[rhai_fn(name = "crop")]
    pub fn crop_string_starting_from(string: &mut ImmutableString, start: INT) {
        crop(string, start, string.len() as INT);
    }

    #[rhai_fn(name = "replace")]
    pub fn replace(string: &mut ImmutableString, find_string: &str, substitute_string: &str) {
        *string = string.replace(find_string, substitute_string).into();
    }
    #[rhai_fn(name = "replace")]
    pub fn replace_string_with_char(
        string: &mut ImmutableString,
        find_string: &str,
        substitute_character: char,
    ) {
        *string = string
            .replace(find_string, &substitute_character.to_string())
            .into();
    }
    #[rhai_fn(name = "replace")]
    pub fn replace_char_with_string(
        string: &mut ImmutableString,
        find_character: char,
        substitute_string: &str,
    ) {
        *string = string
            .replace(&find_character.to_string(), substitute_string)
            .into();
    }
    #[rhai_fn(name = "replace")]
    pub fn replace_char(
        string: &mut ImmutableString,
        find_character: char,
        substitute_character: char,
    ) {
        *string = string
            .replace(
                &find_character.to_string(),
                &substitute_character.to_string(),
            )
            .into();
    }

    #[rhai_fn(return_raw)]
    pub fn pad(
        _ctx: NativeCallContext,
        string: &mut ImmutableString,
        len: INT,
        character: char,
    ) -> Result<(), Box<crate::EvalAltResult>> {
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
            let orig_len = string.chars().count();

            if len as usize > orig_len {
                let p = string.make_mut();

                for _ in 0..(len as usize - orig_len) {
                    p.push(character);
                }

                #[cfg(not(feature = "unchecked"))]
                if _ctx.engine().max_string_size() > 0
                    && string.len() > _ctx.engine().max_string_size()
                {
                    return crate::EvalAltResult::ErrorDataTooLarge(
                        "Length of string".to_string(),
                        crate::Position::NONE,
                    )
                    .into();
                }
            }
        }

        Ok(())
    }
    #[rhai_fn(name = "pad", return_raw)]
    pub fn pad_with_string(
        _ctx: NativeCallContext,
        string: &mut ImmutableString,
        len: INT,
        padding: &str,
    ) -> Result<(), Box<crate::EvalAltResult>> {
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
            let mut str_len = string.chars().count();
            let padding_len = padding.chars().count();

            if len as usize > str_len {
                let p = string.make_mut();

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
                if _ctx.engine().max_string_size() > 0
                    && string.len() > _ctx.engine().max_string_size()
                {
                    return crate::EvalAltResult::ErrorDataTooLarge(
                        "Length of string".to_string(),
                        crate::Position::NONE,
                    )
                    .into();
                }
            }
        }

        Ok(())
    }

    #[cfg(not(feature = "no_index"))]
    pub mod arrays {
        use crate::stdlib::vec;
        use crate::{Array, ImmutableString};

        #[rhai_fn(name = "split")]
        pub fn chars(string: &str) -> Array {
            string.chars().map(Into::<Dynamic>::into).collect()
        }
        #[rhai_fn(name = "split")]
        pub fn split_at(ctx: NativeCallContext, string: ImmutableString, start: INT) -> Array {
            if start <= 0 {
                vec![ctx.engine().empty_string.clone().into(), string.into()]
            } else {
                let prefix: String = string.chars().take(start as usize).collect();
                let prefix_len = prefix.len();
                vec![prefix.into(), string[prefix_len..].into()]
            }
        }
        pub fn split(string: &str, delimiter: &str) -> Array {
            string.split(delimiter).map(Into::<Dynamic>::into).collect()
        }
        #[rhai_fn(name = "split")]
        pub fn splitn(string: &str, delimiter: &str, segments: INT) -> Array {
            let pieces: usize = if segments < 1 { 1 } else { segments as usize };
            string
                .splitn(pieces, delimiter)
                .map(Into::<Dynamic>::into)
                .collect()
        }
        #[rhai_fn(name = "split")]
        pub fn split_char(string: &str, delimiter: char) -> Array {
            string.split(delimiter).map(Into::<Dynamic>::into).collect()
        }
        #[rhai_fn(name = "split")]
        pub fn splitn_char(string: &str, delimiter: char, segments: INT) -> Array {
            let pieces: usize = if segments < 1 { 1 } else { segments as usize };
            string
                .splitn(pieces, delimiter)
                .map(Into::<Dynamic>::into)
                .collect()
        }
        #[rhai_fn(name = "split_rev")]
        pub fn rsplit(string: &str, delimiter: &str) -> Array {
            string
                .rsplit(delimiter)
                .map(Into::<Dynamic>::into)
                .collect()
        }
        #[rhai_fn(name = "split_rev")]
        pub fn rsplitn(string: &str, delimiter: &str, segments: INT) -> Array {
            let pieces: usize = if segments < 1 { 1 } else { segments as usize };
            string
                .rsplitn(pieces, delimiter)
                .map(Into::<Dynamic>::into)
                .collect()
        }
        #[rhai_fn(name = "split_rev")]
        pub fn rsplit_char(string: &str, delimiter: char) -> Array {
            string
                .rsplit(delimiter)
                .map(Into::<Dynamic>::into)
                .collect()
        }
        #[rhai_fn(name = "split_rev")]
        pub fn rsplitn_char(string: &str, delimiter: char, segments: INT) -> Array {
            let pieces: usize = if segments < 1 { 1 } else { segments as usize };
            string
                .rsplitn(pieces, delimiter)
                .map(Into::<Dynamic>::into)
                .collect()
        }
    }
}
