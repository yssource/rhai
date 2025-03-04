use crate::plugin::*;
use crate::{def_package, FnPtr, INT};
use std::any::TypeId;
use std::fmt::{Binary, LowerHex, Octal};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_object"))]
use crate::Map;

pub const FUNC_TO_STRING: &str = "to_string";
pub const FUNC_TO_DEBUG: &str = "to_debug";

def_package! {
    /// Package of basic string utilities (e.g. printing)
    pub BasicStringPackage(lib) {
        lib.standard = true;

        combine_with_exported_module!(lib, "print_debug", print_debug_functions);
        combine_with_exported_module!(lib, "number_formatting", number_formatting);

        // Register characters iterator
        #[cfg(not(feature = "no_index"))]
        lib.set_iter(TypeId::of::<ImmutableString>(), |value| Box::new(
            value.cast::<ImmutableString>().chars().map(Into::into).collect::<crate::Array>().into_iter()
        ));
    }
}

// Register print and debug

#[inline]
pub fn print_with_func(
    fn_name: &str,
    ctx: &NativeCallContext,
    value: &mut Dynamic,
) -> crate::ImmutableString {
    match ctx.call_fn_raw(fn_name, true, false, &mut [value]) {
        Ok(result) if result.is::<crate::ImmutableString>() => {
            result.into_immutable_string().expect("`ImmutableString`")
        }
        Ok(result) => ctx.engine().map_type_name(result.type_name()).into(),
        Err(_) => ctx.engine().map_type_name(value.type_name()).into(),
    }
}

#[export_module]
mod print_debug_functions {
    use crate::ImmutableString;

    /// Convert the value of the `item` into a string.
    #[rhai_fn(name = "print", pure)]
    pub fn print_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        print_with_func(FUNC_TO_STRING, &ctx, item)
    }
    /// Convert the value of the `item` into a string.
    #[rhai_fn(name = "to_string", pure)]
    pub fn to_string_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        ctx.engine().map_type_name(&item.to_string()).into()
    }
    /// Convert the value of the `item` into a string in debug format.
    #[rhai_fn(name = "debug", pure)]
    pub fn debug_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        print_with_func(FUNC_TO_DEBUG, &ctx, item)
    }
    /// Convert the value of the `item` into a string in debug format.
    #[rhai_fn(name = "to_debug", pure)]
    pub fn to_debug_generic(ctx: NativeCallContext, item: &mut Dynamic) -> ImmutableString {
        ctx.engine().map_type_name(&format!("{:?}", item)).into()
    }

    /// Return the empty string.
    #[rhai_fn(name = "print", name = "debug")]
    pub fn print_empty_string(ctx: NativeCallContext) -> ImmutableString {
        ctx.engine().const_empty_string()
    }

    /// Return the `string`.
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_string(string: ImmutableString) -> ImmutableString {
        string
    }
    /// Convert the string into debug format.
    #[rhai_fn(name = "debug", name = "to_debug", pure)]
    pub fn debug_string(string: &mut ImmutableString) -> ImmutableString {
        format!("{:?}", string).into()
    }

    /// Return the character into a string.
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_char(character: char) -> ImmutableString {
        character.to_string().into()
    }
    /// Convert the string into debug format.
    #[rhai_fn(name = "debug", name = "to_debug")]
    pub fn debug_char(character: char) -> ImmutableString {
        format!("{:?}", character).into()
    }

    /// Convert the function pointer into a string in debug format.
    #[rhai_fn(name = "debug", name = "to_debug", pure)]
    pub fn debug_fn_ptr(f: &mut FnPtr) -> ImmutableString {
        f.to_string().into()
    }

    /// Return the boolean value into a string.
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_bool(value: bool) -> ImmutableString {
        format!("{}", value).into()
    }
    /// Convert the boolean value into a string in debug format.
    #[rhai_fn(name = "debug", name = "to_debug")]
    pub fn debug_bool(value: bool) -> ImmutableString {
        format!("{:?}", value).into()
    }

    /// Return the empty string.
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_unit(ctx: NativeCallContext, unit: ()) -> ImmutableString {
        let _ = unit;
        ctx.engine().const_empty_string()
    }
    /// Convert the unit into a string in debug format.
    #[rhai_fn(name = "debug", name = "to_debug")]
    pub fn debug_unit(unit: ()) -> ImmutableString {
        let _ = unit;
        "()".into()
    }

    /// Convert the value of `number` into a string.
    #[cfg(not(feature = "no_float"))]
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_f64(number: f64) -> ImmutableString {
        crate::ast::FloatWrapper::new(number).to_string().into()
    }
    /// Convert the value of `number` into a string.
    #[cfg(not(feature = "no_float"))]
    #[rhai_fn(name = "print", name = "to_string")]
    pub fn print_f32(number: f32) -> ImmutableString {
        crate::ast::FloatWrapper::new(number).to_string().into()
    }
    /// Convert the value of `number` into a string.
    #[cfg(not(feature = "no_float"))]
    #[rhai_fn(name = "debug", name = "to_debug")]
    pub fn debug_f64(number: f64) -> ImmutableString {
        format!("{:?}", crate::ast::FloatWrapper::new(number)).into()
    }
    /// Convert the value of `number` into a string.
    #[cfg(not(feature = "no_float"))]
    #[rhai_fn(name = "debug", name = "to_debug")]
    pub fn debug_f32(number: f32) -> ImmutableString {
        format!("{:?}", crate::ast::FloatWrapper::new(number)).into()
    }

    /// Convert the array into a string.
    #[cfg(not(feature = "no_index"))]
    #[rhai_fn(
        name = "print",
        name = "to_string",
        name = "debug",
        name = "to_debug",
        pure
    )]
    pub fn format_array(ctx: NativeCallContext, array: &mut Array) -> ImmutableString {
        let len = array.len();
        let mut result = String::with_capacity(len * 5 + 2);
        result.push('[');

        array.iter_mut().enumerate().for_each(|(i, x)| {
            result.push_str(&print_with_func(FUNC_TO_DEBUG, &ctx, x));
            if i < len - 1 {
                result.push_str(", ");
            }
        });

        result.push(']');
        result.into()
    }

    /// Convert the object map into a string.
    #[cfg(not(feature = "no_object"))]
    #[rhai_fn(
        name = "print",
        name = "to_string",
        name = "debug",
        name = "to_debug",
        pure
    )]
    pub fn format_map(ctx: NativeCallContext, map: &mut Map) -> ImmutableString {
        let len = map.len();
        let mut result = String::with_capacity(len * 5 + 3);
        result.push_str("#{");

        map.iter_mut().enumerate().for_each(|(i, (k, v))| {
            use std::fmt::Write;

            write!(
                result,
                "{:?}: {}{}",
                k,
                &print_with_func(FUNC_TO_DEBUG, &ctx, v),
                if i < len - 1 { ", " } else { "" }
            )
            .unwrap();
        });

        result.push('}');
        result.into()
    }
}

#[export_module]
mod number_formatting {
    fn to_hex<T: LowerHex>(value: T) -> ImmutableString {
        format!("{:x}", value).into()
    }
    fn to_octal<T: Octal>(value: T) -> ImmutableString {
        format!("{:o}", value).into()
    }
    fn to_binary<T: Binary>(value: T) -> ImmutableString {
        format!("{:b}", value).into()
    }

    /// Convert the `value` into a string in hex format.
    #[rhai_fn(name = "to_hex")]
    pub fn int_to_hex(value: INT) -> ImmutableString {
        to_hex(value)
    }
    /// Convert the `value` into a string in octal format.
    #[rhai_fn(name = "to_octal")]
    pub fn int_to_octal(value: INT) -> ImmutableString {
        to_octal(value)
    }
    /// Convert the `value` into a string in binary format.
    #[rhai_fn(name = "to_binary")]
    pub fn int_to_binary(value: INT) -> ImmutableString {
        to_binary(value)
    }

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    pub mod numbers {
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn u8_to_hex(value: u8) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn u16_to_hex(value: u16) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn u32_to_hex(value: u32) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn u64_to_hex(value: u64) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn i8_to_hex(value: i8) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn i16_to_hex(value: i16) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn i32_to_hex(value: i32) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in hex format.
        #[rhai_fn(name = "to_hex")]
        pub fn i64_to_hex(value: i64) -> ImmutableString {
            to_hex(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn u8_to_octal(value: u8) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn u16_to_octal(value: u16) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn u32_to_octal(value: u32) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn u64_to_octal(value: u64) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn i8_to_octal(value: i8) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn i16_to_octal(value: i16) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn i32_to_octal(value: i32) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in octal format.
        #[rhai_fn(name = "to_octal")]
        pub fn i64_to_octal(value: i64) -> ImmutableString {
            to_octal(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn u8_to_binary(value: u8) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn u16_to_binary(value: u16) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn u32_to_binary(value: u32) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn u64_to_binary(value: u64) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn i8_to_binary(value: i8) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn i16_to_binary(value: i16) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn i32_to_binary(value: i32) -> ImmutableString {
            to_binary(value)
        }
        /// Convert the `value` into a string in binary format.
        #[rhai_fn(name = "to_binary")]
        pub fn i64_to_binary(value: i64) -> ImmutableString {
            to_binary(value)
        }

        #[cfg(not(target_family = "wasm"))]

        pub mod num_128 {
            /// Convert the `value` into a string in hex format.
            #[rhai_fn(name = "to_hex")]
            pub fn u128_to_hex(value: u128) -> ImmutableString {
                to_hex(value)
            }
            /// Convert the `value` into a string in hex format.
            #[rhai_fn(name = "to_hex")]
            pub fn i128_to_hex(value: i128) -> ImmutableString {
                to_hex(value)
            }
            /// Convert the `value` into a string in octal format.
            #[rhai_fn(name = "to_octal")]
            pub fn u128_to_octal(value: u128) -> ImmutableString {
                to_octal(value)
            }
            /// Convert the `value` into a string in octal format.
            #[rhai_fn(name = "to_octal")]
            pub fn i128_to_octal(value: i128) -> ImmutableString {
                to_octal(value)
            }
            /// Convert the `value` into a string in binary format.
            #[rhai_fn(name = "to_binary")]
            pub fn u128_to_binary(value: u128) -> ImmutableString {
                to_binary(value)
            }
            /// Convert the `value` into a string in binary format.
            #[rhai_fn(name = "to_binary")]
            pub fn i128_to_binary(value: i128) -> ImmutableString {
                to_binary(value)
            }
        }
    }
}
