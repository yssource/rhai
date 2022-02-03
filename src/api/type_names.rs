use crate::{
    Engine, ExclusiveRange, FnPtr, ImmutableString, InclusiveRange, Instant, Position, RhaiError,
    ERR,
};
use std::any::type_name;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// Map the name of a standard type into a friendly form.
#[inline]
#[must_use]
fn map_std_type_name(name: &str, shorthands: bool) -> &str {
    let name = name.trim();

    if name == type_name::<String>() {
        return if shorthands { "string" } else { "String" };
    }
    if name == type_name::<ImmutableString>() || name == "ImmutableString" {
        return if shorthands {
            "string"
        } else {
            "ImmutableString"
        };
    }
    if name == type_name::<&str>() {
        return if shorthands { "string" } else { "&str" };
    }
    #[cfg(feature = "decimal")]
    if name == type_name::<rust_decimal::Decimal>() {
        return if shorthands { "decimal" } else { "Decimal" };
    }
    if name == type_name::<FnPtr>() || name == "FnPtr" {
        return if shorthands { "Fn" } else { "FnPtr" };
    }
    #[cfg(not(feature = "no_index"))]
    if name == type_name::<crate::Array>() || name == "Array" {
        return if shorthands { "array" } else { "Array" };
    }
    #[cfg(not(feature = "no_index"))]
    if name == type_name::<crate::Blob>() || name == "Blob" {
        return if shorthands { "blob" } else { "Blob" };
    }
    #[cfg(not(feature = "no_object"))]
    if name == type_name::<crate::Map>() || name == "Map" {
        return if shorthands { "map" } else { "Map" };
    }
    #[cfg(not(feature = "no_std"))]
    if name == type_name::<Instant>() || name == "Instant" {
        return if shorthands { "timestamp" } else { "Instant" };
    }
    if name == type_name::<ExclusiveRange>() || name == "ExclusiveRange" {
        return if shorthands {
            "range"
        } else if cfg!(feature = "only_i32") {
            "Range<i32>"
        } else {
            "Range<i64>"
        };
    }
    if name == type_name::<InclusiveRange>() || name == "InclusiveRange" {
        return if shorthands {
            "range="
        } else if cfg!(feature = "only_i32") {
            "RangeInclusive<i32>"
        } else {
            "RangeInclusive<i64>"
        };
    }

    if name.starts_with("rhai::") {
        map_std_type_name(&name[6..], shorthands)
    } else {
        name
    }
}

impl Engine {
    /// Pretty-print a type name.
    ///
    /// If a type is registered via [`register_type_with_name`][Engine::register_type_with_name],
    /// the type name provided for the registration will be used.
    ///
    /// # Panics
    ///
    /// Panics if the type name is `&mut`.
    #[inline]
    #[must_use]
    pub fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .get(name)
            .map(|s| s.as_str())
            .unwrap_or_else(|| map_std_type_name(name, true))
    }

    /// Format a type name.
    ///
    /// If a type is registered via [`register_type_with_name`][Engine::register_type_with_name],
    /// the type name provided for the registration will be used.
    #[cfg(feature = "metadata")]
    #[inline]
    #[must_use]
    pub(crate) fn format_type_name<'a>(&'a self, name: &'a str) -> std::borrow::Cow<'a, str> {
        if name.starts_with("&mut ") {
            let x = &name[5..];
            let r = self.format_type_name(x);
            return if x != r {
                format!("&mut {}", r).into()
            } else {
                name.into()
            };
        }

        self.type_names
            .get(name)
            .map(|s| s.as_str())
            .unwrap_or_else(|| match name {
                "INT" => return type_name::<crate::INT>(),
                #[cfg(not(feature = "no_float"))]
                "FLOAT" => return type_name::<crate::FLOAT>(),
                _ => map_std_type_name(name, false),
            })
            .into()
    }

    /// Make a `Box<`[`EvalAltResult<ErrorMismatchDataType>`][ERR::ErrorMismatchDataType]`>`.
    #[inline(never)]
    #[must_use]
    pub(crate) fn make_type_mismatch_err<T>(&self, typ: &str, pos: Position) -> RhaiError {
        ERR::ErrorMismatchDataType(self.map_type_name(type_name::<T>()).into(), typ.into(), pos)
            .into()
    }
}
