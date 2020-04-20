use super::{
    create_new_package, reg_binary, reg_binary_mut, reg_none, reg_unary, reg_unary_mut, Package,
    PackageLibrary, PackageLibraryStore,
};

use crate::engine::{Array, Map, FUNC_TO_STRING, KEYWORD_DEBUG, KEYWORD_PRINT};
use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

use crate::stdlib::{
    fmt::{Debug, Display},
    format,
    ops::Deref,
};

// Register print and debug
fn to_debug<T: Debug>(x: &mut T) -> String {
    format!("{:?}", x)
}
fn to_string<T: Display>(x: &mut T) -> String {
    format!("{}", x)
}
fn format_map(x: &mut Map) -> String {
    format!("#{:?}", x)
}

macro_rules! reg_op { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_unary_mut($lib, $op, $func::<$par>, map);)* };
}

pub struct BasicStringPackage(PackageLibrary);

impl Deref for BasicStringPackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

impl Package for BasicStringPackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }

    fn init(lib: &mut PackageLibraryStore) {
        reg_op!(lib, KEYWORD_PRINT, to_string, String, INT, bool);
        reg_op!(lib, FUNC_TO_STRING, to_string, String, INT, bool);
        reg_op!(lib, KEYWORD_PRINT, to_string, String, char, String);
        reg_op!(lib, FUNC_TO_STRING, to_string, String, char, String);
        reg_none(lib, KEYWORD_PRINT, || "".to_string(), map);
        reg_unary(lib, KEYWORD_PRINT, |_: ()| "".to_string(), map);
        reg_unary(lib, FUNC_TO_STRING, |_: ()| "".to_string(), map);
        reg_op!(lib, KEYWORD_DEBUG, to_debug, String, INT, bool, ());
        reg_op!(lib, KEYWORD_DEBUG, to_debug, String, char, String);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            reg_op!(lib, KEYWORD_PRINT, to_string, String, i8, u8, i16, u16);
            reg_op!(lib, FUNC_TO_STRING, to_string, String, i8, u8, i16, u16);
            reg_op!(lib, KEYWORD_PRINT, to_string, String, i32, u32, i64, u64);
            reg_op!(lib, FUNC_TO_STRING, to_string, String, i32, u32, i64, u64);
            reg_op!(lib, KEYWORD_PRINT, to_string, String, i128, u128);
            reg_op!(lib, FUNC_TO_STRING, to_string, String, i128, u128);
            reg_op!(lib, KEYWORD_DEBUG, to_debug, String, i8, u8, i16, u16);
            reg_op!(lib, KEYWORD_DEBUG, to_debug, String, i32, u32, i64, u64);
            reg_op!(lib, KEYWORD_DEBUG, to_debug, String, i128, u128);
        }

        #[cfg(not(feature = "no_float"))]
        {
            reg_op!(lib, KEYWORD_PRINT, to_string, String, f32, f64);
            reg_op!(lib, FUNC_TO_STRING, to_string, String, f32, f64);
            reg_op!(lib, KEYWORD_DEBUG, to_debug, String, f32, f64);
        }

        #[cfg(not(feature = "no_index"))]
        {
            reg_op!(lib, KEYWORD_PRINT, to_debug, String, Array);
            reg_op!(lib, FUNC_TO_STRING, to_debug, String, Array);
            reg_op!(lib, KEYWORD_DEBUG, to_debug, String, Array);
        }

        #[cfg(not(feature = "no_object"))]
        {
            reg_unary_mut(lib, KEYWORD_PRINT, format_map, map);
            reg_unary_mut(lib, FUNC_TO_STRING, format_map, map);
            reg_unary_mut(lib, KEYWORD_DEBUG, format_map, map);
        }

        reg_binary(
            lib,
            "+",
            |mut s: String, ch: char| {
                s.push(ch);
                s
            },
            map,
        );
        reg_binary(
            lib,
            "+",
            |mut s: String, s2: String| {
                s.push_str(&s2);
                s
            },
            map,
        );
        reg_binary_mut(lib, "append", |s: &mut String, ch: char| s.push(ch), map);
        reg_binary_mut(
            lib,
            "append",
            |s: &mut String, add: String| s.push_str(&add),
            map,
        );
    }
}
