use super::{
    create_new_package, reg_binary, reg_binary_mut, reg_trinary_mut, reg_unary_mut, Package,
    PackageLibrary, PackageLibraryStore,
};

use crate::any::{Dynamic, Variant};
use crate::engine::Array;
use crate::fn_register::{map_dynamic as map, map_identity as copy};
use crate::parser::INT;

use crate::stdlib::ops::Deref;

// Register array utility functions
fn push<T: Variant + Clone>(list: &mut Array, item: T) {
    list.push(Dynamic::from(item));
}
fn ins<T: Variant + Clone>(list: &mut Array, position: INT, item: T) {
    if position <= 0 {
        list.insert(0, Dynamic::from(item));
    } else if (position as usize) >= list.len() - 1 {
        push(list, item);
    } else {
        list.insert(position as usize, Dynamic::from(item));
    }
}
fn pad<T: Variant + Clone>(list: &mut Array, len: INT, item: T) {
    if len >= 0 {
        while list.len() < len as usize {
            push(list, item.clone());
        }
    }
}

pub struct BasicArrayPackage(PackageLibrary);

impl Deref for BasicArrayPackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

macro_rules! reg_op { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_binary_mut($lib, $op, $func::<$par>, map);)* };
}
macro_rules! reg_tri { ($lib:expr, $op:expr, $func:ident, $($par:ty),*) => {
    $(reg_trinary_mut($lib, $op, $func::<$par>, map);)* };
}

impl Package for BasicArrayPackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }

    fn init(lib: &mut PackageLibraryStore) {
        #[cfg(not(feature = "no_index"))]
        {
            reg_op!(lib, "push", push, INT, bool, char, String, Array, ());
            reg_tri!(lib, "pad", pad, INT, bool, char, String, Array, ());
            reg_tri!(lib, "insert", ins, INT, bool, char, String, Array, ());

            reg_binary_mut(lib, "append", |x: &mut Array, y: Array| x.extend(y), map);
            reg_binary(
                lib,
                "+",
                |mut x: Array, y: Array| {
                    x.extend(y);
                    x
                },
                map,
            );

            #[cfg(not(feature = "only_i32"))]
            #[cfg(not(feature = "only_i64"))]
            {
                reg_op!(lib, "push", push, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
                reg_tri!(lib, "pad", pad, i8, u8, i16, u16, i32, u32, i64, u64, i128, u128);
                reg_tri!(lib, "insert", ins, i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
            }

            #[cfg(not(feature = "no_float"))]
            {
                reg_op!(lib, "push", push, f32, f64);
                reg_tri!(lib, "pad", pad, f32, f64);
                reg_tri!(lib, "insert", ins, f32, f64);
            }

            reg_unary_mut(
                lib,
                "pop",
                |list: &mut Array| list.pop().unwrap_or_else(|| Dynamic::from_unit()),
                copy,
            );
            reg_unary_mut(
                lib,
                "shift",
                |list: &mut Array| {
                    if !list.is_empty() {
                        Dynamic::from_unit()
                    } else {
                        list.remove(0)
                    }
                },
                copy,
            );
            reg_binary_mut(
                lib,
                "remove",
                |list: &mut Array, len: INT| {
                    if len < 0 || (len as usize) >= list.len() {
                        Dynamic::from_unit()
                    } else {
                        list.remove(len as usize)
                    }
                },
                copy,
            );
            reg_unary_mut(lib, "len", |list: &mut Array| list.len() as INT, map);
            reg_unary_mut(lib, "clear", |list: &mut Array| list.clear(), map);
            reg_binary_mut(
                lib,
                "truncate",
                |list: &mut Array, len: INT| {
                    if len >= 0 {
                        list.truncate(len as usize);
                    }
                },
                map,
            );
        }
    }
}
