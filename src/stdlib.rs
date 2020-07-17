//! Helper module which defines most of the needed features from `std` for `no-std` builds.

#[cfg(feature = "no_std")]
mod inner {
    pub use core::{
        any, arch, array, ascii, cell, char, clone, cmp, convert, default, f32, f64, ffi, fmt,
        future, hash, hint, i16, i32, i64, i8, isize, iter, marker, mem, num, ops, option, panic,
        pin, prelude, ptr, result, slice, str, task, time, u16, u32, u64, u8, usize,
    };

    #[cfg(not(target_arch = "wasm32"))]
    pub use core::{i128, u128};

    pub use alloc::{borrow, boxed, format, rc, string, sync, vec};

    pub use core_error as error;

    pub mod collections {
        pub use hashbrown::{HashMap, HashSet};
    }
}

#[cfg(not(feature = "no_std"))]
mod inner {
    pub use std::*;
}

pub use self::inner::*;
