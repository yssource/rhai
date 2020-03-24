//! Helper module which defines `FnArgs` to make function calling easier.

#![allow(non_snake_case)]

use crate::any::{Any, Dynamic};
use crate::parser::INT;

#[cfg(not(feature = "no_index"))]
use crate::engine::Array;

use crate::stdlib::{string::String, vec, vec::Vec};

/// Trait that represent arguments to a function call.
pub trait FuncArgs {
    /// Convert to a `Vec` of `Dynamic` arguments.
    fn into_vec(self) -> Vec<Dynamic>;
}

macro_rules! impl_std_args {
    ($($p:ty),*) => {
        $(
            impl FuncArgs for $p {
                fn into_vec(self) -> Vec<Dynamic> {
                    vec![self.into_dynamic()]
                }
            }
        )*
    };
}

impl_std_args!(String, char, bool);

#[cfg(not(feature = "no_index"))]
impl_std_args!(Array);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
impl_std_args!(u8, i8, u16, i16, u32, i32, u64, i64);

#[cfg(any(feature = "only_i32", feature = "only_i64"))]
impl_std_args!(INT);

#[cfg(not(feature = "no_float"))]
impl_std_args!(f32, f64);

macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<$($p: Any + Clone),*> FuncArgs for ($($p,)*)
        {
            fn into_vec(self) -> Vec<Dynamic> {
                let ($($p,)*) = self;

                #[allow(unused_variables, unused_mut)]
                let mut v = Vec::new();
                $(v.push($p.into_dynamic());)*

                v
            }
        }

        impl_args!(@pop $($p),*);
    };
    (@pop) => {
    };
    (@pop $head:ident) => {
    };
    (@pop $head:ident $(, $tail:ident)+) => {
        impl_args!($($tail),*);
    };
}

#[rustfmt::skip]
impl_args!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
