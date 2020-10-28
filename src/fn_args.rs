//! Helper module which defines `FuncArgs` to make function calling easier.

#![allow(non_snake_case)]

use crate::dynamic::{Dynamic, Variant};
use crate::StaticVec;

/// Trait that represents arguments to a function call.
/// Any data type that can be converted into a `Vec<Dynamic>` can be used
/// as arguments to a function call.
pub trait FuncArgs {
    /// Convert to a `StaticVec<Dynamic>` of the function call arguments.
    fn into_vec(self) -> StaticVec<Dynamic>;
}

/// Macro to implement `FuncArgs` for tuples of standard types (each can be
/// converted into `Dynamic`).
macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<$($p: Variant + Clone),*> FuncArgs for ($($p,)*)
        {
            #[inline]
            fn into_vec(self) -> StaticVec<Dynamic> {
                let ($($p,)*) = self;

                let mut _v = StaticVec::new();
                $(_v.push($p.into_dynamic());)*

                _v
            }
        }

        impl_args!(@pop $($p),*);
    };
    (@pop) => {
    };
    (@pop $head:ident) => {
        impl_args!();
    };
    (@pop $head:ident $(, $tail:ident)+) => {
        impl_args!($($tail),*);
    };
}

impl_args!(A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, T, U, V);
