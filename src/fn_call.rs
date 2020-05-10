//! Helper module which defines `FnArgs` to make function calling easier.

#![allow(non_snake_case)]

use crate::any::{Dynamic, Variant};
use crate::utils::StaticVec;

/// Trait that represents arguments to a function call.
/// Any data type that can be converted into a `Vec<Dynamic>` can be used
/// as arguments to a function call.
pub trait FuncArgs {
    /// Convert to a `Vec<Dynamic>` of the function call arguments.
    fn into_vec(self) -> StaticVec<Dynamic>;
}

/// Macro to implement `FuncArgs` for tuples of standard types (each can be
/// converted into `Dynamic`).
macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<$($p: Variant + Clone),*> FuncArgs for ($($p,)*)
        {
            fn into_vec(self) -> StaticVec<Dynamic> {
                let ($($p,)*) = self;

                #[allow(unused_mut)]
                let mut v = StaticVec::new();
                $(v.push($p.into_dynamic());)*

                v
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
