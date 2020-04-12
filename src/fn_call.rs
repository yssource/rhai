//! Helper module which defines `FnArgs` to make function calling easier.

#![allow(non_snake_case)]

use crate::any::{Dynamic, Variant};
use crate::stdlib::{string::String, vec, vec::Vec};

/// Trait that represent arguments to a function call.
/// Any data type that can be converted into a `Vec` of `Dynamic` values can be used
/// as arguments to a function call.
pub trait FuncArgs {
    /// Convert to a `Vec` of `Dynamic` arguments.
    fn into_vec(self) -> Vec<Dynamic>;
}

// Macro to implement `FuncArgs` for tuples of standard types (each can be
/// converted into `Dynamic`).
macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<$($p: Variant + Clone),*> FuncArgs for ($($p,)*)
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
        impl_args!();
    };
    (@pop $head:ident $(, $tail:ident)+) => {
        impl_args!($($tail),*);
    };
}

#[rustfmt::skip]
impl_args!(A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, T, U, V);
