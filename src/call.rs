//! Helper module which defines `FnArgs`
//! to make function calling easier.

use crate::any::{Any, Variant};

pub trait FunArgs<'a> {
    fn into_vec(self) -> Vec<&'a mut Variant>;
}

macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<'a, $($p: Any + Clone),*> FunArgs<'a> for ($(&'a mut $p,)*)
        {
            fn into_vec(self) -> Vec<&'a mut Variant> {
                let ($($p,)*) = self;

                #[allow(unused_variables, unused_mut)]
                let mut v = Vec::new();
                $(v.push($p as &mut Variant);)*

                v
            }
        }

        impl_args!(@pop $($p),*);
    };
    (@pop) => {
    };
    (@pop $head:ident $(, $tail:ident)*) => {
        impl_args!($($tail),*);
    };
}

#[cfg_attr(rustfmt, rustfmt_skip)]
impl_args!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
