//! Helper module which defines `FnArgs`
//! to make function calling easier.

use crate::any::{Any, Variant};

/// Trait that represent arguments to a function call.
pub trait FuncArgs<'a> {
    /// Convert to a `Vec` of `Variant` arguments.
    fn into_vec(self) -> Vec<&'a mut Variant>;
}

impl<'a> FuncArgs<'a> for Vec<&'a mut Variant> {
    fn into_vec(self) -> Self {
        self
    }
}

impl<'a, T: Any> FuncArgs<'a> for &'a mut Vec<T> {
    fn into_vec(self) -> Vec<&'a mut Variant> {
        self.iter_mut().map(|x| x as &mut Variant).collect()
    }
}

macro_rules! impl_args {
    ($($p:ident),*) => {
        impl<'a, $($p: Any + Clone),*> FuncArgs<'a> for ($(&'a mut $p,)*)
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
