//! Helper module which defines `FnArgs` to make function calling easier.

use crate::any::{Any, Dynamic};

/// Trait that represent arguments to a function call.
pub trait FuncArgs {
    /// Convert to a `Vec` of `Dynamic` arguments.
    fn into_vec(self) -> Vec<Dynamic>;
}

impl<T: Any> FuncArgs for &mut Vec<T> {
    fn into_vec(self) -> Vec<Dynamic> {
        self.iter_mut().map(|x| x.into_dynamic()).collect()
    }
}

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
    (@pop $head:ident $(, $tail:ident)*) => {
        impl_args!($($tail),*);
    };
}

#[cfg_attr(rustfmt, rustfmt_skip)]
impl_args!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
