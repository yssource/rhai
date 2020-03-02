use std::any::TypeId;

use crate::any::{Any, Dynamic};
use crate::engine::{Engine, EvalAltResult, FnCallArgs};
use crate::parser::Position;

pub trait RegisterFn<FN, ARGS, RET> {
    fn register_fn(&mut self, name: &str, f: FN);
}
pub trait RegisterDynamicFn<FN, ARGS> {
    fn register_dynamic_fn(&mut self, name: &str, f: FN);
}

pub struct Ref<A>(A);
pub struct Mut<A>(A);

macro_rules! count_args {
    () => {0usize};
    ($head:ident $($tail:ident)*) => {1usize + count_args!($($tail)*)};
}

macro_rules! def_register {
    () => {
        def_register!(imp);
    };
    (imp $($par:ident => $mark:ty => $param:ty => $clone:expr),*) => {
        impl<
            $($par: Any + Clone,)*
            FN: Fn($($param),*) -> RET + 'static,
            RET: Any
        > RegisterFn<FN, ($($mark,)*), RET> for Engine
        {
            fn register_fn(&mut self, name: &str, f: FN) {
                let fn_name = name.to_string();

                let fun = move |mut args: FnCallArgs, pos: Position| {
                    // Check for length at the beginning to avoid
                    // per-element bound checks.
                    const NUM_ARGS: usize = count_args!($($par)*);

                    if args.len() != NUM_ARGS {
                        return Err(EvalAltResult::ErrorFunctionArgsMismatch(fn_name.clone(), NUM_ARGS, pos));
                    }

                    #[allow(unused_variables, unused_mut)]
                    let mut drain = args.drain(..);
                    $(
                    // Downcast every element, return in case of a type mismatch
                    let $par = ((*drain.next().unwrap()).downcast_mut() as Option<&mut $par>).unwrap();
                    )*

                    // Call the user-supplied function using ($clone) to
                    // potentially clone the value, otherwise pass the reference.
                    let r = f($(($clone)($par)),*);
                    Ok(Box::new(r) as Dynamic)
                };
                self.register_fn_raw(name.into(), Some(vec![$(TypeId::of::<$par>()),*]), Box::new(fun));
            }
        }

        impl<
            $($par: Any + Clone,)*
            FN: Fn($($param),*) -> Dynamic + 'static,
        > RegisterDynamicFn<FN, ($($mark,)*)> for Engine
        {
            fn register_dynamic_fn(&mut self, name: &str, f: FN) {
                let fn_name = name.to_string();

                let fun = move |mut args: FnCallArgs, pos: Position| {
                    // Check for length at the beginning to avoid
                    // per-element bound checks.
                    const NUM_ARGS: usize = count_args!($($par)*);

                    if args.len() != NUM_ARGS {
                        return Err(EvalAltResult::ErrorFunctionArgsMismatch(fn_name.clone(), NUM_ARGS, pos));
                    }

                    #[allow(unused_variables, unused_mut)]
                    let mut drain = args.drain(..);
                    $(
                    // Downcast every element, return in case of a type mismatch
                    let $par = ((*drain.next().unwrap()).downcast_mut() as Option<&mut $par>).unwrap();
                    )*

                    // Call the user-supplied function using ($clone) to
                    // potentially clone the value, otherwise pass the reference.
                    Ok(f($(($clone)($par)),*))
                };
                self.register_fn_raw(name.into(), Some(vec![$(TypeId::of::<$par>()),*]), Box::new(fun));
            }
        }

        //def_register!(imp_pop $($par => $mark => $param),*);
    };
    ($p0:ident $(, $p:ident)*) => {
        def_register!(imp $p0 => $p0 => $p0 => Clone::clone $(, $p => $p => $p => Clone::clone)*);
        def_register!(imp $p0 => Ref<$p0> => &$p0 => |x| { x } $(, $p => $p => $p => Clone::clone)*);
        def_register!(imp $p0 => Mut<$p0> => &mut $p0 => |x| { x } $(, $p => $p => $p => Clone::clone)*);

        def_register!($($p),*);
    };
//    (imp_pop) => {};
//    (imp_pop $head:ident => $head_mark:ty => $head_param:ty $(,$tail:ident => $tail_mark:ty => $tp:ty)*) => {
//        def_register!(imp $($tail => $tail_mark => $tp),*);
//    };
}

#[cfg_attr(rustfmt, rustfmt_skip)]
def_register!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
