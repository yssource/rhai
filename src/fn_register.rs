//! Module which defines the function registration mechanism.

#![allow(non_snake_case)]

use crate::dynamic::{DynamicWriteLock, Variant};
use crate::fn_native::{CallableFunction, FnAny, FnCallArgs, SendSync};
use crate::r#unsafe::unsafe_try_cast;
use crate::stdlib::{any::TypeId, boxed::Box, mem, string::String};
use crate::{Dynamic, Engine, FnAccess, FnNamespace, NativeCallContext, RhaiResult};

// These types are used to build a unique _marker_ tuple type for each combination
// of function parameter types in order to make each trait implementation unique.
// That is because stable Rust currently does not allow distinguishing implementations
// based purely on parameter types of traits (`Fn`, `FnOnce` and `FnMut`).
//
// For example:
//
// `NativeFunction<(Mut<A>, B, Ref<C>), R>`
//
// will have the function prototype constraint to:
//
// `FN: (&mut A, B, &C) -> R`
//
// These types are not actually used anywhere.
pub struct Mut<T>(T);
//pub struct Ref<T>(T);

/// Dereference into DynamicWriteLock
#[inline(always)]
pub fn by_ref<T: Variant + Clone>(data: &mut Dynamic) -> DynamicWriteLock<T> {
    // Directly cast the &mut Dynamic into DynamicWriteLock to access the underlying data.
    data.write_lock::<T>().unwrap()
}

/// Dereference into value.
#[inline(always)]
pub fn by_value<T: Variant + Clone>(data: &mut Dynamic) -> T {
    if TypeId::of::<T>() == TypeId::of::<&str>() {
        // If T is `&str`, data must be `ImmutableString`, so map directly to it
        data.flatten_in_place();
        let ref_str = data
            .as_str_ref()
            .expect("argument passed by value should not be shared");
        let ref_t = unsafe { mem::transmute::<_, &T>(&ref_str) };
        ref_t.clone()
    } else if TypeId::of::<T>() == TypeId::of::<String>() {
        // If T is `String`, data must be `ImmutableString`, so map directly to it
        unsafe_try_cast(mem::take(data).take_string().unwrap()).unwrap()
    } else {
        // We consume the argument and then replace it with () - the argument is not supposed to be used again.
        // This way, we avoid having to clone the argument again, because it is already a clone when passed here.
        mem::take(data).cast::<T>()
    }
}

/// Trait to register custom functions with an [`Engine`].
pub trait RegisterNativeFunction<Args, Result> {
    /// Register the function with an [`Engine`].
    fn register_into(self, engine: &mut Engine, name: &str);
}

macro_rules! def_register {
    () => {
        def_register!(imp from_pure :);
    };
    (imp $abi:ident : $($par:ident => $arg:expr => $mark:ty => $param:ty => $let:stmt => $clone:expr),*) => {
    //   ^ function ABI type
    //                  ^ function parameter generic type name (A, B, C etc.)
    //                                ^ call argument(like A, *B, &mut C etc)
    //                                             ^ function parameter marker type (T, Ref<T> or Mut<T>)
    //                                                         ^ function parameter actual type (T, &T or &mut T)
    //                                                                      ^ argument let statement

        impl<
            FN: Fn($($param),*) -> RET + SendSync + 'static,
            $($par: Variant + Clone,)*
            RET: Variant + Clone
        > RegisterNativeFunction<($($mark,)*), ()> for FN {
            #[inline(always)]
            fn register_into(self, engine: &mut Engine, name: &str) {
                engine.global_namespace.set_fn(name, FnNamespace::Global, FnAccess::Public, None,
                    &[$(TypeId::of::<$par>()),*],
                    CallableFunction::$abi(Box::new(move |_: NativeCallContext, args: &mut FnCallArgs| {
                        // The arguments are assumed to be of the correct number and types!
                        let mut _drain = args.iter_mut();
                        $($let $par = ($clone)(_drain.next().unwrap()); )*

                        // Call the function with each argument value
                        let r = self($($arg),*);

                        // Map the result
                        Ok(r.into_dynamic())
                    }) as Box<FnAny>)
                );
            }
        }

        impl<
            FN: for<'a> Fn(NativeCallContext<'a>, $($param),*) -> RET + SendSync + 'static,
            $($par: Variant + Clone,)*
            RET: Variant + Clone
        > RegisterNativeFunction<(NativeCallContext<'static>, $($mark,)*), ()> for FN {
            #[inline(always)]
            fn register_into(self, engine: &mut Engine, name: &str) {
                engine.global_namespace.set_fn(name, FnNamespace::Global, FnAccess::Public, None,
                    &[$(TypeId::of::<$par>()),*],
                    CallableFunction::$abi(Box::new(move |ctx: NativeCallContext, args: &mut FnCallArgs| {
                        // The arguments are assumed to be of the correct number and types!
                        let mut _drain = args.iter_mut();
                        $($let $par = ($clone)(_drain.next().unwrap()); )*

                        // Call the function with each argument value
                        let r = self(ctx, $($arg),*);

                        // Map the result
                        Ok(r.into_dynamic())
                    }) as Box<FnAny>)
                );
            }
        }

        impl<
            FN: Fn($($param),*) -> RhaiResult + SendSync + 'static,
            $($par: Variant + Clone,)*
        > RegisterNativeFunction<($($mark,)*), RhaiResult> for FN {
            #[inline(always)]
            fn register_into(self, engine: &mut Engine, name: &str) {
                engine.global_namespace.set_fn(name, FnNamespace::Global, FnAccess::Public, None,
                    &[$(TypeId::of::<$par>()),*],
                    CallableFunction::$abi(Box::new(move |_: NativeCallContext, args: &mut FnCallArgs| {
                        // The arguments are assumed to be of the correct number and types!
                        let mut _drain = args.iter_mut();
                        $($let $par = ($clone)(_drain.next().unwrap()); )*

                        // Call the function with each argument value
                        self($($arg),*)
                    }) as Box<FnAny>)
                );
            }
        }

        impl<
            FN: for<'a> Fn(NativeCallContext<'a>, $($param),*) -> RhaiResult + SendSync + 'static,
            $($par: Variant + Clone,)*
        > RegisterNativeFunction<(NativeCallContext<'static>, $($mark,)*), RhaiResult> for FN {
            #[inline(always)]
            fn register_into(self, engine: &mut Engine, name: &str) {
                engine.global_namespace.set_fn(name, FnNamespace::Global, FnAccess::Public, None,
                    &[$(TypeId::of::<$par>()),*],
                    CallableFunction::$abi(Box::new(move |ctx: NativeCallContext, args: &mut FnCallArgs| {
                        // The arguments are assumed to be of the correct number and types!
                        let mut _drain = args.iter_mut();
                        $($let $par = ($clone)(_drain.next().unwrap()); )*

                        // Call the function with each argument value
                        self(ctx, $($arg),*)
                    }) as Box<FnAny>)
                );
            }
        }

        //def_register!(imp_pop $($par => $mark => $param),*);
    };
    ($p0:ident $(, $p:ident)*) => {
        def_register!(imp from_pure   : $p0 => $p0      => $p0      => $p0      => let $p0     => by_value $(, $p => $p => $p => $p => let $p => by_value)*);
        def_register!(imp from_method : $p0 => &mut $p0 => Mut<$p0> => &mut $p0 => let mut $p0 => by_ref   $(, $p => $p => $p => $p => let $p => by_value)*);
        //                ^ CallableFunction constructor
        //                                                             ^ first parameter passed through
        //                                                                                                     ^ others passed by value (by_value)

        // Currently does not support first argument which is a reference, as there will be
        // conflicting implementations since &T: Any and T: Any cannot be distinguished
        //def_register!(imp $p0 => Ref<$p0> => &$p0     => by_ref   $(, $p => $p => $p => by_value)*);

        def_register!($($p),*);
    };
}

def_register!(A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, T, U, V);
