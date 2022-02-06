/// Runs `$code` if `$old` is of type `$t`.
///
/// This macro is primarily used for type casting between known types.
#[macro_export]
macro_rules! reify {
    ($old:ident, |$new:ident : $t:ty| $code:expr, || $fallback:expr) => {{
        #[allow(unused_imports)]
        use std::any::Any;

        if std::any::TypeId::of::<$t>() == $old.type_id() {
            // SAFETY: This is safe because we check to make sure the two types are
            // actually the same type.
            let $new: $t = unsafe { std::mem::transmute_copy(&std::mem::ManuallyDrop::new($old)) };
            $code
        } else {
            $fallback
        }
    }};
    ($old:expr, |$new:ident : $t:ty| $code:expr, || $fallback:expr) => {{
        let old = $old;
        reify!(old, |$new: $t| $code, || $fallback)
    }};
    ($old:ident, |$new:ident : $t:ty| $code:expr) => {
        reify!($old, |$new: $t| $code, || ())
    };
    ($old:expr, |$new:ident : $t:ty| $code:expr) => {
        reify!($old, |$new: $t| $code, || ())
    };
}

#[macro_export]
macro_rules! reify_dynamic {
    ($old:ident, |$new:ident : $t:ty| $code:expr) => {{
        #[allow(unused_imports)]
        use ::std::{
            any::{Any, TypeId},
            mem::{transmute_copy, ManuallyDrop},
        };
        if TypeId::of::<$t>() == TypeId::of::<crate::Dynamic>() {
            // SAFETY: This is safe because we check to make sure the two types are
            // actually the same type.
            let $new: $t = unsafe { transmute_copy(&ManuallyDrop::new($old)) };
            $code
        }
    }};
}
