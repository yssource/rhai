/// Runs `$code` if `$old` is of type `$t`.
///
/// This macro is primarily used for type casting between known types.
#[macro_export]
macro_rules! reify {
    ($old:ident, |$new:ident : $t:ty| $code:expr, || $fallback:expr) => {{
        if std::any::TypeId::of::<$t>() == std::any::Any::type_id(&$old) {
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

    ($old:ident => Option<$t:ty>) => {
        reify!($old, |v: $t| Some(v), || None)
    };
    ($old:expr => Option<$t:ty>) => {
        reify!($old, |v: $t| Some(v), || None)
    };

    ($old:ident => $t:ty) => {
        reify!($old, |v: $t| v, || unreachable!())
    };
    ($old:expr => $t:ty) => {
        reify!($old, |v: $t| v, || unreachable!())
    };
}
