//! A module containing all unsafe code.

use crate::any::Variant;
use crate::engine::State;

use crate::stdlib::{
    any::{Any, TypeId},
    borrow::Cow,
    boxed::Box,
    mem, ptr,
    string::ToString,
};

/// Cast a type into another type.
pub fn unsafe_try_cast<A: Any, B: Any>(a: A) -> Option<B> {
    if TypeId::of::<B>() == a.type_id() {
        // SAFETY: Just checked we have the right type. We explicitly forget the
        // value immediately after moving out, removing any chance of a destructor
        // running or value otherwise being used again.
        unsafe {
            let ret: B = ptr::read(&a as *const _ as *const B);
            mem::forget(a);
            Some(ret)
        }
    } else {
        None
    }
}

/// Cast a Boxed type into another type.
pub fn unsafe_cast_box<X: Variant, T: Variant>(item: Box<X>) -> Result<Box<T>, Box<X>> {
    // Only allow casting to the exact same type
    if TypeId::of::<X>() == TypeId::of::<T>() {
        // SAFETY: just checked whether we are pointing to the correct type
        unsafe {
            let raw: *mut dyn Any = Box::into_raw(item as Box<dyn Any>);
            Ok(Box::from_raw(raw as *mut T))
        }
    } else {
        // Return the consumed item for chaining.
        Err(item)
    }
}

/// # DANGEROUS!!!
///
/// A dangerous function that blindly casts a `&str` from one lifetime to a `Cow<str>` of
/// another lifetime.  This is mainly used to let us push a block-local variable into the
/// current `Scope` without cloning the variable name.  Doing this is safe because all local
/// variables in the `Scope` are cleared out before existing the block.
///
/// Force-casting a local variable's lifetime to the current `Scope`'s larger lifetime saves
/// on allocations and string cloning, thus avoids us having to maintain a chain of `Scope`'s.
pub fn unsafe_cast_var_name_to_lifetime<'s>(name: &str, state: &State) -> Cow<'s, str> {
    // If not at global level, we can force-cast
    if state.scope_level > 0 {
        // WARNING - force-cast the variable name into the scope's lifetime to avoid cloning it
        //           this is safe because all local variables are cleared at the end of the block
        unsafe { mem::transmute::<_, &'s str>(name) }.into()
    } else {
        // The variable is introduced at global (top) level and may persist after the script run.
        // Therefore, clone the variable name.
        name.to_string().into()
    }
}
