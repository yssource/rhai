//! A helper module containing unsafe utility functions.

#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::{Any, TypeId},
    mem, ptr,
};

/// Cast a type into another type.
///
/// # Undefined Behavior
///
/// It is UB if the types are not compatible.
#[inline(always)]
#[must_use]
pub fn unsafe_cast<A: Any, B: Any>(a: A) -> B {
    unsafe {
        let ret: B = ptr::read(&a as *const _ as *const B);
        // We explicitly forget the value immediately after moving out,
        // removing any chance of a destructor running or value otherwise
        // being used again.
        mem::forget(a);
        ret
    }
}

/// Cast a type into another type.
#[inline(always)]
#[must_use]
pub fn unsafe_try_cast<A: Any, B: Any>(a: A) -> Option<B> {
    if TypeId::of::<B>() == a.type_id() {
        // SAFETY: Just checked we have the right type.
        Some(unsafe_cast(a))
    } else {
        None
    }
}

/// Cast a Boxed type into another type.
#[inline(always)]
#[must_use]
pub fn unsafe_cast_box<X: Any, T: Any>(item: Box<X>) -> Option<T> {
    // Only allow casting to the exact same type
    if TypeId::of::<X>() == TypeId::of::<T>() {
        // SAFETY: just checked whether we are pointing to the correct type
        unsafe {
            let raw: *mut dyn Any = Box::into_raw(item as Box<dyn Any>);
            Some(*Box::from_raw(raw as *mut T))
        }
    } else {
        None
    }
}
