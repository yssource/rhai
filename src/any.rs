//! Helper module which defines the `Any` trait to to allow dynamic value handling.

use crate::stdlib::{
    any::{type_name, TypeId},
    boxed::Box,
    fmt,
};

/// An raw value of any type.
///
/// Currently, `Variant` is not `Send` nor `Sync`, so it can practically be any type.
/// Turn on the `sync` feature to restrict it to only types that implement `Send + Sync`.
pub type Variant = dyn Any;

/// A boxed dynamic type containing any value.
///
/// Currently, `Dynamic` is not `Send` nor `Sync`, so it can practically be any type.
/// Turn on the `sync` feature to restrict it to only types that implement `Send + Sync`.
pub type Dynamic = Box<Variant>;

/// A trait covering any type.
#[cfg(feature = "sync")]
pub trait Any: crate::stdlib::any::Any + Send + Sync {
    /// Get the `TypeId` of this type.
    fn type_id(&self) -> TypeId;

    /// Get the name of this type.
    fn type_name(&self) -> &'static str;

    /// Convert into `Dynamic`.
    fn into_dynamic(&self) -> Dynamic;

    /// This trait may only be implemented by `rhai`.
    #[doc(hidden)]
    fn _closed(&self) -> _Private;
}

#[cfg(feature = "sync")]
impl<T: crate::stdlib::any::Any + Clone + Send + Sync + ?Sized> Any for T {
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }

    fn type_name(&self) -> &'static str {
        type_name::<T>()
    }

    fn into_dynamic(&self) -> Dynamic {
        Box::new(self.clone())
    }

    fn _closed(&self) -> _Private {
        _Private
    }
}

/// A trait covering any type.
#[cfg(not(feature = "sync"))]
pub trait Any: crate::stdlib::any::Any {
    /// Get the `TypeId` of this type.
    fn type_id(&self) -> TypeId;

    /// Get the name of this type.
    fn type_name(&self) -> &'static str;

    /// Convert into `Dynamic`.
    fn into_dynamic(&self) -> Dynamic;

    /// This trait may only be implemented by `rhai`.
    #[doc(hidden)]
    fn _closed(&self) -> _Private;
}

#[cfg(not(feature = "sync"))]
impl<T: crate::stdlib::any::Any + Clone + ?Sized> Any for T {
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }

    fn type_name(&self) -> &'static str {
        type_name::<T>()
    }

    fn into_dynamic(&self) -> Dynamic {
        Box::new(self.clone())
    }

    fn _closed(&self) -> _Private {
        _Private
    }
}

impl Variant {
    /// Is this `Variant` a specific type?
    pub fn is<T: Any>(&self) -> bool {
        TypeId::of::<T>() == <Variant as Any>::type_id(self)
    }

    /// Get a reference of a specific type to the `Variant`.
    /// Returns `None` if the cast fails.
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const Variant as *const T)) }
        } else {
            None
        }
    }

    /// Get a mutable reference of a specific type to the `Variant`.
    /// Returns `None` if the cast fails.
    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe { Some(&mut *(self as *mut Variant as *mut T)) }
        } else {
            None
        }
    }
}

impl fmt::Debug for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("?")
    }
}

impl Clone for Dynamic {
    fn clone(&self) -> Self {
        Any::into_dynamic(self.as_ref())
    }
}

/// An extension trait that allows down-casting a `Dynamic` value to a specific type.
pub trait AnyExt: Sized {
    /// Get a copy of a `Dynamic` value as a specific type.
    fn try_cast<T: Any + Clone>(self) -> Result<T, Self>;

    /// Get a copy of a `Dynamic` value as a specific type.
    ///
    /// # Panics
    ///
    /// Panics if the cast fails (e.g. the type of the actual value is not the same as the specified type).
    fn cast<T: Any + Clone>(self) -> T;

    /// This trait may only be implemented by `rhai`.
    #[doc(hidden)]
    fn _closed(&self) -> _Private;
}

impl AnyExt for Dynamic {
    /// Get a copy of the `Dynamic` value as a specific type.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Any, AnyExt};
    ///
    /// let x: Dynamic = 42_u32.into_dynamic();
    ///
    /// assert_eq!(x.try_cast::<u32>().unwrap(), 42);
    /// ```
    fn try_cast<T: Any + Clone>(self) -> Result<T, Self> {
        if self.is::<T>() {
            unsafe {
                let raw: *mut Variant = Box::into_raw(self);
                Ok(*Box::from_raw(raw as *mut T))
            }
        } else {
            Err(self)
        }
    }

    /// Get a copy of the `Dynamic` value as a specific type.
    ///
    /// # Panics
    ///
    /// Panics if the cast fails (e.g. the type of the actual value is not the same as the specified type).
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Any, AnyExt};
    ///
    /// let x: Dynamic = 42_u32.into_dynamic();
    ///
    /// assert_eq!(x.cast::<u32>(), 42);
    /// ```
    fn cast<T: Any + Clone>(self) -> T {
        self.try_cast::<T>().expect("cast failed")
    }

    fn _closed(&self) -> _Private {
        _Private
    }
}

/// Private type which ensures that `rhai::Any` and `rhai::AnyExt` can only
/// be implemented by this crate.
#[doc(hidden)]
pub struct _Private;
