use std::any::{type_name, Any as StdAny, TypeId};
use std::fmt;

pub type Variant = dyn Any;
pub type Dynamic = Box<Variant>;

pub trait Any: StdAny {
    fn type_id(&self) -> TypeId;

    fn type_name(&self) -> &'static str;

    fn into_dynamic(&self) -> Dynamic;

    /// This type may only be implemented by `rhai`.
    #[doc(hidden)]
    fn _closed(&self) -> _Private;
}

impl<T> Any for T
where
    T: Clone + StdAny + ?Sized,
{
    #[inline]
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }

    fn type_name(&self) -> &'static str {
        type_name::<T>()
    }

    #[inline]
    fn into_dynamic(&self) -> Dynamic {
        Box::new(self.clone())
    }

    fn _closed(&self) -> _Private {
        _Private
    }
}

impl Variant {
    //#[inline]
    // fn into_dynamic(&self) -> Box<Variant> {
    //     Any::into_dynamic(self)
    // }
    #[inline]
    pub fn is<T: Any>(&self) -> bool {
        let t = TypeId::of::<T>();
        let boxed = <Variant as Any>::type_id(self);

        t == boxed
    }

    #[inline]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const Variant as *const T)) }
        } else {
            None
        }
    }

    #[inline]
    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe { Some(&mut *(self as *mut Variant as *mut T)) }
        } else {
            None
        }
    }
}

impl Clone for Dynamic {
    fn clone(&self) -> Self {
        Any::into_dynamic(self.as_ref())
    }
}

impl fmt::Debug for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("?")
    }
}

pub trait AnyExt: Sized {
    fn downcast<T: Any + Clone>(self) -> Result<Box<T>, Self>;
}

impl AnyExt for Dynamic {
    fn downcast<T: Any + Clone>(self) -> Result<Box<T>, Self> {
        if self.is::<T>() {
            unsafe {
                let raw: *mut Variant = Box::into_raw(self);
                Ok(Box::from_raw(raw as *mut T))
            }
        } else {
            Err(self)
        }
    }
}

/// Private type which ensures that `rhai::Any` can only
/// be implemented by this crate.
#[doc(hidden)]
pub struct _Private;
