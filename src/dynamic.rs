//! Helper module which defines the [`Any`] trait to to allow dynamic value handling.

use crate::fn_native::SendSync;
use crate::r#unsafe::{unsafe_cast_box, unsafe_try_cast};
use crate::stdlib::{
    any::{type_name, Any, TypeId},
    boxed::Box,
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    string::String,
};
use crate::{FnPtr, ImmutableString, INT};

#[cfg(not(feature = "no_float"))]
use crate::{ast::FloatWrapper, FLOAT};

#[cfg(feature = "decimal")]
use rust_decimal::Decimal;

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_object"))]
use crate::Map;

#[cfg(not(feature = "no_std"))]
#[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
use crate::stdlib::time::Instant;

use fmt::Debug;
#[cfg(not(feature = "no_std"))]
#[cfg(any(target_arch = "wasm32", target_arch = "wasm64"))]
use instant::Instant;

mod private {
    use crate::fn_native::SendSync;
    use crate::stdlib::any::Any;

    /// A sealed trait that prevents other crates from implementing [`Variant`].
    pub trait Sealed {}

    impl<T: Any + Clone + SendSync> Sealed for T {}
}

/// Trait to represent any type.
///
/// Currently, [`Variant`] is not [`Send`] nor [`Sync`], so it can practically be any type.
/// Turn on the `sync` feature to restrict it to only types that implement [`Send`] `+` [`Sync`].
#[cfg(not(feature = "sync"))]
pub trait Variant: Any + private::Sealed {
    /// Convert this [`Variant`] trait object to [`&dyn Any`][Any].
    fn as_any(&self) -> &dyn Any;

    /// Convert this [`Variant`] trait object to [`&mut dyn Any`][Any].
    fn as_mut_any(&mut self) -> &mut dyn Any;

    /// Convert this [`Variant`] trait object to an [`Any`] trait object.
    fn as_box_any(self: Box<Self>) -> Box<dyn Any>;

    /// Get the name of this type.
    fn type_name(&self) -> &'static str;

    /// Convert into [`Dynamic`].
    fn into_dynamic(self) -> Dynamic;

    /// Clone into [`Dynamic`].
    fn clone_into_dynamic(&self) -> Dynamic;
}

/// Trait to represent any type.
#[cfg(feature = "sync")]
pub trait Variant: Any + Send + Sync + private::Sealed {
    /// Convert this [`Variant`] trait object to [`&dyn Any`][Any].
    fn as_any(&self) -> &dyn Any;

    /// Convert this [`Variant`] trait object to [`&mut dyn Any`][Any].
    fn as_mut_any(&mut self) -> &mut dyn Any;

    /// Convert this [`Variant`] trait object to an [`Any`] trait object.
    fn as_box_any(self: Box<Self>) -> Box<dyn Any>;

    /// Get the name of this type.
    fn type_name(&self) -> &'static str;

    /// Convert into [`Dynamic`].
    fn into_dynamic(self) -> Dynamic;

    /// Clone into [`Dynamic`].
    fn clone_into_dynamic(&self) -> Dynamic;
}

impl<T: Any + Clone + SendSync> Variant for T {
    #[inline(always)]
    fn as_any(&self) -> &dyn Any {
        self
    }
    #[inline(always)]
    fn as_mut_any(&mut self) -> &mut dyn Any {
        self
    }
    #[inline(always)]
    fn as_box_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
    #[inline(always)]
    fn type_name(&self) -> &'static str {
        type_name::<T>()
    }
    #[inline(always)]
    fn into_dynamic(self) -> Dynamic {
        Dynamic::from(self)
    }
    #[inline(always)]
    fn clone_into_dynamic(&self) -> Dynamic {
        Dynamic::from(self.clone())
    }
}

impl dyn Variant {
    /// Is this [`Variant`] a specific type?
    #[inline(always)]
    pub fn is<T: Any>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }
}

/// Modes of access.
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum AccessMode {
    /// Mutable.
    ReadWrite,
    /// Immutable.
    ReadOnly,
}

impl AccessMode {
    /// Is the access type `ReadOnly`?
    #[inline(always)]
    pub fn is_read_only(self) -> bool {
        match self {
            Self::ReadWrite => false,
            Self::ReadOnly => true,
        }
    }
}

/// Dynamic type containing any value.
pub struct Dynamic(pub(crate) Union);

/// Internal [`Dynamic`] representation.
///
/// Most variants are boxed to reduce the size.
pub enum Union {
    /// The Unit value - ().
    Unit((), AccessMode),
    /// A boolean value.
    Bool(bool, AccessMode),
    /// An [`ImmutableString`] value.
    Str(ImmutableString, AccessMode),
    /// A character value.
    Char(char, AccessMode),
    /// An integer value.
    Int(INT, AccessMode),
    /// A floating-point value.
    #[cfg(not(feature = "no_float"))]
    Float(FloatWrapper, AccessMode),
    /// A fixed-precision decimal value.
    #[cfg(feature = "decimal")]
    Decimal(Box<Decimal>, AccessMode),
    /// An array value.
    #[cfg(not(feature = "no_index"))]
    Array(Box<Array>, AccessMode),
    /// An object map value.
    #[cfg(not(feature = "no_object"))]
    Map(Box<Map>, AccessMode),
    /// A function pointer.
    FnPtr(Box<FnPtr>, AccessMode),
    /// A timestamp value.
    #[cfg(not(feature = "no_std"))]
    TimeStamp(Box<Instant>, AccessMode),

    /// Any type as a trait object.
    Variant(Box<Box<dyn Variant>>, AccessMode),

    /// A _shared_ value of any type.
    #[cfg(not(feature = "no_closure"))]
    Shared(crate::Shared<crate::Locked<Dynamic>>, AccessMode),
}

/// Underlying [`Variant`] read guard for [`Dynamic`].
///
/// This data structure provides transparent interoperability between
/// normal [`Dynamic`] and shared [`Dynamic`] values.
#[derive(Debug)]
pub struct DynamicReadLock<'d, T: Variant + Clone>(DynamicReadLockInner<'d, T>);

/// Different types of read guards for [`DynamicReadLock`].
#[derive(Debug)]
enum DynamicReadLockInner<'d, T: Variant + Clone> {
    /// A simple reference to a non-shared value.
    Reference(&'d T),

    /// A read guard to a shared [`RefCell`][std::cell::RefCell].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(not(feature = "sync"))]
    Guard(crate::stdlib::cell::Ref<'d, Dynamic>),
    /// A read guard to a shared [`RwLock`][std::sync::RwLock].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(feature = "sync")]
    Guard(crate::stdlib::sync::RwLockReadGuard<'d, Dynamic>),
}

impl<'d, T: Variant + Clone> Deref for DynamicReadLock<'d, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        match &self.0 {
            DynamicReadLockInner::Reference(reference) => *reference,
            // Unwrapping is safe because all checking is already done in its constructor
            #[cfg(not(feature = "no_closure"))]
            DynamicReadLockInner::Guard(guard) => guard.downcast_ref().unwrap(),
        }
    }
}

/// Underlying [`Variant`] write guard for [`Dynamic`].
///
/// This data structure provides transparent interoperability between
/// normal [`Dynamic`] and shared [`Dynamic`] values.
#[derive(Debug)]
pub struct DynamicWriteLock<'d, T: Variant + Clone>(DynamicWriteLockInner<'d, T>);

/// Different types of write guards for [`DynamicReadLock`].
#[derive(Debug)]
enum DynamicWriteLockInner<'d, T: Variant + Clone> {
    /// A simple mutable reference to a non-shared value.
    Reference(&'d mut T),

    /// A write guard to a shared [`RefCell`][std::cell::RefCell].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(not(feature = "sync"))]
    Guard(crate::stdlib::cell::RefMut<'d, Dynamic>),
    /// A write guard to a shared [`RwLock`][std::sync::RwLock].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(feature = "sync")]
    Guard(crate::stdlib::sync::RwLockWriteGuard<'d, Dynamic>),
}

impl<'d, T: Variant + Clone> Deref for DynamicWriteLock<'d, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        match &self.0 {
            DynamicWriteLockInner::Reference(reference) => *reference,
            // Unwrapping is safe because all checking is already done in its constructor
            #[cfg(not(feature = "no_closure"))]
            DynamicWriteLockInner::Guard(guard) => guard.downcast_ref().unwrap(),
        }
    }
}

impl<'d, T: Variant + Clone> DerefMut for DynamicWriteLock<'d, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        match &mut self.0 {
            DynamicWriteLockInner::Reference(reference) => *reference,
            // Unwrapping is safe because all checking is already done in its constructor
            #[cfg(not(feature = "no_closure"))]
            DynamicWriteLockInner::Guard(guard) => guard.downcast_mut().unwrap(),
        }
    }
}

impl Dynamic {
    /// Does this [`Dynamic`] hold a variant data type
    /// instead of one of the supported system primitive types?
    #[inline(always)]
    pub fn is_variant(&self) -> bool {
        match self.0 {
            Union::Variant(_, _) => true,
            _ => false,
        }
    }
    /// Is the value held by this [`Dynamic`] shared?
    ///
    /// Always [`false`] under the `no_closure` feature.
    #[inline(always)]
    pub fn is_shared(&self) -> bool {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => true,
            _ => false,
        }
    }
    /// Is the value held by this [`Dynamic`] a particular type?
    ///
    /// If the [`Dynamic`] is a shared variant checking is performed on
    /// top of its internal value.
    #[inline(always)]
    pub fn is<T: Variant + Clone>(&self) -> bool {
        let mut target_type_id = TypeId::of::<T>();

        if target_type_id == TypeId::of::<String>() {
            target_type_id = TypeId::of::<ImmutableString>();
        }

        self.type_id() == target_type_id
    }
    /// Get the [`TypeId`] of the value held by this [`Dynamic`].
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    pub fn type_id(&self) -> TypeId {
        match &self.0 {
            Union::Unit(_, _) => TypeId::of::<()>(),
            Union::Bool(_, _) => TypeId::of::<bool>(),
            Union::Str(_, _) => TypeId::of::<ImmutableString>(),
            Union::Char(_, _) => TypeId::of::<char>(),
            Union::Int(_, _) => TypeId::of::<INT>(),
            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _) => TypeId::of::<FLOAT>(),
            #[cfg(feature = "decimal")]
            Union::Decimal(_, _) => TypeId::of::<Decimal>(),
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _) => TypeId::of::<Array>(),
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _) => TypeId::of::<Map>(),
            Union::FnPtr(_, _) => TypeId::of::<FnPtr>(),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _) => TypeId::of::<Instant>(),

            Union::Variant(value, _) => (***value).type_id(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _) => (*cell.borrow()).type_id(),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _) => (*cell.read().unwrap()).type_id(),
        }
    }
    /// Get the name of the type of the value held by this [`Dynamic`].
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    pub fn type_name(&self) -> &'static str {
        match &self.0 {
            Union::Unit(_, _) => "()",
            Union::Bool(_, _) => "bool",
            Union::Str(_, _) => "string",
            Union::Char(_, _) => "char",
            Union::Int(_, _) => type_name::<INT>(),
            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _) => type_name::<FLOAT>(),
            #[cfg(feature = "decimal")]
            Union::Decimal(_, _) => "decimal",
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _) => "array",
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _) => "map",
            Union::FnPtr(_, _) => "Fn",
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _) => "timestamp",

            Union::Variant(value, _) => (***value).type_name(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _) => cell
                .try_borrow()
                .map(|v| (*v).type_name())
                .unwrap_or("<shared>"),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _) => (*cell.read().unwrap()).type_name(),
        }
    }
}

impl Hash for Dynamic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Union::Unit(_, _) => ().hash(state),
            Union::Bool(value, _) => value.hash(state),
            Union::Str(s, _) => s.hash(state),
            Union::Char(ch, _) => ch.hash(state),
            Union::Int(i, _) => i.hash(state),
            #[cfg(not(feature = "no_float"))]
            Union::Float(f, _) => f.hash(state),
            #[cfg(not(feature = "no_index"))]
            Union::Array(a, _) => (**a).hash(state),
            #[cfg(not(feature = "no_object"))]
            Union::Map(m, _) => {
                let mut buf: crate::StaticVec<_> = m.iter().collect();
                buf.sort_by(|(a, _), (b, _)| a.cmp(b));

                buf.into_iter().for_each(|(key, value)| {
                    key.hash(state);
                    value.hash(state);
                })
            }

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _) => (*cell.borrow()).hash(state),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _) => (*cell.read().unwrap()).hash(state),

            _ => unimplemented!(),
        }
    }
}

/// Map the name of a standard type into a friendly form.
#[inline]
pub(crate) fn map_std_type_name(name: &str) -> &str {
    if name == type_name::<String>() {
        "string"
    } else if name == type_name::<ImmutableString>() {
        "string"
    } else if name == type_name::<&str>() {
        "string"
    } else if name == type_name::<FnPtr>() {
        "Fn"
    } else {
        #[cfg(feature = "decimal")]
        if name == type_name::<Decimal>() {
            return "decimal";
        }
        #[cfg(not(feature = "no_index"))]
        if name == type_name::<Array>() {
            return "array";
        }
        #[cfg(not(feature = "no_object"))]
        if name == type_name::<Map>() {
            return "map";
        }
        #[cfg(not(feature = "no_std"))]
        if name == type_name::<Instant>() {
            return "timestamp";
        }

        name
    }
}

impl fmt::Display for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Union::Unit(_, _) => write!(f, ""),
            Union::Bool(value, _) => fmt::Display::fmt(value, f),
            Union::Str(value, _) => fmt::Display::fmt(value, f),
            Union::Char(value, _) => fmt::Display::fmt(value, f),
            Union::Int(value, _) => fmt::Display::fmt(value, f),
            #[cfg(not(feature = "no_float"))]
            Union::Float(value, _) => fmt::Display::fmt(value, f),
            #[cfg(feature = "decimal")]
            Union::Decimal(value, _) => fmt::Display::fmt(value, f),
            #[cfg(not(feature = "no_index"))]
            Union::Array(value, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_object"))]
            Union::Map(value, _) => {
                f.write_str("#")?;
                fmt::Debug::fmt(value, f)
            }
            Union::FnPtr(value, _) => fmt::Display::fmt(value, f),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _) => f.write_str("<timestamp>"),

            Union::Variant(value, _) => {
                let _type_id = (***value).type_id();

                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                if _type_id == TypeId::of::<u8>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<u8>().unwrap());
                } else if _type_id == TypeId::of::<u16>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<u16>().unwrap());
                } else if _type_id == TypeId::of::<u32>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<u32>().unwrap());
                } else if _type_id == TypeId::of::<u64>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<u64>().unwrap());
                } else if _type_id == TypeId::of::<i8>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<i8>().unwrap());
                } else if _type_id == TypeId::of::<i16>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<i16>().unwrap());
                } else if _type_id == TypeId::of::<i32>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<i32>().unwrap());
                } else if _type_id == TypeId::of::<i64>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<i64>().unwrap());
                }

                #[cfg(not(feature = "no_float"))]
                if _type_id == TypeId::of::<f32>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<f32>().unwrap());
                } else if _type_id == TypeId::of::<f64>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<f64>().unwrap());
                }

                #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
                if _type_id == TypeId::of::<u128>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<u128>().unwrap());
                } else if _type_id == TypeId::of::<i128>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<i128>().unwrap());
                }

                f.write_str((***value).type_name())
            }

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _) => {
                if let Ok(v) = cell.try_borrow() {
                    fmt::Display::fmt(&*v, f)
                } else {
                    f.write_str("<shared>")
                }
            }
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _) => fmt::Display::fmt(&*cell.read().unwrap(), f),
        }
    }
}

impl fmt::Debug for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Union::Unit(value, _) => fmt::Debug::fmt(value, f),
            Union::Bool(value, _) => fmt::Debug::fmt(value, f),
            Union::Str(value, _) => fmt::Debug::fmt(value, f),
            Union::Char(value, _) => fmt::Debug::fmt(value, f),
            Union::Int(value, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_float"))]
            Union::Float(value, _) => fmt::Debug::fmt(value, f),
            #[cfg(feature = "decimal")]
            Union::Decimal(value, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_index"))]
            Union::Array(value, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_object"))]
            Union::Map(value, _) => {
                f.write_str("#")?;
                fmt::Debug::fmt(value, f)
            }
            Union::FnPtr(value, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _) => write!(f, "<timestamp>"),

            Union::Variant(value, _) => {
                let _type_id = (***value).type_id();

                #[cfg(not(feature = "only_i32"))]
                #[cfg(not(feature = "only_i64"))]
                if _type_id == TypeId::of::<u8>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<u8>().unwrap());
                } else if _type_id == TypeId::of::<u16>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<u16>().unwrap());
                } else if _type_id == TypeId::of::<u32>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<u32>().unwrap());
                } else if _type_id == TypeId::of::<u64>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<u64>().unwrap());
                } else if _type_id == TypeId::of::<i8>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<i8>().unwrap());
                } else if _type_id == TypeId::of::<i16>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<i16>().unwrap());
                } else if _type_id == TypeId::of::<i32>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<i32>().unwrap());
                } else if _type_id == TypeId::of::<i64>() {
                    return write!(f, "{:?}", (**value).as_any().downcast_ref::<i64>().unwrap());
                }

                #[cfg(not(feature = "no_float"))]
                if _type_id == TypeId::of::<f32>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<f32>().unwrap());
                } else if _type_id == TypeId::of::<f64>() {
                    return write!(f, "{}", (**value).as_any().downcast_ref::<f64>().unwrap());
                }

                #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
                if _type_id == TypeId::of::<u128>() {
                    return write!(
                        f,
                        "{:?}",
                        (**value).as_any().downcast_ref::<u128>().unwrap()
                    );
                } else if _type_id == TypeId::of::<i128>() {
                    return write!(
                        f,
                        "{:?}",
                        (**value).as_any().downcast_ref::<i128>().unwrap()
                    );
                }

                write!(f, "{}", (*value).type_name())
            }

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _) => {
                if let Ok(v) = cell.try_borrow() {
                    write!(f, "{:?} (shared)", *v)
                } else {
                    f.write_str("<shared>")
                }
            }
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _) => fmt::Debug::fmt(&*cell.read().unwrap(), f),
        }
    }
}

impl Clone for Dynamic {
    /// Clone the [`Dynamic`] value.
    ///
    /// # WARNING
    ///
    /// The cloned copy is marked read-write even if the original is read-only.
    fn clone(&self) -> Self {
        match self.0 {
            Union::Unit(value, _) => Self(Union::Unit(value, AccessMode::ReadWrite)),
            Union::Bool(value, _) => Self(Union::Bool(value, AccessMode::ReadWrite)),
            Union::Str(ref value, _) => Self(Union::Str(value.clone(), AccessMode::ReadWrite)),
            Union::Char(value, _) => Self(Union::Char(value, AccessMode::ReadWrite)),
            Union::Int(value, _) => Self(Union::Int(value, AccessMode::ReadWrite)),
            #[cfg(not(feature = "no_float"))]
            Union::Float(value, _) => Self(Union::Float(value, AccessMode::ReadWrite)),
            #[cfg(feature = "decimal")]
            Union::Decimal(ref value, _) => {
                Self(Union::Decimal(value.clone(), AccessMode::ReadWrite))
            }
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref value, _) => Self(Union::Array(value.clone(), AccessMode::ReadWrite)),
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref value, _) => Self(Union::Map(value.clone(), AccessMode::ReadWrite)),
            Union::FnPtr(ref value, _) => Self(Union::FnPtr(value.clone(), AccessMode::ReadWrite)),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(ref value, _) => {
                Self(Union::TimeStamp(value.clone(), AccessMode::ReadWrite))
            }

            Union::Variant(ref value, _) => (***value).clone_into_dynamic(),

            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, _) => Self(Union::Shared(cell.clone(), AccessMode::ReadWrite)),
        }
    }
}

impl Default for Dynamic {
    #[inline(always)]
    fn default() -> Self {
        Self::UNIT
    }
}

impl Dynamic {
    /// A [`Dynamic`] containing a `()`.
    pub const UNIT: Dynamic = Self(Union::Unit((), AccessMode::ReadWrite));
    /// A [`Dynamic`] containing a `true`.
    pub const TRUE: Dynamic = Self(Union::Bool(true, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing a [`false`].
    pub const FALSE: Dynamic = Self(Union::Bool(false, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the integer zero.
    pub const ZERO: Dynamic = Self(Union::Int(0, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the integer one.
    pub const ONE: Dynamic = Self(Union::Int(1, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the integer negative one.
    pub const NEGATIVE_ONE: Dynamic = Self(Union::Int(-1, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the floating-point zero.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_ZERO: Dynamic =
        Self(Union::Float(FloatWrapper::new(0.0), AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the floating-point one.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_ONE: Dynamic =
        Self(Union::Float(FloatWrapper::new(1.0), AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the floating-point negative one.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_NEGATIVE_ONE: Dynamic =
        Self(Union::Float(FloatWrapper::new(-1.0), AccessMode::ReadWrite));

    /// Get the [`AccessMode`] for this [`Dynamic`].
    pub(crate) fn access_mode(&self) -> AccessMode {
        match self.0 {
            Union::Unit(_, access)
            | Union::Bool(_, access)
            | Union::Str(_, access)
            | Union::Char(_, access)
            | Union::Int(_, access)
            | Union::FnPtr(_, access)
            | Union::Variant(_, access) => access,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, access) => access,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, access) => access,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, access) => access,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, access) => access,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, access) => access,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, access) => access,
        }
    }
    /// Set the [`AccessMode`] for this [`Dynamic`].
    pub(crate) fn set_access_mode(&mut self, typ: AccessMode) {
        match &mut self.0 {
            Union::Unit(_, access)
            | Union::Bool(_, access)
            | Union::Str(_, access)
            | Union::Char(_, access)
            | Union::Int(_, access)
            | Union::FnPtr(_, access)
            | Union::Variant(_, access) => *access = typ,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, access) => *access = typ,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, access) => *access = typ,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, access) => *access = typ,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, access) => *access = typ,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, access) => *access = typ,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, access) => *access = typ,
        }
    }
    /// Is this [`Dynamic`] read-only?
    ///
    /// Constant [`Dynamic`] values are read-only. If a [`&mut Dynamic`][Dynamic] to such a constant
    /// is passed to a Rust function, the function can use this information to return an error of
    /// [`ErrorAssignmentToConstant`][crate::EvalAltResult::ErrorAssignmentToConstant]
    /// if its value is going to be modified. This safe-guards constant values from being modified
    /// from within Rust functions.
    pub fn is_read_only(&self) -> bool {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, access) if access.is_read_only() => true,

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(ref cell, _) => cell.borrow().access_mode().is_read_only(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(ref cell, _) => cell.read().unwrap().access_mode().is_read_only(),

            _ => self.access_mode().is_read_only(),
        }
    }
    /// Create a [`Dynamic`] from any type.  A [`Dynamic`] value is simply returned as is.
    ///
    /// # Safety
    ///
    /// This type uses some unsafe code, mainly for type casting.
    ///
    /// # Notes
    ///
    /// Beware that you need to pass in an [`Array`] type for it to be recognized as an [`Array`].
    /// A [`Vec<T>`][Vec] does not get automatically converted to an [`Array`], but will be a generic
    /// restricted trait object instead, because [`Vec<T>`][Vec] is not a supported standard type.
    ///
    /// Similarly, passing in a [`HashMap<String, T>`][std::collections::HashMap] will not get a [`Map`]
    /// but a trait object.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let result = Dynamic::from(42_i64);
    /// assert_eq!(result.type_name(), "i64");
    /// assert_eq!(result.to_string(), "42");
    ///
    /// let result = Dynamic::from("hello");
    /// assert_eq!(result.type_name(), "string");
    /// assert_eq!(result.to_string(), "hello");
    ///
    /// let new_result = Dynamic::from(result);
    /// assert_eq!(new_result.type_name(), "string");
    /// assert_eq!(new_result.to_string(), "hello");
    /// ```
    #[inline(always)]
    pub fn from<T: Variant + Clone>(value: T) -> Self {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return <dyn Any>::downcast_ref::<INT>(&value)
                .unwrap()
                .clone()
                .into();
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return <dyn Any>::downcast_ref::<FLOAT>(&value)
                .unwrap()
                .clone()
                .into();
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return <dyn Any>::downcast_ref::<Decimal>(&value)
                .unwrap()
                .clone()
                .into();
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return <dyn Any>::downcast_ref::<bool>(&value)
                .unwrap()
                .clone()
                .into();
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return <dyn Any>::downcast_ref::<char>(&value)
                .unwrap()
                .clone()
                .into();
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return <dyn Any>::downcast_ref::<ImmutableString>(&value)
                .unwrap()
                .clone()
                .into();
        }
        if TypeId::of::<T>() == TypeId::of::<&str>() {
            return <dyn Any>::downcast_ref::<&str>(&value)
                .unwrap()
                .deref()
                .into();
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return ().into();
        }

        let mut boxed = Box::new(value);

        boxed = match unsafe_cast_box::<_, Dynamic>(boxed) {
            Ok(d) => return *d,
            Err(val) => val,
        };
        boxed = match unsafe_cast_box::<_, String>(boxed) {
            Ok(s) => return (*s).into(),
            Err(val) => val,
        };
        #[cfg(not(feature = "no_index"))]
        {
            boxed = match unsafe_cast_box::<_, Array>(boxed) {
                Ok(array) => return (*array).into(),
                Err(val) => val,
            };
        }

        #[cfg(not(feature = "no_object"))]
        {
            boxed = match unsafe_cast_box::<_, Map>(boxed) {
                Ok(map) => return (*map).into(),
                Err(val) => val,
            }
        }

        boxed = match unsafe_cast_box::<_, FnPtr>(boxed) {
            Ok(fn_ptr) => return (*fn_ptr).into(),
            Err(val) => val,
        };

        #[cfg(not(feature = "no_std"))]
        {
            boxed = match unsafe_cast_box::<_, Instant>(boxed) {
                Ok(timestamp) => return (*timestamp).into(),
                Err(val) => val,
            }
        }

        Self(Union::Variant(Box::new(boxed), AccessMode::ReadWrite))
    }
    /// Turn the [`Dynamic`] value into a shared [`Dynamic`] value backed by an
    /// [`Rc`][std::rc::Rc]`<`[`RefCell`][std::cell::RefCell]`<`[`Dynamic`]`>>` or
    /// [`Arc`][std::sync::Arc]`<`[`RwLock`][std::sync::RwLock]`<`[`Dynamic`]`>>`
    /// depending on the `sync` feature.
    ///
    /// Not available under `no_closure`.
    ///
    /// Shared [`Dynamic`] values are relatively cheap to clone as they simply increment the
    /// reference counts.
    ///
    /// Shared [`Dynamic`] values can be converted seamlessly to and from ordinary [`Dynamic`]
    /// values.
    ///
    /// If the [`Dynamic`] value is already shared, this method returns itself.
    ///
    /// # Panics
    ///
    /// Panics under the `no_closure` feature.
    #[cfg(not(feature = "no_closure"))]
    #[inline(always)]
    pub fn into_shared(self) -> Self {
        let _access = self.access_mode();

        return match self.0 {
            Union::Shared(_, _) => self,
            _ => Self(Union::Shared(crate::Locked::new(self).into(), _access)),
        };

        #[cfg(feature = "no_closure")]
        panic!("converting into a shared value is not supported under 'no_closure'");
    }
    /// Convert the [`Dynamic`] value into specific type.
    ///
    /// Casting to a [`Dynamic`] just returns as is, but if it contains a shared value,
    /// it is cloned into a [`Dynamic`] with a normal value.
    ///
    /// Returns [`None`] if types mismatched.
    ///
    /// # Panics or Deadlocks
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    ///
    /// These normally shouldn't occur since most operations in Rhai is single-threaded.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let x = Dynamic::from(42_u32);
    ///
    /// assert_eq!(x.try_cast::<u32>().unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn try_cast<T: Variant>(self) -> Option<T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _) => return cell.borrow().clone().try_cast(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _) => return cell.read().unwrap().clone().try_cast(),

            _ => (),
        }

        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return unsafe_cast_box::<_, T>(Box::new(self)).ok().map(|v| *v);
        }

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match self.0 {
                Union::Int(value, _) => unsafe_try_cast(value),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return match self.0 {
                Union::Float(value, _) => unsafe_try_cast(*value),
                _ => None,
            };
        }

        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return match self.0 {
                Union::Decimal(value, _) => unsafe_try_cast(*value),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match self.0 {
                Union::Bool(value, _) => unsafe_try_cast(value),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match self.0 {
                Union::Str(value, _) => unsafe_try_cast(value),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<String>() {
            return match self.0 {
                Union::Str(value, _) => unsafe_try_cast(value.into_owned()),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match self.0 {
                Union::Char(value, _) => unsafe_try_cast(value),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            return match self.0 {
                Union::Array(value, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            return match self.0 {
                Union::Map(value, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match self.0 {
                Union::FnPtr(value, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match self.0 {
                Union::TimeStamp(value, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match self.0 {
                Union::Unit(value, _) => unsafe_try_cast(value),
                _ => None,
            };
        }

        match self.0 {
            Union::Variant(value, _) => (*value).as_box_any().downcast().map(|x| *x).ok(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => unreachable!("Union::Shared case should be already handled"),
            _ => None,
        }
    }
    /// Convert the [`Dynamic`] value into a specific type.
    ///
    /// Casting to a [`Dynamic`] just returns as is, but if it contains a shared value,
    /// it is cloned into a [`Dynamic`] with a normal value.
    ///
    /// Returns [`None`] if types mismatched.
    ///
    /// # Panics or Deadlocks
    ///
    /// Panics if the cast fails (e.g. the type of the actual value is not the
    /// same as the specified type).
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    ///
    /// These normally shouldn't occur since most operations in Rhai is single-threaded.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Dynamic;
    ///
    /// let x = Dynamic::from(42_u32);
    ///
    /// assert_eq!(x.cast::<u32>(), 42);
    /// ```
    #[inline(always)]
    pub fn cast<T: Variant + Clone>(self) -> T {
        let self_type_name = if self.is_shared() {
            // Avoid panics/deadlocks with shared values
            "<shared>"
        } else {
            self.type_name()
        };

        self.try_cast::<T>().unwrap_or_else(|| {
            panic!(
                "cannot cast {} value and to {}",
                self_type_name,
                type_name::<T>()
            )
        })
    }
    /// Flatten the [`Dynamic`] and clone it.
    ///
    /// If the [`Dynamic`] is not a shared value, it returns a cloned copy.
    ///
    /// If the [`Dynamic`] is a shared value, it a cloned copy of the shared value.
    #[inline(always)]
    pub fn flatten_clone(&self) -> Self {
        match &self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _) => {
                #[cfg(not(feature = "sync"))]
                return cell.borrow().clone();

                #[cfg(feature = "sync")]
                return cell.read().unwrap().clone();
            }
            _ => self.clone(),
        }
    }
    /// Flatten the [`Dynamic`].
    ///
    /// If the [`Dynamic`] is not a shared value, it returns itself.
    ///
    /// If the [`Dynamic`] is a shared value, it returns the shared value if there are no
    /// outstanding references, or a cloned copy.
    #[inline(always)]
    pub fn flatten(self) -> Self {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _) => crate::fn_native::shared_try_take(cell).map_or_else(
                |cell| {
                    #[cfg(not(feature = "sync"))]
                    return cell.borrow().clone();
                    #[cfg(feature = "sync")]
                    return cell.read().unwrap().clone();
                },
                |value| {
                    #[cfg(not(feature = "sync"))]
                    return value.into_inner();
                    #[cfg(feature = "sync")]
                    return value.into_inner().unwrap();
                },
            ),
            _ => self,
        }
    }
    /// Is the [`Dynamic`] a shared value that is locked?
    ///
    /// ## Note
    ///
    /// Under the `sync` feature, shared values use [`RwLock`][std::sync::RwLock] and they are never locked.
    /// Access just waits until the [`RwLock`][std::sync::RwLock] is released.
    /// So this method always returns [`false`] under [`Sync`].
    #[inline(always)]
    pub fn is_locked(&self) -> bool {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref _cell, _) => {
                #[cfg(not(feature = "sync"))]
                return _cell.try_borrow().is_err();

                #[cfg(feature = "sync")]
                return false;
            }
            _ => false,
        }
    }
    /// Get a reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a reference to it.
    ///
    /// Returns [`None`] if the cast fails.
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    #[inline(always)]
    pub fn read_lock<T: Variant + Clone>(&self) -> Option<DynamicReadLock<T>> {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, _) => {
                #[cfg(not(feature = "sync"))]
                let data = cell.borrow();
                #[cfg(feature = "sync")]
                let data = cell.read().unwrap();

                let type_id = (*data).type_id();

                if type_id != TypeId::of::<T>() && TypeId::of::<Dynamic>() != TypeId::of::<T>() {
                    None
                } else {
                    Some(DynamicReadLock(DynamicReadLockInner::Guard(data)))
                }
            }
            _ => self
                .downcast_ref()
                .map(|r| DynamicReadLock(DynamicReadLockInner::Reference(r))),
        }
    }
    /// Get a mutable reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a mutable reference to it.
    ///
    /// Returns [`None`] if the cast fails.
    ///
    /// # Panics or Deadlocks When Value is Shared
    ///
    /// Under the `sync` feature, this call may deadlock, or [panic](https://doc.rust-lang.org/std/sync/struct.RwLock.html#panics-1).
    /// Otherwise, this call panics if the data is currently borrowed for write.
    #[inline(always)]
    pub fn write_lock<T: Variant + Clone>(&mut self) -> Option<DynamicWriteLock<T>> {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, _) => {
                #[cfg(not(feature = "sync"))]
                let data = cell.borrow_mut();
                #[cfg(feature = "sync")]
                let data = cell.write().unwrap();

                let type_id = (*data).type_id();

                if type_id != TypeId::of::<T>() && TypeId::of::<Dynamic>() != TypeId::of::<T>() {
                    None
                } else {
                    Some(DynamicWriteLock(DynamicWriteLockInner::Guard(data)))
                }
            }
            _ => self
                .downcast_mut()
                .map(|r| DynamicWriteLock(DynamicWriteLockInner::Reference(r))),
        }
    }
    /// Get a reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a reference to it.
    ///
    /// Returns [`None`] if the cast fails, or if the value is shared.
    #[inline(always)]
    pub(crate) fn downcast_ref<T: Variant + Clone>(&self) -> Option<&T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match &self.0 {
                Union::Int(value, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return match &self.0 {
                Union::Float(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return match &self.0 {
                Union::Decimal(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match &self.0 {
                Union::Bool(value, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match &self.0 {
                Union::Str(value, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<String>() {
            return match &self.0 {
                Union::Str(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match &self.0 {
                Union::Char(value, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            return match &self.0 {
                Union::Array(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            return match &self.0 {
                Union::Map(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match &self.0 {
                Union::FnPtr(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match &self.0 {
                Union::TimeStamp(value, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match &self.0 {
                Union::Unit(value, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return <dyn Any>::downcast_ref::<T>(self);
        }

        match &self.0 {
            Union::Variant(value, _) => value.as_ref().as_ref().as_any().downcast_ref::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => None,
            _ => None,
        }
    }
    /// Get a mutable reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a mutable reference to it.
    ///
    /// Returns [`None`] if the cast fails, or if the value is shared.
    #[inline(always)]
    pub(crate) fn downcast_mut<T: Variant + Clone>(&mut self) -> Option<&mut T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match &mut self.0 {
                Union::Int(value, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return match &mut self.0 {
                Union::Float(value, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return match &mut self.0 {
                Union::Decimal(value, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match &mut self.0 {
                Union::Bool(value, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match &mut self.0 {
                Union::Str(value, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match &mut self.0 {
                Union::Char(value, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            return match &mut self.0 {
                Union::Array(value, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            return match &mut self.0 {
                Union::Map(value, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match &mut self.0 {
                Union::FnPtr(value, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match &mut self.0 {
                Union::TimeStamp(value, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match &mut self.0 {
                Union::Unit(value, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return <dyn Any>::downcast_mut::<T>(self);
        }

        match &mut self.0 {
            Union::Variant(value, _) => value.as_mut().as_mut_any().downcast_mut::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => None,
            _ => None,
        }
    }
    /// Cast the [`Dynamic`] as the system integer type [`INT`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_int(&self) -> Result<INT, &'static str> {
        match self.0 {
            Union::Int(n, _) => Ok(n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as the system floating-point type [`FLOAT`] and return it.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// Not available under `no_float`.
    #[cfg(not(feature = "no_float"))]
    #[inline(always)]
    pub fn as_float(&self) -> Result<FLOAT, &'static str> {
        match self.0 {
            Union::Float(n, _) => Ok(*n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// _(DECIMAL)_ Cast the [`Dynamic`] as a [`Decimal`] and return it.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// Exported under the `decimal` feature only.
    #[cfg(feature = "decimal")]
    #[inline(always)]
    pub fn as_decimal(&self) -> Result<Decimal, &'static str> {
        match &self.0 {
            Union::Decimal(n, _) => Ok(**n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`bool`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_bool(&self) -> Result<bool, &'static str> {
        match self.0 {
            Union::Bool(b, _) => Ok(b),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`char`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_char(&self) -> Result<char, &'static str> {
        match self.0 {
            Union::Char(n, _) => Ok(n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`String`] and return the string slice.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// Fails if `self` is _shared_.
    #[inline(always)]
    pub fn as_str(&self) -> Result<&str, &'static str> {
        match &self.0 {
            Union::Str(s, _) => Ok(s),
            Union::FnPtr(f, _) => Ok(f.fn_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Convert the [`Dynamic`] into a [`String`] and return it.
    /// If there are other references to the same string, a cloned copy is returned.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn take_string(self) -> Result<String, &'static str> {
        self.take_immutable_string()
            .map(ImmutableString::into_owned)
    }
    /// Convert the [`Dynamic`] into an [`ImmutableString`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline]
    pub fn take_immutable_string(self) -> Result<ImmutableString, &'static str> {
        match self.0 {
            Union::Str(s, _) => Ok(s),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _) => {
                #[cfg(not(feature = "sync"))]
                let data = cell.borrow();
                #[cfg(feature = "sync")]
                let data = cell.read().unwrap();

                match &data.0 {
                    Union::Str(s, _) => Ok(s.clone()),
                    _ => Err((*data).type_name()),
                }
            }
            _ => Err(self.type_name()),
        }
    }
}

impl From<()> for Dynamic {
    #[inline(always)]
    fn from(value: ()) -> Self {
        Self(Union::Unit(value, AccessMode::ReadWrite))
    }
}
impl From<bool> for Dynamic {
    #[inline(always)]
    fn from(value: bool) -> Self {
        Self(Union::Bool(value, AccessMode::ReadWrite))
    }
}
impl From<INT> for Dynamic {
    #[inline(always)]
    fn from(value: INT) -> Self {
        Self(Union::Int(value, AccessMode::ReadWrite))
    }
}
#[cfg(not(feature = "no_float"))]
impl From<FLOAT> for Dynamic {
    #[inline(always)]
    fn from(value: FLOAT) -> Self {
        Self(Union::Float(value.into(), AccessMode::ReadWrite))
    }
}
#[cfg(not(feature = "no_float"))]
impl From<FloatWrapper> for Dynamic {
    #[inline(always)]
    fn from(value: FloatWrapper) -> Self {
        Self(Union::Float(value, AccessMode::ReadWrite))
    }
}
#[cfg(feature = "decimal")]
impl From<Decimal> for Dynamic {
    #[inline(always)]
    fn from(value: Decimal) -> Self {
        Self(Union::Decimal(
            Box::new(value.into()),
            AccessMode::ReadWrite,
        ))
    }
}
impl From<char> for Dynamic {
    #[inline(always)]
    fn from(value: char) -> Self {
        Self(Union::Char(value, AccessMode::ReadWrite))
    }
}
impl<S: Into<ImmutableString>> From<S> for Dynamic {
    #[inline(always)]
    fn from(value: S) -> Self {
        Self(Union::Str(value.into(), AccessMode::ReadWrite))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> From<crate::stdlib::vec::Vec<T>> for Dynamic {
    #[inline(always)]
    fn from(value: crate::stdlib::vec::Vec<T>) -> Self {
        Self(Union::Array(
            Box::new(value.into_iter().map(Dynamic::from).collect()),
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> From<&[T]> for Dynamic {
    #[inline(always)]
    fn from(value: &[T]) -> Self {
        Self(Union::Array(
            Box::new(value.iter().cloned().map(Dynamic::from).collect()),
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
impl<K: Into<ImmutableString>, T: Variant + Clone> From<crate::stdlib::collections::HashMap<K, T>>
    for Dynamic
{
    #[inline(always)]
    fn from(value: crate::stdlib::collections::HashMap<K, T>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|(k, v)| (k.into(), Dynamic::from(v)))
                    .collect(),
            ),
            AccessMode::ReadWrite,
        ))
    }
}
impl From<FnPtr> for Dynamic {
    #[inline(always)]
    fn from(value: FnPtr) -> Self {
        Self(Union::FnPtr(Box::new(value), AccessMode::ReadWrite))
    }
}
impl From<Box<FnPtr>> for Dynamic {
    #[inline(always)]
    fn from(value: Box<FnPtr>) -> Self {
        Self(Union::FnPtr(value, AccessMode::ReadWrite))
    }
}
#[cfg(not(feature = "no_std"))]
impl From<Instant> for Dynamic {
    #[inline(always)]
    fn from(value: Instant) -> Self {
        Self(Union::TimeStamp(Box::new(value), AccessMode::ReadWrite))
    }
}
