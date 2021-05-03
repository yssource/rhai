//! Helper module which defines the [`Any`] trait to to allow dynamic value handling.

use crate::fn_native::SendSync;
use crate::r#unsafe::{unsafe_cast_box, unsafe_try_cast};
use crate::{FnPtr, ImmutableString, SmartString, INT};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::{type_name, Any, TypeId},
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
};

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
use std::time::Instant;

use fmt::Debug;
#[cfg(not(feature = "no_std"))]
#[cfg(any(target_arch = "wasm32", target_arch = "wasm64"))]
use instant::Instant;

mod private {
    use crate::fn_native::SendSync;
    use std::any::Any;

    /// A sealed trait that prevents other crates from implementing [`Variant`].
    pub trait Sealed {}

    impl<T: Any + Clone + SendSync> Sealed for T {}
}

/// _(INTERNALS)_ Trait to represent any type.
/// Exported under the `internals` feature only.
///
/// This trait is sealed and cannot be implemented.
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

/// _(INTERNALS)_ Trait to represent any type.
/// Exported under the `internals` feature only.
///
/// This trait is sealed and cannot be implemented.
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

/// Arbitrary data attached to a [`Dynamic`] value.
pub type Tag = i16;

/// Default tag value for [`Dynamic`].
const DEFAULT_TAG: Tag = 0;

/// Dynamic type containing any value.
pub struct Dynamic(pub(crate) Union);

/// Internal [`Dynamic`] representation.
///
/// Most variants are boxed to reduce the size.
pub enum Union {
    /// The Unit value - ().
    Unit((), Tag, AccessMode),
    /// A boolean value.
    Bool(bool, Tag, AccessMode),
    /// An [`ImmutableString`] value.
    Str(ImmutableString, Tag, AccessMode),
    /// A character value.
    Char(char, Tag, AccessMode),
    /// An integer value.
    Int(INT, Tag, AccessMode),
    /// A floating-point value.
    #[cfg(not(feature = "no_float"))]
    Float(FloatWrapper<FLOAT>, Tag, AccessMode),
    /// A fixed-precision decimal value.
    #[cfg(feature = "decimal")]
    Decimal(Box<Decimal>, Tag, AccessMode),
    /// An array value.
    #[cfg(not(feature = "no_index"))]
    Array(Box<Array>, Tag, AccessMode),
    /// An object map value.
    #[cfg(not(feature = "no_object"))]
    Map(Box<Map>, Tag, AccessMode),
    /// A function pointer.
    FnPtr(Box<FnPtr>, Tag, AccessMode),
    /// A timestamp value.
    #[cfg(not(feature = "no_std"))]
    TimeStamp(Box<Instant>, Tag, AccessMode),

    /// Any type as a trait object.
    Variant(Box<Box<dyn Variant>>, Tag, AccessMode),

    /// A _shared_ value of any type.
    #[cfg(not(feature = "no_closure"))]
    Shared(crate::Shared<crate::Locked<Dynamic>>, Tag, AccessMode),
}

/// Underlying [`Variant`] read guard for [`Dynamic`].
///
/// This data structure provides transparent interoperability between
/// normal [`Dynamic`] and shared [`Dynamic`] values.
#[derive(Debug)]
pub struct DynamicReadLock<'d, T: Clone>(DynamicReadLockInner<'d, T>);

/// Different types of read guards for [`DynamicReadLock`].
#[derive(Debug)]
enum DynamicReadLockInner<'d, T: Clone> {
    /// A simple reference to a non-shared value.
    Reference(&'d T),

    /// A read guard to a shared [`RefCell`][std::cell::RefCell].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(not(feature = "sync"))]
    Guard(std::cell::Ref<'d, Dynamic>),
    /// A read guard to a shared [`RwLock`][std::sync::RwLock].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(feature = "sync")]
    Guard(std::sync::RwLockReadGuard<'d, Dynamic>),
}

impl<'d, T: Any + Clone> Deref for DynamicReadLock<'d, T> {
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
pub struct DynamicWriteLock<'d, T: Clone>(DynamicWriteLockInner<'d, T>);

/// Different types of write guards for [`DynamicReadLock`].
#[derive(Debug)]
enum DynamicWriteLockInner<'d, T: Clone> {
    /// A simple mutable reference to a non-shared value.
    Reference(&'d mut T),

    /// A write guard to a shared [`RefCell`][std::cell::RefCell].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(not(feature = "sync"))]
    Guard(std::cell::RefMut<'d, Dynamic>),
    /// A write guard to a shared [`RwLock`][std::sync::RwLock].
    #[cfg(not(feature = "no_closure"))]
    #[cfg(feature = "sync")]
    Guard(std::sync::RwLockWriteGuard<'d, Dynamic>),
}

impl<'d, T: Any + Clone> Deref for DynamicWriteLock<'d, T> {
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

impl<'d, T: Any + Clone> DerefMut for DynamicWriteLock<'d, T> {
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
    /// Get the arbitrary data attached to this [`Dynamic`].
    pub fn tag(&self) -> Tag {
        match self.0 {
            Union::Unit(_, tag, _)
            | Union::Bool(_, tag, _)
            | Union::Str(_, tag, _)
            | Union::Char(_, tag, _)
            | Union::Int(_, tag, _)
            | Union::FnPtr(_, tag, _)
            | Union::Variant(_, tag, _) => tag,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, tag, _) => tag,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, tag, _) => tag,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, tag, _) => tag,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, tag, _) => tag,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, tag, _) => tag,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, tag, _) => tag,
        }
    }
    /// Attach arbitrary data to this [`Dynamic`].
    pub fn set_tag(&mut self, value: Tag) {
        match &mut self.0 {
            Union::Unit(_, tag, _)
            | Union::Bool(_, tag, _)
            | Union::Str(_, tag, _)
            | Union::Char(_, tag, _)
            | Union::Int(_, tag, _)
            | Union::FnPtr(_, tag, _)
            | Union::Variant(_, tag, _) => *tag = value,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, tag, _) => *tag = value,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, tag, _) => *tag = value,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, tag, _) => *tag = value,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, tag, _) => *tag = value,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, tag, _) => *tag = value,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, tag, _) => *tag = value,
        }
    }
    /// Does this [`Dynamic`] hold a variant data type
    /// instead of one of the supported system primitive types?
    #[inline(always)]
    pub fn is_variant(&self) -> bool {
        match self.0 {
            Union::Variant(_, _, _) => true,
            _ => false,
        }
    }
    /// Is the value held by this [`Dynamic`] shared?
    ///
    /// Not available under `no_closure`.
    #[cfg(not(feature = "no_closure"))]
    #[inline(always)]
    pub fn is_shared(&self) -> bool {
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(_, _, _) => return true,
            _ => (),
        }

        false
    }
    /// Is the value held by this [`Dynamic`] a particular type?
    ///
    /// If the [`Dynamic`] is a shared variant checking is performed on
    /// top of its internal value.
    #[inline(always)]
    pub fn is<T: Any + Clone>(&self) -> bool {
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
            Union::Unit(_, _, _) => TypeId::of::<()>(),
            Union::Bool(_, _, _) => TypeId::of::<bool>(),
            Union::Str(_, _, _) => TypeId::of::<ImmutableString>(),
            Union::Char(_, _, _) => TypeId::of::<char>(),
            Union::Int(_, _, _) => TypeId::of::<INT>(),
            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _, _) => TypeId::of::<FLOAT>(),
            #[cfg(feature = "decimal")]
            Union::Decimal(_, _, _) => TypeId::of::<Decimal>(),
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _, _) => TypeId::of::<Array>(),
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _, _) => TypeId::of::<Map>(),
            Union::FnPtr(_, _, _) => TypeId::of::<FnPtr>(),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _, _) => TypeId::of::<Instant>(),

            Union::Variant(value, _, _) => (***value).type_id(),

            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                (*value).type_id()
            }
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
            Union::Unit(_, _, _) => "()",
            Union::Bool(_, _, _) => "bool",
            Union::Str(_, _, _) => "string",
            Union::Char(_, _, _) => "char",
            Union::Int(_, _, _) => type_name::<INT>(),
            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _, _) => type_name::<FLOAT>(),
            #[cfg(feature = "decimal")]
            Union::Decimal(_, _, _) => "decimal",
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _, _) => "array",
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _, _) => "map",
            Union::FnPtr(_, _, _) => "Fn",
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _, _) => "timestamp",

            Union::Variant(value, _, _) => (***value).type_name(),

            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "sync"))]
            Union::Shared(cell, _, _) => cell
                .try_borrow()
                .map(|v| (*v).type_name())
                .unwrap_or("<shared>"),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _, _) => (*cell.read().unwrap()).type_name(),
        }
    }
}

impl Hash for Dynamic {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Union::Unit(_, _, _) => ().hash(state),
            Union::Bool(value, _, _) => value.hash(state),
            Union::Str(s, _, _) => s.hash(state),
            Union::Char(ch, _, _) => ch.hash(state),
            Union::Int(i, _, _) => i.hash(state),
            #[cfg(not(feature = "no_float"))]
            Union::Float(f, _, _) => f.hash(state),
            #[cfg(not(feature = "no_index"))]
            Union::Array(a, _, _) => (**a).hash(state),
            #[cfg(not(feature = "no_object"))]
            Union::Map(m, _, _) => {
                let mut buf: crate::StaticVec<_> = m.iter().collect();
                buf.sort_by(|(a, _), (b, _)| a.cmp(b));

                buf.into_iter().for_each(|(key, value)| {
                    key.hash(state);
                    value.hash(state);
                })
            }
            Union::FnPtr(f, _, _) if f.is_curried() => {
                unimplemented!(
                    "{} with curried arguments cannot be hashed",
                    self.type_name()
                )
            }
            Union::FnPtr(f, _, _) => f.fn_name().hash(state),

            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                (*value).hash(state)
            }

            _ => unimplemented!("{} cannot be hashed", self.type_name()),
        }
    }
}

/// Map the name of a standard type into a friendly form.
#[inline(always)]
pub(crate) fn map_std_type_name(name: &str) -> &str {
    if name == type_name::<String>() {
        return "string";
    }
    if name == type_name::<ImmutableString>() {
        return "string";
    }
    if name == type_name::<&str>() {
        return "string";
    }
    if name == type_name::<FnPtr>() {
        return "Fn";
    }
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

impl fmt::Display for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Union::Unit(_, _, _) => write!(f, ""),
            Union::Bool(value, _, _) => fmt::Display::fmt(value, f),
            Union::Str(value, _, _) => fmt::Display::fmt(value, f),
            Union::Char(value, _, _) => fmt::Display::fmt(value, f),
            Union::Int(value, _, _) => fmt::Display::fmt(value, f),
            #[cfg(not(feature = "no_float"))]
            Union::Float(value, _, _) => fmt::Display::fmt(value, f),
            #[cfg(feature = "decimal")]
            Union::Decimal(value, _, _) => fmt::Display::fmt(value, f),
            #[cfg(not(feature = "no_index"))]
            Union::Array(value, _, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_object"))]
            Union::Map(value, _, _) => {
                f.write_str("#")?;
                fmt::Debug::fmt(value, f)
            }
            Union::FnPtr(value, _, _) => fmt::Display::fmt(value, f),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _, _) => f.write_str("<timestamp>"),

            Union::Variant(value, _, _) => {
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
            Union::Shared(cell, _, _) => {
                if let Ok(v) = cell.try_borrow() {
                    fmt::Display::fmt(&*v, f)
                } else {
                    f.write_str("<shared>")
                }
            }
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _, _) => fmt::Display::fmt(&*cell.read().unwrap(), f),
        }
    }
}

impl fmt::Debug for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Union::Unit(value, _, _) => fmt::Debug::fmt(value, f),
            Union::Bool(value, _, _) => fmt::Debug::fmt(value, f),
            Union::Str(value, _, _) => fmt::Debug::fmt(value, f),
            Union::Char(value, _, _) => fmt::Debug::fmt(value, f),
            Union::Int(value, _, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_float"))]
            Union::Float(value, _, _) => fmt::Debug::fmt(value, f),
            #[cfg(feature = "decimal")]
            Union::Decimal(value, _, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_index"))]
            Union::Array(value, _, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_object"))]
            Union::Map(value, _, _) => {
                f.write_str("#")?;
                fmt::Debug::fmt(value, f)
            }
            Union::FnPtr(value, _, _) => fmt::Debug::fmt(value, f),
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _, _) => write!(f, "<timestamp>"),

            Union::Variant(value, _, _) => {
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
            Union::Shared(cell, _, _) => {
                if let Ok(v) = cell.try_borrow() {
                    write!(f, "{:?} (shared)", *v)
                } else {
                    f.write_str("<shared>")
                }
            }
            #[cfg(not(feature = "no_closure"))]
            #[cfg(feature = "sync")]
            Union::Shared(cell, _, _) => fmt::Debug::fmt(&*cell.read().unwrap(), f),
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
            Union::Unit(value, tag, _) => Self(Union::Unit(value, tag, AccessMode::ReadWrite)),
            Union::Bool(value, tag, _) => Self(Union::Bool(value, tag, AccessMode::ReadWrite)),
            Union::Str(ref value, tag, _) => {
                Self(Union::Str(value.clone(), tag, AccessMode::ReadWrite))
            }
            Union::Char(value, tag, _) => Self(Union::Char(value, tag, AccessMode::ReadWrite)),
            Union::Int(value, tag, _) => Self(Union::Int(value, tag, AccessMode::ReadWrite)),
            #[cfg(not(feature = "no_float"))]
            Union::Float(value, tag, _) => Self(Union::Float(value, tag, AccessMode::ReadWrite)),
            #[cfg(feature = "decimal")]
            Union::Decimal(ref value, tag, _) => {
                Self(Union::Decimal(value.clone(), tag, AccessMode::ReadWrite))
            }
            #[cfg(not(feature = "no_index"))]
            Union::Array(ref value, tag, _) => {
                Self(Union::Array(value.clone(), tag, AccessMode::ReadWrite))
            }
            #[cfg(not(feature = "no_object"))]
            Union::Map(ref value, tag, _) => {
                Self(Union::Map(value.clone(), tag, AccessMode::ReadWrite))
            }
            Union::FnPtr(ref value, tag, _) => {
                Self(Union::FnPtr(value.clone(), tag, AccessMode::ReadWrite))
            }
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(ref value, tag, _) => {
                Self(Union::TimeStamp(value.clone(), tag, AccessMode::ReadWrite))
            }

            Union::Variant(ref value, tag, _) => {
                let mut x = (***value).clone_into_dynamic();
                x.set_tag(tag);
                x
            }

            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, tag, _) => {
                Self(Union::Shared(cell.clone(), tag, AccessMode::ReadWrite))
            }
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
    pub const UNIT: Dynamic = Self(Union::Unit((), DEFAULT_TAG, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing a `true`.
    pub const TRUE: Dynamic = Self(Union::Bool(true, DEFAULT_TAG, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing a [`false`].
    pub const FALSE: Dynamic = Self(Union::Bool(false, DEFAULT_TAG, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the integer zero.
    pub const ZERO: Dynamic = Self(Union::Int(0, DEFAULT_TAG, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the integer one.
    pub const ONE: Dynamic = Self(Union::Int(1, DEFAULT_TAG, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the integer negative one.
    pub const NEGATIVE_ONE: Dynamic = Self(Union::Int(-1, DEFAULT_TAG, AccessMode::ReadWrite));
    /// A [`Dynamic`] containing the floating-point zero.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_ZERO: Dynamic = Self(Union::Float(
        FloatWrapper::const_new(0.0),
        DEFAULT_TAG,
        AccessMode::ReadWrite,
    ));
    /// A [`Dynamic`] containing the floating-point one.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_ONE: Dynamic = Self(Union::Float(
        FloatWrapper::const_new(1.0),
        DEFAULT_TAG,
        AccessMode::ReadWrite,
    ));
    /// A [`Dynamic`] containing the floating-point negative one.
    #[cfg(not(feature = "no_float"))]
    pub const FLOAT_NEGATIVE_ONE: Dynamic = Self(Union::Float(
        FloatWrapper::const_new(-1.0),
        DEFAULT_TAG,
        AccessMode::ReadWrite,
    ));

    /// Get the [`AccessMode`] for this [`Dynamic`].
    pub(crate) fn access_mode(&self) -> AccessMode {
        match self.0 {
            Union::Unit(_, _, access)
            | Union::Bool(_, _, access)
            | Union::Str(_, _, access)
            | Union::Char(_, _, access)
            | Union::Int(_, _, access)
            | Union::FnPtr(_, _, access)
            | Union::Variant(_, _, access) => access,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _, access) => access,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, _, access) => access,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _, access) => access,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _, access) => access,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _, access) => access,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, access) => access,
        }
    }
    /// Set the [`AccessMode`] for this [`Dynamic`].
    pub(crate) fn set_access_mode(&mut self, typ: AccessMode) {
        match &mut self.0 {
            Union::Unit(_, _, access)
            | Union::Bool(_, _, access)
            | Union::Str(_, _, access)
            | Union::Char(_, _, access)
            | Union::Int(_, _, access)
            | Union::FnPtr(_, _, access)
            | Union::Variant(_, _, access) => *access = typ,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _, access) => *access = typ,
            #[cfg(feature = "decimal")]
            Union::Decimal(_, _, access) => *access = typ,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _, access) => *access = typ,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _, access) => *access = typ,
            #[cfg(not(feature = "no_std"))]
            Union::TimeStamp(_, _, access) => *access = typ,
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, access) => *access = typ,
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
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(_, _, AccessMode::ReadOnly) => return true,
            Union::Shared(ref cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                return match value.access_mode() {
                    AccessMode::ReadWrite => false,
                    AccessMode::ReadOnly => true,
                };
            }
            _ => (),
        }

        match self.access_mode() {
            AccessMode::ReadWrite => false,
            AccessMode::ReadOnly => true,
        }
    }
    /// Can this [`Dynamic`] be hashed?
    pub(crate) fn is_hashable(&self) -> bool {
        match &self.0 {
            Union::Unit(_, _, _)
            | Union::Bool(_, _, _)
            | Union::Str(_, _, _)
            | Union::Char(_, _, _)
            | Union::Int(_, _, _) => true,

            #[cfg(not(feature = "no_float"))]
            Union::Float(_, _, _) => true,
            #[cfg(not(feature = "no_index"))]
            Union::Array(_, _, _) => true,
            #[cfg(not(feature = "no_object"))]
            Union::Map(_, _, _) => true,

            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                value.is_hashable()
            }

            _ => false,
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
    /// Similarly, passing in a [`HashMap<String, T>`][std::collections::HashMap] or
    /// [`BTreeMap<String, T>`][std::collections::BTreeMap] will not get a [`Map`] but a trait object.
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
    pub fn from<T: Variant + Clone>(mut value: T) -> Self {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return unsafe_try_cast::<_, Dynamic>(value).ok().unwrap();
        }

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
        if TypeId::of::<T>() == TypeId::of::<SmartString>() {
            return <dyn Any>::downcast_ref::<SmartString>(&value)
                .unwrap()
                .clone()
                .into();
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return ().into();
        }

        value = match unsafe_try_cast::<_, String>(value) {
            Ok(s) => return (s).into(),
            Err(val) => val,
        };
        #[cfg(not(feature = "no_index"))]
        {
            value = match unsafe_try_cast::<_, Array>(value) {
                Ok(array) => return (array).into(),
                Err(val) => val,
            };
        }

        #[cfg(not(feature = "no_object"))]
        {
            value = match unsafe_try_cast::<_, Map>(value) {
                Ok(map) => return (map).into(),
                Err(val) => val,
            };
        }

        value = match unsafe_try_cast::<_, FnPtr>(value) {
            Ok(fn_ptr) => return (fn_ptr).into(),
            Err(val) => val,
        };

        #[cfg(not(feature = "no_std"))]
        {
            value = match unsafe_try_cast::<_, Instant>(value) {
                Ok(timestamp) => return (timestamp).into(),
                Err(val) => val,
            };
        }

        Self(Union::Variant(
            Box::new(Box::new(value)),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
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
    #[cfg(not(feature = "no_closure"))]
    #[inline(always)]
    pub fn into_shared(self) -> Self {
        let _access = self.access_mode();

        match self.0 {
            Union::Shared(_, _, _) => self,
            _ => Self(Union::Shared(
                crate::Locked::new(self).into(),
                DEFAULT_TAG,
                _access,
            )),
        }
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
    pub fn try_cast<T: Any>(self) -> Option<T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        #[cfg(not(feature = "no_closure"))]
        if let Union::Shared(_, _, _) = self.0 {
            return self.flatten().try_cast::<T>();
        }

        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return unsafe_try_cast::<_, T>(self).ok();
        }

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match self.0 {
                Union::Int(value, _, _) => unsafe_try_cast(value).ok(),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return match self.0 {
                Union::Float(value, _, _) => unsafe_try_cast(*value).ok(),
                _ => None,
            };
        }

        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return match self.0 {
                Union::Decimal(value, _, _) => unsafe_try_cast(*value).ok(),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match self.0 {
                Union::Bool(value, _, _) => unsafe_try_cast(value).ok(),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match self.0 {
                Union::Str(value, _, _) => unsafe_try_cast(value).ok(),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<String>() {
            return match self.0 {
                Union::Str(value, _, _) => unsafe_try_cast(value.into_owned()).ok(),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match self.0 {
                Union::Char(value, _, _) => unsafe_try_cast(value).ok(),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            return match self.0 {
                Union::Array(value, _, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            return match self.0 {
                Union::Map(value, _, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match self.0 {
                Union::FnPtr(value, _, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match self.0 {
                Union::TimeStamp(value, _, _) => unsafe_cast_box::<_, T>(value).ok().map(|v| *v),
                _ => None,
            };
        }

        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match self.0 {
                Union::Unit(value, _, _) => unsafe_try_cast(value).ok(),
                _ => None,
            };
        }

        match self.0 {
            Union::Variant(value, _, _) => (*value).as_box_any().downcast().map(|x| *x).ok(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => unreachable!("Union::Shared case should be already handled"),
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
    pub fn cast<T: Any + Clone>(self) -> T {
        #[cfg(not(feature = "no_closure"))]
        let self_type_name = if self.is_shared() {
            // Avoid panics/deadlocks with shared values
            "<shared>"
        } else {
            self.type_name()
        };
        #[cfg(feature = "no_closure")]
        let self_type_name = self.type_name();

        self.try_cast::<T>().unwrap_or_else(|| {
            panic!(
                "cannot cast {} value and to {}",
                self_type_name,
                type_name::<T>()
            )
        })
    }
    /// Clone the [`Dynamic`] value and convert it into a specific type.
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
    /// let y = &x;
    ///
    /// assert_eq!(y.clone_cast::<u32>(), 42);
    /// ```
    #[inline(always)]
    pub fn clone_cast<T: Any + Clone>(&self) -> T {
        self.flatten_clone().cast::<T>()
    }
    /// Flatten the [`Dynamic`] and clone it.
    ///
    /// If the [`Dynamic`] is not a shared value, it returns a cloned copy.
    ///
    /// If the [`Dynamic`] is a shared value, it returns a cloned copy of the shared value.
    #[inline(always)]
    pub fn flatten_clone(&self) -> Self {
        #[cfg(not(feature = "no_closure"))]
        match &self.0 {
            Union::Shared(cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                return value.clone();
            }
            _ => (),
        }

        self.clone()
    }
    /// Flatten the [`Dynamic`].
    ///
    /// If the [`Dynamic`] is not a shared value, it returns itself.
    ///
    /// If the [`Dynamic`] is a shared value, it returns the shared value if there are no
    /// outstanding references, or a cloned copy.
    #[inline(always)]
    pub fn flatten(self) -> Self {
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(cell, _, _) => {
                return crate::fn_native::shared_try_take(cell).map_or_else(
                    |cell| {
                        #[cfg(not(feature = "sync"))]
                        let value = cell.borrow();
                        #[cfg(feature = "sync")]
                        let value = cell.read().unwrap();

                        value.clone()
                    },
                    |value| {
                        #[cfg(not(feature = "sync"))]
                        return value.into_inner();
                        #[cfg(feature = "sync")]
                        return value.into_inner().unwrap();
                    },
                )
            }
            _ => (),
        }

        self
    }
    /// Flatten the [`Dynamic`] in place.
    ///
    /// If the [`Dynamic`] is not a shared value, it does nothing.
    ///
    /// If the [`Dynamic`] is a shared value, it is set to the shared value if there are no
    /// outstanding references, or a cloned copy otherwise.
    #[inline(always)]
    pub(crate) fn flatten_in_place(&mut self) {
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(_, _, _) => match std::mem::take(self).0 {
                Union::Shared(cell, _, _) => {
                    *self = crate::fn_native::shared_try_take(cell).map_or_else(
                        |cell| {
                            #[cfg(not(feature = "sync"))]
                            let value = cell.borrow();
                            #[cfg(feature = "sync")]
                            let value = cell.read().unwrap();

                            value.clone()
                        },
                        |value| {
                            #[cfg(not(feature = "sync"))]
                            return value.into_inner();
                            #[cfg(feature = "sync")]
                            return value.into_inner().unwrap();
                        },
                    )
                }
                _ => unreachable!(),
            },
            _ => (),
        }
    }
    /// Is the [`Dynamic`] a shared value that is locked?
    ///
    /// Not available under `no_closure`.
    ///
    /// ## Note
    ///
    /// Under the `sync` feature, shared values use [`RwLock`][std::sync::RwLock] and they are never locked.
    /// Access just waits until the [`RwLock`][std::sync::RwLock] is released.
    /// So this method always returns [`false`] under [`Sync`].
    #[cfg(not(feature = "no_closure"))]
    #[inline(always)]
    pub fn is_locked(&self) -> bool {
        #[cfg(not(feature = "no_closure"))]
        match self.0 {
            Union::Shared(ref _cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                return _cell.try_borrow().is_err();

                #[cfg(feature = "sync")]
                return false;
            }
            _ => (),
        }

        false
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
    pub fn read_lock<T: Any + Clone>(&self) -> Option<DynamicReadLock<T>> {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                let type_id = (*value).type_id();

                if type_id != TypeId::of::<T>() && TypeId::of::<Dynamic>() != TypeId::of::<T>() {
                    return None;
                } else {
                    return Some(DynamicReadLock(DynamicReadLockInner::Guard(value)));
                }
            }
            _ => (),
        }

        self.downcast_ref()
            .map(|r| DynamicReadLock(DynamicReadLockInner::Reference(r)))
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
    pub fn write_lock<T: Any + Clone>(&mut self) -> Option<DynamicWriteLock<T>> {
        match self.0 {
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(ref cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow_mut();
                #[cfg(feature = "sync")]
                let value = cell.write().unwrap();

                let type_id = (*value).type_id();

                if type_id != TypeId::of::<T>() && TypeId::of::<Dynamic>() != TypeId::of::<T>() {
                    return None;
                } else {
                    return Some(DynamicWriteLock(DynamicWriteLockInner::Guard(value)));
                }
            }
            _ => (),
        }

        self.downcast_mut()
            .map(|r| DynamicWriteLock(DynamicWriteLockInner::Reference(r)))
    }
    /// Get a reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a reference to it.
    ///
    /// Returns [`None`] if the cast fails, or if the value is shared.
    #[inline(always)]
    pub(crate) fn downcast_ref<T: Any + Clone + ?Sized>(&self) -> Option<&T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match &self.0 {
                Union::Int(value, _, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return match &self.0 {
                Union::Float(value, _, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return match &self.0 {
                Union::Decimal(value, _, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match &self.0 {
                Union::Bool(value, _, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match &self.0 {
                Union::Str(value, _, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match &self.0 {
                Union::Char(value, _, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            return match &self.0 {
                Union::Array(value, _, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            return match &self.0 {
                Union::Map(value, _, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match &self.0 {
                Union::FnPtr(value, _, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match &self.0 {
                Union::TimeStamp(value, _, _) => <dyn Any>::downcast_ref::<T>(value.as_ref()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match &self.0 {
                Union::Unit(value, _, _) => <dyn Any>::downcast_ref::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return <dyn Any>::downcast_ref::<T>(self);
        }

        match &self.0 {
            Union::Variant(value, _, _) => value.as_ref().as_ref().as_any().downcast_ref::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => None,
            _ => None,
        }
    }
    /// Get a mutable reference of a specific type to the [`Dynamic`].
    /// Casting to [`Dynamic`] just returns a mutable reference to it.
    ///
    /// Returns [`None`] if the cast fails, or if the value is shared.
    #[inline(always)]
    pub(crate) fn downcast_mut<T: Any + Clone>(&mut self) -> Option<&mut T> {
        // Coded this way in order to maximally leverage potentials for dead-code removal.

        if TypeId::of::<T>() == TypeId::of::<INT>() {
            return match &mut self.0 {
                Union::Int(value, _, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_float"))]
        if TypeId::of::<T>() == TypeId::of::<FLOAT>() {
            return match &mut self.0 {
                Union::Float(value, _, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        #[cfg(feature = "decimal")]
        if TypeId::of::<T>() == TypeId::of::<Decimal>() {
            return match &mut self.0 {
                Union::Decimal(value, _, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<bool>() {
            return match &mut self.0 {
                Union::Bool(value, _, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<ImmutableString>() {
            return match &mut self.0 {
                Union::Str(value, _, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<char>() {
            return match &mut self.0 {
                Union::Char(value, _, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_index"))]
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            return match &mut self.0 {
                Union::Array(value, _, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            return match &mut self.0 {
                Union::Map(value, _, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<FnPtr>() {
            return match &mut self.0 {
                Union::FnPtr(value, _, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        #[cfg(not(feature = "no_std"))]
        if TypeId::of::<T>() == TypeId::of::<Instant>() {
            return match &mut self.0 {
                Union::TimeStamp(value, _, _) => <dyn Any>::downcast_mut::<T>(value.as_mut()),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<()>() {
            return match &mut self.0 {
                Union::Unit(value, _, _) => <dyn Any>::downcast_mut::<T>(value),
                _ => None,
            };
        }
        if TypeId::of::<T>() == TypeId::of::<Dynamic>() {
            return <dyn Any>::downcast_mut::<T>(self);
        }

        match &mut self.0 {
            Union::Variant(value, _, _) => value.as_mut().as_mut_any().downcast_mut::<T>(),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => None,
            _ => None,
        }
    }
    /// Cast the [`Dynamic`] as a unit `()` and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_unit(&self) -> Result<(), &'static str> {
        match self.0 {
            Union::Unit(value, _, _) => Ok(value),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as the system integer type [`INT`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_int(&self) -> Result<INT, &'static str> {
        match self.0 {
            Union::Int(n, _, _) => Ok(n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
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
            Union::Float(n, _, _) => Ok(*n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
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
            Union::Decimal(n, _, _) => Ok(**n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`bool`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_bool(&self) -> Result<bool, &'static str> {
        match self.0 {
            Union::Bool(b, _, _) => Ok(b),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as a [`char`] and return it.
    /// Returns the name of the actual type if the cast fails.
    #[inline(always)]
    pub fn as_char(&self) -> Result<char, &'static str> {
        match self.0 {
            Union::Char(n, _, _) => Ok(n),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => self.read_lock().map(|v| *v).ok_or_else(|| self.type_name()),
            _ => Err(self.type_name()),
        }
    }
    /// Cast the [`Dynamic`] as an [`ImmutableString`] and return it.
    /// Returns the name of the actual type if the cast fails.
    ///
    /// # Panics
    ///
    /// Panics if the value is shared.
    #[inline(always)]
    pub(crate) fn as_str_ref(&self) -> Result<&str, &'static str> {
        match &self.0 {
            Union::Str(s, _, _) => Ok(s),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(_, _, _) => panic!("as_str() cannot be called on shared values"),
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
    #[inline(always)]
    pub fn take_immutable_string(self) -> Result<ImmutableString, &'static str> {
        match self.0 {
            Union::Str(s, _, _) => Ok(s),
            #[cfg(not(feature = "no_closure"))]
            Union::Shared(cell, _, _) => {
                #[cfg(not(feature = "sync"))]
                let value = cell.borrow();
                #[cfg(feature = "sync")]
                let value = cell.read().unwrap();

                match &value.0 {
                    Union::Str(s, _, _) => Ok(s.clone()),
                    _ => Err((*value).type_name()),
                }
            }
            _ => Err(self.type_name()),
        }
    }
}

impl From<()> for Dynamic {
    #[inline(always)]
    fn from(value: ()) -> Self {
        Self(Union::Unit(value, DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
impl From<bool> for Dynamic {
    #[inline(always)]
    fn from(value: bool) -> Self {
        Self(Union::Bool(value, DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
impl From<INT> for Dynamic {
    #[inline(always)]
    fn from(value: INT) -> Self {
        Self(Union::Int(value, DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
#[cfg(not(feature = "no_float"))]
impl From<FLOAT> for Dynamic {
    #[inline(always)]
    fn from(value: FLOAT) -> Self {
        Self(Union::Float(
            value.into(),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_float"))]
impl From<FloatWrapper<FLOAT>> for Dynamic {
    #[inline(always)]
    fn from(value: FloatWrapper<FLOAT>) -> Self {
        Self(Union::Float(value, DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
#[cfg(feature = "decimal")]
impl From<Decimal> for Dynamic {
    #[inline(always)]
    fn from(value: Decimal) -> Self {
        Self(Union::Decimal(
            Box::new(value.into()),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
impl From<char> for Dynamic {
    #[inline(always)]
    fn from(value: char) -> Self {
        Self(Union::Char(value, DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
impl<S: Into<ImmutableString>> From<S> for Dynamic {
    #[inline(always)]
    fn from(value: S) -> Self {
        Self(Union::Str(value.into(), DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
impl From<&ImmutableString> for Dynamic {
    #[inline(always)]
    fn from(value: &ImmutableString) -> Self {
        value.clone().into()
    }
}
#[cfg(not(feature = "no_smartstring_for_identifier"))]
impl From<&crate::Identifier> for Dynamic {
    #[inline(always)]
    fn from(value: &crate::Identifier) -> Self {
        value.to_string().into()
    }
}
#[cfg(not(feature = "no_index"))]
impl Dynamic {
    /// Create a [`Dynamc`] from an [`Array`].
    #[inline(always)]
    pub(crate) fn from_array(array: Array) -> Self {
        Self(Union::Array(
            Box::new(array),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> From<Vec<T>> for Dynamic {
    #[inline(always)]
    fn from(value: Vec<T>) -> Self {
        Self(Union::Array(
            Box::new(value.into_iter().map(Dynamic::from).collect()),
            DEFAULT_TAG,
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
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_index"))]
impl<T: Variant + Clone> std::iter::FromIterator<T> for Dynamic {
    #[inline(always)]
    fn from_iter<X: IntoIterator<Item = T>>(iter: X) -> Self {
        Self(Union::Array(
            Box::new(iter.into_iter().map(Dynamic::from).collect()),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
impl Dynamic {
    /// Create a [`Dynamc`] from a [`Map`].
    #[inline(always)]
    pub(crate) fn from_map(map: Map) -> Self {
        Self(Union::Map(
            Box::new(map),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "no_std"))]
impl<K: Into<crate::Identifier>, T: Variant + Clone> From<std::collections::HashMap<K, T>>
    for Dynamic
{
    #[inline(always)]
    fn from(value: std::collections::HashMap<K, T>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|(k, v)| (k.into(), Dynamic::from(v)))
                    .collect(),
            ),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
#[cfg(not(feature = "no_object"))]
impl<K: Into<crate::Identifier>, T: Variant + Clone> From<std::collections::BTreeMap<K, T>>
    for Dynamic
{
    #[inline(always)]
    fn from(value: std::collections::BTreeMap<K, T>) -> Self {
        Self(Union::Map(
            Box::new(
                value
                    .into_iter()
                    .map(|(k, v)| (k.into(), Dynamic::from(v)))
                    .collect(),
            ),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
impl From<FnPtr> for Dynamic {
    #[inline(always)]
    fn from(value: FnPtr) -> Self {
        Self(Union::FnPtr(
            Box::new(value),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
impl From<Box<FnPtr>> for Dynamic {
    #[inline(always)]
    fn from(value: Box<FnPtr>) -> Self {
        Self(Union::FnPtr(value, DEFAULT_TAG, AccessMode::ReadWrite))
    }
}
#[cfg(not(feature = "no_std"))]
impl From<Instant> for Dynamic {
    #[inline(always)]
    fn from(value: Instant) -> Self {
        Self(Union::TimeStamp(
            Box::new(value),
            DEFAULT_TAG,
            AccessMode::ReadWrite,
        ))
    }
}
