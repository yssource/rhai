use crate::any::Dynamic;
use crate::parser::FnDef;
use crate::result::EvalAltResult;

use crate::stdlib::{boxed::Box, rc::Rc, sync::Arc};

pub type FnCallArgs<'a> = [&'a mut Dynamic];

#[cfg(feature = "sync")]
pub type FnAny = dyn Fn(&mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>>;

pub type IteratorFn = fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

#[cfg(feature = "sync")]
pub type PrintCallback = dyn Fn(&str) + Send + Sync + 'static;
#[cfg(not(feature = "sync"))]
pub type PrintCallback = dyn Fn(&str) + 'static;

#[cfg(feature = "sync")]
pub type ProgressCallback = dyn Fn(u64) -> bool + Send + Sync + 'static;
#[cfg(not(feature = "sync"))]
pub type ProgressCallback = dyn Fn(u64) -> bool + 'static;

// Define callback function types
#[cfg(feature = "sync")]
pub trait ObjectGetCallback<T, U>: Fn(&mut T) -> U + Send + Sync + 'static {}
#[cfg(feature = "sync")]
impl<F: Fn(&mut T) -> U + Send + Sync + 'static, T, U> ObjectGetCallback<T, U> for F {}

#[cfg(not(feature = "sync"))]
pub trait ObjectGetCallback<T, U>: Fn(&mut T) -> U + 'static {}
#[cfg(not(feature = "sync"))]
impl<F: Fn(&mut T) -> U + 'static, T, U> ObjectGetCallback<T, U> for F {}

#[cfg(feature = "sync")]
pub trait ObjectSetCallback<T, U>: Fn(&mut T, U) + Send + Sync + 'static {}
#[cfg(feature = "sync")]
impl<F: Fn(&mut T, U) + Send + Sync + 'static, T, U> ObjectSetCallback<T, U> for F {}

#[cfg(not(feature = "sync"))]
pub trait ObjectSetCallback<T, U>: Fn(&mut T, U) + 'static {}
#[cfg(not(feature = "sync"))]
impl<F: Fn(&mut T, U) + 'static, T, U> ObjectSetCallback<T, U> for F {}

#[cfg(feature = "sync")]
pub trait ObjectIndexerCallback<T, X, U>: Fn(&mut T, X) -> U + Send + Sync + 'static {}
#[cfg(feature = "sync")]
impl<F: Fn(&mut T, X) -> U + Send + Sync + 'static, T, X, U> ObjectIndexerCallback<T, X, U> for F {}

#[cfg(not(feature = "sync"))]
pub trait ObjectIndexerCallback<T, X, U>: Fn(&mut T, X) -> U + 'static {}
#[cfg(not(feature = "sync"))]
impl<F: Fn(&mut T, X) -> U + 'static, T, X, U> ObjectIndexerCallback<T, X, U> for F {}

#[cfg(not(feature = "sync"))]
pub type SharedNativeFunction = Rc<FnAny>;
#[cfg(feature = "sync")]
pub type SharedNativeFunction = Arc<FnAny>;

#[cfg(feature = "sync")]
pub type SharedFnDef = Arc<FnDef>;
#[cfg(not(feature = "sync"))]
pub type SharedFnDef = Rc<FnDef>;

/// A type encapsulating a function callable by Rhai.
#[derive(Clone)]
pub enum CallableFunction {
    /// A pure native Rust function with all arguments passed by value.
    Pure(SharedNativeFunction),
    /// A native Rust object method with the first argument passed by reference,
    /// and the rest passed by value.
    Method(SharedNativeFunction),
    /// An iterator function.
    Iterator(IteratorFn),
    /// A script-defined function.
    Script(SharedFnDef),
}

impl CallableFunction {
    /// Is this a pure native Rust function?
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Pure(_) => true,
            Self::Method(_) | Self::Iterator(_) | Self::Script(_) => false,
        }
    }
    /// Is this a pure native Rust method-call?
    pub fn is_method(&self) -> bool {
        match self {
            Self::Method(_) => true,
            Self::Pure(_) | Self::Iterator(_) | Self::Script(_) => false,
        }
    }
    /// Is this an iterator function?
    pub fn is_iter(&self) -> bool {
        match self {
            Self::Iterator(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Script(_) => false,
        }
    }
    /// Is this a Rhai-scripted function?
    pub fn is_script(&self) -> bool {
        match self {
            Self::Script(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => false,
        }
    }
    /// Get a reference to a native Rust function.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Pure` or `Method`.
    pub fn get_native_fn(&self) -> &FnAny {
        match self {
            Self::Pure(f) | Self::Method(f) => f.as_ref(),
            Self::Iterator(_) | Self::Script(_) => panic!(),
        }
    }
    /// Get a reference to a script-defined function definition.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Script`.
    pub fn get_fn_def(&self) -> &FnDef {
        match self {
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => panic!(),
            Self::Script(f) => f,
        }
    }
    /// Get a reference to an iterator function.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Iterator`.
    pub fn get_iter_fn(&self) -> IteratorFn {
        match self {
            Self::Iterator(f) => *f,
            Self::Pure(_) | Self::Method(_) | Self::Script(_) => panic!(),
        }
    }
    /// Create a new `CallableFunction::Pure`.
    pub fn from_pure(func: Box<FnAny>) -> Self {
        Self::Pure(func.into())
    }
    /// Create a new `CallableFunction::Method`.
    pub fn from_method(func: Box<FnAny>) -> Self {
        Self::Method(func.into())
    }
}
