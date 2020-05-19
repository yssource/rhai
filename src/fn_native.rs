use crate::any::Dynamic;
use crate::parser::{FnDef, SharedFnDef};
use crate::result::EvalAltResult;

use crate::stdlib::{boxed::Box, rc::Rc, sync::Arc};

pub type FnCallArgs<'a> = [&'a mut Dynamic];

#[cfg(feature = "sync")]
pub type FnAny = dyn Fn(&mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>>;

#[cfg(feature = "sync")]
pub type IteratorFn = dyn Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type IteratorFn = dyn Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

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

#[cfg(feature = "sync")]
pub trait IteratorCallback:
    Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + Send + Sync + 'static
{
}
#[cfg(feature = "sync")]
impl<F: Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + Send + Sync + 'static> IteratorCallback
    for F
{
}

#[cfg(not(feature = "sync"))]
pub trait IteratorCallback: Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + 'static {}
#[cfg(not(feature = "sync"))]
impl<F: Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + 'static> IteratorCallback for F {}

/// A type encapsulating a function callable by Rhai.
pub enum CallableFunction {
    /// A pure native Rust function with all arguments passed by value.
    Pure(Box<FnAny>),
    /// A native Rust object method with the first argument passed by reference,
    /// and the rest passed by value.
    Method(Box<FnAny>),
    /// An iterator function.
    Iterator(Box<IteratorFn>),
    /// A script-defined function.
    Script(SharedFnDef),
}

impl CallableFunction {
    /// Is this a pure native Rust function?
    pub fn is_pure(&self) -> bool {
        match self {
            CallableFunction::Pure(_) => true,
            CallableFunction::Method(_)
            | CallableFunction::Iterator(_)
            | CallableFunction::Script(_) => false,
        }
    }
    /// Is this a pure native Rust method-call?
    pub fn is_method(&self) -> bool {
        match self {
            CallableFunction::Method(_) => true,
            CallableFunction::Pure(_)
            | CallableFunction::Iterator(_)
            | CallableFunction::Script(_) => false,
        }
    }
    /// Is this an iterator function?
    pub fn is_iter(&self) -> bool {
        match self {
            CallableFunction::Iterator(_) => true,
            CallableFunction::Pure(_)
            | CallableFunction::Method(_)
            | CallableFunction::Script(_) => false,
        }
    }
    /// Is this a Rhai-scripted function?
    pub fn is_script(&self) -> bool {
        match self {
            CallableFunction::Script(_) => true,
            CallableFunction::Pure(_)
            | CallableFunction::Method(_)
            | CallableFunction::Iterator(_) => false,
        }
    }
    /// Get a reference to a native Rust function.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Pure` or `Method`.
    pub fn get_native_fn(&self) -> &Box<FnAny> {
        match self {
            CallableFunction::Pure(f) | CallableFunction::Method(f) => f,
            CallableFunction::Iterator(_) | CallableFunction::Script(_) => panic!(),
        }
    }
    /// Get a reference to a script-defined function definition.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Script`.
    pub fn get_fn_def(&self) -> &FnDef {
        match self {
            CallableFunction::Pure(_)
            | CallableFunction::Method(_)
            | CallableFunction::Iterator(_) => panic!(),
            CallableFunction::Script(f) => f,
        }
    }
    /// Get a reference to an iterator function.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Iterator`.
    pub fn get_iter_fn(&self) -> &Box<IteratorFn> {
        match self {
            CallableFunction::Pure(_)
            | CallableFunction::Method(_)
            | CallableFunction::Script(_) => panic!(),
            CallableFunction::Iterator(f) => f,
        }
    }
}

/// A callable function.
#[cfg(not(feature = "sync"))]
pub type SharedFunction = Rc<CallableFunction>;
/// A callable function.
#[cfg(feature = "sync")]
pub type SharedFunction = Arc<CallableFunction>;
