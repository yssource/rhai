use crate::any::Dynamic;
use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{boxed::Box, rc::Rc, sync::Arc};

pub type FnCallArgs<'a> = [&'a mut Dynamic];

#[cfg(feature = "sync")]
pub type FnAny =
    dyn Fn(&mut FnCallArgs, Position) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&mut FnCallArgs, Position) -> Result<Dynamic, Box<EvalAltResult>>;

#[cfg(feature = "sync")]
pub type IteratorFn = dyn Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type IteratorFn = dyn Fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

#[cfg(feature = "sync")]
pub type PrintCallback = dyn Fn(&str) + Send + Sync + 'static;
#[cfg(not(feature = "sync"))]
pub type PrintCallback = dyn Fn(&str) + 'static;

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

/// A type representing the type of ABI of a native Rust function.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum NativeFunctionABI {
    /// A pure function where all arguments are passed by value.
    Pure,
    /// An object method where the first argument is the object passed by mutable reference.
    /// All other arguments are passed by value.
    Method,
}

/// A trait implemented by all native Rust functions that are callable by Rhai.
#[cfg(not(feature = "sync"))]
pub trait NativeCallable {
    /// Get the ABI type of a native Rust function.
    fn abi(&self) -> NativeFunctionABI;
    /// Call a native Rust function.
    fn call(&self, args: &mut FnCallArgs, pos: Position) -> Result<Dynamic, Box<EvalAltResult>>;
}

/// A trait implemented by all native Rust functions that are callable by Rhai.
#[cfg(feature = "sync")]
pub trait NativeCallable: Send + Sync {
    /// Get the ABI type of a native Rust function.
    fn abi(&self) -> NativeFunctionABI;
    /// Call a native Rust function.
    fn call(&self, args: &mut FnCallArgs, pos: Position) -> Result<Dynamic, Box<EvalAltResult>>;
}

/// A type encapsulating a native Rust function callable by Rhai.
pub struct NativeFunction(Box<FnAny>, NativeFunctionABI);

impl NativeCallable for NativeFunction {
    fn abi(&self) -> NativeFunctionABI {
        self.1
    }
    fn call(&self, args: &mut FnCallArgs, pos: Position) -> Result<Dynamic, Box<EvalAltResult>> {
        (self.0)(args, pos)
    }
}

impl From<(Box<FnAny>, NativeFunctionABI)> for NativeFunction {
    fn from(func: (Box<FnAny>, NativeFunctionABI)) -> Self {
        Self::new(func.0, func.1)
    }
}
impl NativeFunction {
    /// Create a new `NativeFunction`.
    pub fn new(func: Box<FnAny>, abi: NativeFunctionABI) -> Self {
        Self(func, abi)
    }
}

/// An external native Rust function.
#[cfg(not(feature = "sync"))]
pub type SharedNativeFunction = Rc<Box<dyn NativeCallable>>;
/// An external native Rust function.
#[cfg(feature = "sync")]
pub type SharedNativeFunction = Arc<Box<dyn NativeCallable>>;
