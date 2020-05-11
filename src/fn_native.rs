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

/// A trait implemented by all native Rust functions that are callable by Rhai.
pub trait NativeCallable {
    /// Call a native Rust function.
    fn call(&self, args: &mut FnCallArgs, pos: Position) -> Result<Dynamic, Box<EvalAltResult>>;
}

/// A type encapsulating a native Rust function callable by Rhai.
pub struct NativeFunction(Box<FnAny>);

impl NativeCallable for NativeFunction {
    fn call(&self, args: &mut FnCallArgs, pos: Position) -> Result<Dynamic, Box<EvalAltResult>> {
        (self.0)(args, pos)
    }
}

impl From<Box<FnAny>> for NativeFunction {
    fn from(func: Box<FnAny>) -> Self {
        Self::new(func)
    }
}
impl NativeFunction {
    /// Create a new `NativeFunction`.
    pub fn new(func: Box<FnAny>) -> Self {
        Self(func)
    }
}

/// An external native Rust function.
#[cfg(not(feature = "sync"))]
pub type SharedNativeFunction = Rc<Box<dyn NativeCallable>>;
/// An external native Rust function.
#[cfg(feature = "sync")]
pub type SharedNativeFunction = Arc<Box<dyn NativeCallable>>;
