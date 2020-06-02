use crate::any::Dynamic;
use crate::parser::FnDef;
use crate::result::EvalAltResult;

use crate::stdlib::{boxed::Box, rc::Rc, sync::Arc};

#[cfg(feature = "sync")]
pub trait SendSync: Send + Sync {}
#[cfg(feature = "sync")]
impl<T: Send + Sync> SendSync for T {}

#[cfg(not(feature = "sync"))]
pub trait SendSync {}
#[cfg(not(feature = "sync"))]
impl<T> SendSync for T {}

#[cfg(not(feature = "sync"))]
pub type Shared<T> = Rc<T>;
#[cfg(feature = "sync")]
pub type Shared<T> = Arc<T>;

/// Consume a `Shared` resource and return a mutable reference to the wrapped value.
/// If the resource is shared (i.e. has other outstanding references), a cloned copy is used.
pub fn shared_make_mut<T: Clone>(value: &mut Shared<T>) -> &mut T {
    #[cfg(not(feature = "sync"))]
    {
        Rc::make_mut(value)
    }
    #[cfg(feature = "sync")]
    {
        Arc::make_mut(value)
    }
}

/// Consume a `Shared` resource, assuming that it is unique (i.e. not shared).
///
/// # Panics
///
/// Panics if the resource is shared (i.e. has other outstanding references).
pub fn shared_take<T: Clone>(value: Shared<T>) -> T {
    #[cfg(not(feature = "sync"))]
    {
        Rc::try_unwrap(value).map_err(|_| ()).unwrap()
    }
    #[cfg(feature = "sync")]
    {
        Arc::try_unwrap(value).map_err(|_| ()).unwrap()
    }
}

pub type FnCallArgs<'a> = [&'a mut Dynamic];

#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>>;
#[cfg(feature = "sync")]
pub type FnAny = dyn Fn(&mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;

pub type IteratorFn = fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

#[cfg(not(feature = "sync"))]
pub type Callback<T, R> = Box<dyn Fn(&T) -> R + 'static>;
#[cfg(feature = "sync")]
pub type Callback<T, R> = Box<dyn Fn(&T) -> R + Send + Sync + 'static>;

/// A type encapsulating a function callable by Rhai.
#[derive(Clone)]
pub enum CallableFunction {
    /// A pure native Rust function with all arguments passed by value.
    Pure(Shared<FnAny>),
    /// A native Rust object method with the first argument passed by reference,
    /// and the rest passed by value.
    Method(Shared<FnAny>),
    /// An iterator function.
    Iterator(IteratorFn),
    /// A script-defined function.
    Script(Shared<FnDef>),
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
