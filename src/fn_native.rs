use crate::any::Dynamic;
use crate::engine::Engine;
use crate::parser::ScriptFnDef;
use crate::result::EvalAltResult;
use crate::utils::ImmutableString;

use crate::stdlib::{boxed::Box, fmt, rc::Rc, sync::Arc};

/// Trait that maps to `Send + Sync` only under the `sync` feature.
#[cfg(feature = "sync")]
pub trait SendSync: Send + Sync {}
#[cfg(feature = "sync")]
impl<T: Send + Sync> SendSync for T {}

/// Trait that maps to `Send + Sync` only under the `sync` feature.
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

/// A general function pointer.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Default)]
pub struct FnPtr(ImmutableString);

impl FnPtr {
    /// Get the name of the function.
    pub fn fn_name(&self) -> &str {
        self.get_fn_name().as_ref()
    }
    /// Get the name of the function.
    pub(crate) fn get_fn_name(&self) -> &ImmutableString {
        &self.0
    }
}

impl fmt::Display for FnPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Fn({})", self.0)
    }
}

impl<S: Into<ImmutableString>> From<S> for FnPtr {
    fn from(value: S) -> Self {
        Self(value.into())
    }
}

/// A general function trail object.
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&Engine, &mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>>;
/// A general function trail object.
#[cfg(feature = "sync")]
pub type FnAny =
    dyn Fn(&Engine, &mut FnCallArgs) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;

/// A standard function that gets an iterator from a type.
pub type IteratorFn = fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

/// A standard callback function.
#[cfg(not(feature = "sync"))]
pub type Callback<T, R> = Box<dyn Fn(&T) -> R + 'static>;
/// A standard callback function.
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
    Script(Shared<ScriptFnDef>),
}

impl fmt::Debug for CallableFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pure(_) => write!(f, "NativePureFunction"),
            Self::Method(_) => write!(f, "NativeMethod"),
            Self::Iterator(_) => write!(f, "NativeIterator"),
            Self::Script(fn_def) => fmt::Debug::fmt(fn_def, f),
        }
    }
}

impl fmt::Display for CallableFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pure(_) => write!(f, "NativePureFunction"),
            Self::Method(_) => write!(f, "NativeMethod"),
            Self::Iterator(_) => write!(f, "NativeIterator"),
            CallableFunction::Script(s) => fmt::Display::fmt(s, f),
        }
    }
}

impl CallableFunction {
    /// Is this a pure native Rust function?
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Pure(_) => true,
            Self::Method(_) | Self::Iterator(_) | Self::Script(_) => false,
        }
    }
    /// Is this a native Rust method function?
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
            Self::Iterator(_) | Self::Script(_) => unreachable!(),
        }
    }
    /// Get a shared reference to a script-defined function definition.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Script`.
    pub fn get_shared_fn_def(&self) -> Shared<ScriptFnDef> {
        match self {
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => unreachable!(),
            Self::Script(f) => f.clone(),
        }
    }
    /// Get a reference to a script-defined function definition.
    ///
    /// # Panics
    ///
    /// Panics if the `CallableFunction` is not `Script`.
    pub fn get_fn_def(&self) -> &ScriptFnDef {
        match self {
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => unreachable!(),
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
            Self::Pure(_) | Self::Method(_) | Self::Script(_) => unreachable!(),
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

impl From<IteratorFn> for CallableFunction {
    fn from(func: IteratorFn) -> Self {
        Self::Iterator(func)
    }
}

impl From<ScriptFnDef> for CallableFunction {
    fn from(func: ScriptFnDef) -> Self {
        Self::Script(func.into())
    }
}

impl From<Shared<ScriptFnDef>> for CallableFunction {
    fn from(func: Shared<ScriptFnDef>) -> Self {
        Self::Script(func)
    }
}
