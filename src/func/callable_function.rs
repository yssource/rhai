//! Module defining the standard Rhai function type.

use super::native::{FnAny, FnPlugin, IteratorFn, SendSync};
use crate::ast::FnAccess;
use crate::plugin::PluginFunction;
use crate::Shared;
use std::fmt;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

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
    /// A plugin function,
    Plugin(Shared<FnPlugin>),
    /// A script-defined function.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    Script(Shared<crate::ast::ScriptFnDef>),
}

impl fmt::Debug for CallableFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pure(_) => write!(f, "NativePureFunction"),
            Self::Method(_) => write!(f, "NativeMethod"),
            Self::Iterator(_) => write!(f, "NativeIterator"),
            Self::Plugin(_) => write!(f, "PluginFunction"),

            #[cfg(not(feature = "no_function"))]
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
            Self::Plugin(_) => write!(f, "PluginFunction"),

            #[cfg(not(feature = "no_function"))]
            Self::Script(s) => fmt::Display::fmt(s, f),
        }
    }
}

impl CallableFunction {
    /// Is this a pure native Rust function?
    #[inline]
    #[must_use]
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Pure(_) => true,
            Self::Method(_) | Self::Iterator(_) => false,

            Self::Plugin(p) => !p.is_method_call(),

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Is this a native Rust method function?
    #[inline]
    #[must_use]
    pub fn is_method(&self) -> bool {
        match self {
            Self::Method(_) => true,
            Self::Pure(_) | Self::Iterator(_) => false,

            Self::Plugin(p) => p.is_method_call(),

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Is this an iterator function?
    #[inline]
    #[must_use]
    pub const fn is_iter(&self) -> bool {
        match self {
            Self::Iterator(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Plugin(_) => false,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Is this a script-defined function?
    #[inline]
    #[must_use]
    pub const fn is_script(&self) -> bool {
        #[cfg(feature = "no_function")]
        return false;

        #[cfg(not(feature = "no_function"))]
        match self {
            Self::Script(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) | Self::Plugin(_) => false,
        }
    }
    /// Is this a plugin function?
    #[inline]
    #[must_use]
    pub const fn is_plugin_fn(&self) -> bool {
        match self {
            Self::Plugin(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => false,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Is this a native Rust function?
    #[inline]
    #[must_use]
    pub const fn is_native(&self) -> bool {
        #[cfg(feature = "no_function")]
        return true;

        #[cfg(not(feature = "no_function"))]
        match self {
            Self::Pure(_) | Self::Method(_) => true,
            Self::Plugin(_) => true,
            Self::Iterator(_) => true,
            Self::Script(_) => false,
        }
    }
    /// Get the access mode.
    #[inline]
    #[must_use]
    pub fn access(&self) -> FnAccess {
        #[cfg(feature = "no_function")]
        return FnAccess::Public;

        #[cfg(not(feature = "no_function"))]
        match self {
            Self::Plugin(_) => FnAccess::Public,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => FnAccess::Public,
            Self::Script(f) => f.access,
        }
    }
    /// Get a shared reference to a native Rust function.
    #[inline]
    #[must_use]
    pub fn get_native_fn(&self) -> Option<&Shared<FnAny>> {
        match self {
            Self::Pure(f) | Self::Method(f) => Some(f),
            Self::Iterator(_) | Self::Plugin(_) => None,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => None,
        }
    }
    /// Get a shared reference to a script-defined function definition.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline]
    #[must_use]
    pub const fn get_script_fn_def(&self) -> Option<&Shared<crate::ast::ScriptFnDef>> {
        match self {
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) | Self::Plugin(_) => None,
            Self::Script(f) => Some(f),
        }
    }
    /// Get a reference to an iterator function.
    #[inline]
    #[must_use]
    pub fn get_iter_fn(&self) -> Option<IteratorFn> {
        match self {
            Self::Iterator(f) => Some(*f),
            Self::Pure(_) | Self::Method(_) | Self::Plugin(_) => None,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => None,
        }
    }
    /// Get a shared reference to a plugin function.
    #[inline]
    #[must_use]
    pub fn get_plugin_fn(&self) -> Option<&Shared<FnPlugin>> {
        match self {
            Self::Plugin(f) => Some(f),
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => None,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => None,
        }
    }
    /// Create a new [`CallableFunction::Pure`].
    #[inline(always)]
    #[must_use]
    pub fn from_pure(func: Box<FnAny>) -> Self {
        Self::Pure(func.into())
    }
    /// Create a new [`CallableFunction::Method`].
    #[inline(always)]
    #[must_use]
    pub fn from_method(func: Box<FnAny>) -> Self {
        Self::Method(func.into())
    }
    /// Create a new [`CallableFunction::Plugin`].
    #[inline(always)]
    #[must_use]
    pub fn from_plugin(func: impl PluginFunction + 'static + SendSync) -> Self {
        Self::Plugin((Box::new(func) as Box<FnPlugin>).into())
    }
}

impl From<IteratorFn> for CallableFunction {
    #[inline(always)]
    fn from(func: IteratorFn) -> Self {
        Self::Iterator(func)
    }
}

#[cfg(not(feature = "no_function"))]
impl From<crate::ast::ScriptFnDef> for CallableFunction {
    #[inline(always)]
    fn from(_func: crate::ast::ScriptFnDef) -> Self {
        Self::Script(_func.into())
    }
}

#[cfg(not(feature = "no_function"))]
impl From<Shared<crate::ast::ScriptFnDef>> for CallableFunction {
    #[inline(always)]
    fn from(_func: Shared<crate::ast::ScriptFnDef>) -> Self {
        Self::Script(_func)
    }
}

impl<T: PluginFunction + 'static + SendSync> From<T> for CallableFunction {
    #[inline(always)]
    fn from(func: T) -> Self {
        Self::from_plugin(func)
    }
}

impl From<Shared<FnPlugin>> for CallableFunction {
    #[inline(always)]
    fn from(func: Shared<FnPlugin>) -> Self {
        Self::Plugin(func)
    }
}
