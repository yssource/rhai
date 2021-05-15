//! Module defining interfaces to native-Rust functions.

use crate::ast::{FnAccess, FnCallHashes};
use crate::engine::Imports;
use crate::plugin::PluginFunction;
use crate::token::is_valid_identifier;
use crate::{
    calc_fn_hash, Dynamic, Engine, EvalAltResult, EvalContext, Identifier, Module, Position,
    RhaiResult, StaticVec,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    convert::{TryFrom, TryInto},
    fmt,
    iter::{empty, once},
    mem,
};

/// Trait that maps to `Send + Sync` only under the `sync` feature.
#[cfg(feature = "sync")]
pub trait SendSync: Send + Sync {}
/// Trait that maps to `Send + Sync` only under the `sync` feature.
#[cfg(feature = "sync")]
impl<T: Send + Sync> SendSync for T {}

/// Trait that maps to `Send + Sync` only under the `sync` feature.
#[cfg(not(feature = "sync"))]
pub trait SendSync {}
/// Trait that maps to `Send + Sync` only under the `sync` feature.
#[cfg(not(feature = "sync"))]
impl<T> SendSync for T {}

/// Immutable reference-counted container.
#[cfg(not(feature = "sync"))]
pub use std::rc::Rc as Shared;
/// Immutable reference-counted container.
#[cfg(feature = "sync")]
pub use std::sync::Arc as Shared;

/// Synchronized shared object.
///
/// Not available under `no_closure`.
#[cfg(not(feature = "no_closure"))]
#[cfg(not(feature = "sync"))]
pub use std::cell::RefCell as Locked;
/// Synchronized shared object.
///
/// Not available under `no_closure`.
#[cfg(not(feature = "no_closure"))]
#[cfg(feature = "sync")]
pub use std::sync::RwLock as Locked;

/// Context of a native Rust function call.
#[derive(Debug)]
pub struct NativeCallContext<'a> {
    engine: &'a Engine,
    fn_name: &'a str,
    source: Option<&'a str>,
    mods: Option<&'a Imports>,
    lib: &'a [&'a Module],
}

impl<'a, M: AsRef<[&'a Module]> + ?Sized>
    From<(&'a Engine, &'a str, Option<&'a str>, &'a Imports, &'a M)> for NativeCallContext<'a>
{
    #[inline(always)]
    fn from(value: (&'a Engine, &'a str, Option<&'a str>, &'a Imports, &'a M)) -> Self {
        Self {
            engine: value.0,
            fn_name: value.1,
            source: value.2,
            mods: Some(value.3),
            lib: value.4.as_ref(),
        }
    }
}

impl<'a, M: AsRef<[&'a Module]> + ?Sized> From<(&'a Engine, &'a str, &'a M)>
    for NativeCallContext<'a>
{
    #[inline(always)]
    fn from(value: (&'a Engine, &'a str, &'a M)) -> Self {
        Self {
            engine: value.0,
            fn_name: value.1,
            source: None,
            mods: None,
            lib: value.2.as_ref(),
        }
    }
}

impl<'a> NativeCallContext<'a> {
    /// Create a new [`NativeCallContext`].
    #[inline(always)]
    pub fn new(engine: &'a Engine, fn_name: &'a str, lib: &'a [&Module]) -> Self {
        Self {
            engine,
            fn_name,
            source: None,
            mods: None,
            lib,
        }
    }
    /// _(INTERNALS)_ Create a new [`NativeCallContext`].
    /// Exported under the `internals` feature only.
    ///
    /// Not available under `no_module`.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn new_with_all_fields(
        engine: &'a Engine,
        fn_name: &'a str,
        source: &'a Option<&str>,
        imports: &'a Imports,
        lib: &'a [&Module],
    ) -> Self {
        Self {
            engine,
            fn_name,
            source: source.clone(),
            mods: Some(imports),
            lib,
        }
    }
    /// The current [`Engine`].
    #[inline(always)]
    pub fn engine(&self) -> &Engine {
        self.engine
    }
    /// Name of the function called.
    #[inline(always)]
    pub fn fn_name(&self) -> &str {
        self.fn_name
    }
    /// The current source.
    #[inline(always)]
    pub fn source(&self) -> Option<&str> {
        self.source
    }
    /// Get an iterator over the current set of modules imported via `import` statements.
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn iter_imports(&self) -> impl Iterator<Item = (&str, &Module)> {
        self.mods.iter().flat_map(|&m| m.iter())
    }
    /// Get an iterator over the current set of modules imported via `import` statements.
    #[cfg(not(feature = "no_module"))]
    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn iter_imports_raw(
        &self,
    ) -> impl Iterator<Item = (&crate::Identifier, &Shared<Module>)> {
        self.mods.iter().flat_map(|&m| m.iter_raw())
    }
    /// _(INTERNALS)_ The current set of modules imported via `import` statements.
    /// Exported under the `internals` feature only.
    ///
    /// Not available under `no_module`.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn imports(&self) -> Option<&Imports> {
        self.mods
    }
    /// Get an iterator over the namespaces containing definitions of all script-defined functions.
    #[inline(always)]
    pub fn iter_namespaces(&self) -> impl Iterator<Item = &Module> {
        self.lib.iter().cloned()
    }
    /// _(INTERNALS)_ The current set of namespaces containing definitions of all script-defined functions.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline(always)]
    pub fn namespaces(&self) -> &[&Module] {
        self.lib
    }
    /// Call a function inside the call context.
    ///
    /// # WARNING
    ///
    /// All arguments may be _consumed_, meaning that they may be replaced by `()`.
    /// This is to avoid unnecessarily cloning the arguments.
    ///
    /// Do not use the arguments after this call. If they are needed afterwards,
    /// clone them _before_ calling this function.
    ///
    /// If `is_method` is [`true`], the first argument is assumed to be passed
    /// by reference and is not consumed.
    #[inline(always)]
    pub fn call_fn_dynamic_raw(
        &self,
        fn_name: impl AsRef<str>,
        is_method: bool,
        args: &mut [&mut Dynamic],
    ) -> RhaiResult {
        let fn_name = fn_name.as_ref();

        let hash = if is_method {
            FnCallHashes::from_script_and_native(
                calc_fn_hash(empty(), fn_name, args.len() - 1),
                calc_fn_hash(empty(), fn_name, args.len()),
            )
        } else {
            FnCallHashes::from_script(calc_fn_hash(empty(), fn_name, args.len()))
        };

        self.engine()
            .exec_fn_call(
                &mut self.mods.cloned().unwrap_or_default(),
                &mut Default::default(),
                self.lib,
                fn_name,
                hash,
                args,
                is_method,
                is_method,
                Position::NONE,
                None,
                0,
            )
            .map(|(r, _)| r)
    }
}

/// Consume a [`Shared`] resource and return a mutable reference to the wrapped value.
/// If the resource is shared (i.e. has other outstanding references), a cloned copy is used.
#[inline(always)]
pub fn shared_make_mut<T: Clone>(value: &mut Shared<T>) -> &mut T {
    Shared::make_mut(value)
}

/// Consume a [`Shared`] resource if is unique (i.e. not shared), or clone it otherwise.
#[inline(always)]
pub fn shared_take_or_clone<T: Clone>(value: Shared<T>) -> T {
    shared_try_take(value).unwrap_or_else(|v| v.as_ref().clone())
}

/// Consume a [`Shared`] resource if is unique (i.e. not shared).
#[inline(always)]
pub fn shared_try_take<T>(value: Shared<T>) -> Result<T, Shared<T>> {
    Shared::try_unwrap(value)
}

/// Consume a [`Shared`] resource, assuming that it is unique (i.e. not shared).
///
/// # Panics
///
/// Panics if the resource is shared (i.e. has other outstanding references).
#[inline(always)]
pub fn shared_take<T>(value: Shared<T>) -> T {
    shared_try_take(value).map_err(|_| ()).unwrap()
}

/// Arguments to a function call, which is a list of [`&mut Dynamic`][Dynamic].
pub type FnCallArgs<'a> = [&'a mut Dynamic];

/// A general function pointer, which may carry additional (i.e. curried) argument values
/// to be passed onto a function during a call.
#[derive(Debug, Clone)]
pub struct FnPtr(Identifier, StaticVec<Dynamic>);

impl FnPtr {
    /// Create a new function pointer.
    #[inline(always)]
    pub fn new(name: impl Into<Identifier>) -> Result<Self, Box<EvalAltResult>> {
        name.into().try_into()
    }
    /// Create a new function pointer without checking its parameters.
    #[inline(always)]
    pub(crate) fn new_unchecked(name: Identifier, curry: StaticVec<Dynamic>) -> Self {
        Self(name.into(), curry)
    }
    /// Get the name of the function.
    #[inline(always)]
    pub fn fn_name(&self) -> &str {
        self.get_fn_name().as_ref()
    }
    /// Get the name of the function.
    #[inline(always)]
    pub(crate) fn get_fn_name(&self) -> &Identifier {
        &self.0
    }
    /// Get the underlying data of the function pointer.
    #[inline(always)]
    pub(crate) fn take_data(self) -> (Identifier, StaticVec<Dynamic>) {
        (self.0, self.1)
    }
    /// Get the curried arguments.
    #[inline(always)]
    pub fn curry(&self) -> &[Dynamic] {
        self.1.as_ref()
    }
    /// Add a new curried argument.
    #[inline(always)]
    pub fn add_curry(&mut self, value: Dynamic) -> &mut Self {
        self.1.push(value);
        self
    }
    /// Set curried arguments to the function pointer.
    #[inline(always)]
    pub fn set_curry(&mut self, values: impl IntoIterator<Item = Dynamic>) -> &mut Self {
        self.1 = values.into_iter().collect();
        self
    }
    /// Is the function pointer curried?
    #[inline(always)]
    pub fn is_curried(&self) -> bool {
        !self.1.is_empty()
    }
    /// Does the function pointer refer to an anonymous function?
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn is_anonymous(&self) -> bool {
        self.0.starts_with(crate::engine::FN_ANONYMOUS)
    }
    /// Call the function pointer with curried arguments (if any).
    ///
    /// If this function is a script-defined function, it must not be marked private.
    ///
    /// # WARNING
    ///
    /// All the arguments are _consumed_, meaning that they're replaced by `()`.
    /// This is to avoid unnecessarily cloning the arguments.
    /// Do not use the arguments after this call. If they are needed afterwards,
    /// clone them _before_ calling this function.
    #[inline(always)]
    pub fn call_dynamic(
        &self,
        ctx: &NativeCallContext,
        this_ptr: Option<&mut Dynamic>,
        mut arg_values: impl AsMut<[Dynamic]>,
    ) -> RhaiResult {
        let mut args_data;

        let arg_values = if self.curry().is_empty() {
            arg_values.as_mut()
        } else {
            args_data = self
                .curry()
                .iter()
                .cloned()
                .chain(arg_values.as_mut().iter_mut().map(mem::take))
                .collect::<StaticVec<_>>();

            args_data.as_mut()
        };

        let is_method = this_ptr.is_some();

        let mut args: StaticVec<_> = if let Some(obj) = this_ptr {
            once(obj).chain(arg_values.iter_mut()).collect()
        } else {
            arg_values.iter_mut().collect()
        };

        ctx.call_fn_dynamic_raw(self.fn_name(), is_method, &mut args)
    }
}

impl fmt::Display for FnPtr {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Fn({})", self.0)
    }
}

impl TryFrom<Identifier> for FnPtr {
    type Error = Box<EvalAltResult>;

    #[inline(always)]
    fn try_from(value: Identifier) -> Result<Self, Self::Error> {
        if is_valid_identifier(value.chars()) {
            Ok(Self(value, Default::default()))
        } else {
            EvalAltResult::ErrorFunctionNotFound(value.to_string(), Position::NONE).into()
        }
    }
}

#[cfg(not(feature = "no_smartstring"))]
impl TryFrom<crate::ImmutableString> for FnPtr {
    type Error = Box<EvalAltResult>;

    #[inline(always)]
    fn try_from(value: crate::ImmutableString) -> Result<Self, Self::Error> {
        let s: Identifier = value.into();
        Self::try_from(s)
    }
}

impl TryFrom<String> for FnPtr {
    type Error = Box<EvalAltResult>;

    #[inline(always)]
    fn try_from(value: String) -> Result<Self, Self::Error> {
        let s: Identifier = value.into();
        Self::try_from(s)
    }
}

impl TryFrom<&str> for FnPtr {
    type Error = Box<EvalAltResult>;

    #[inline(always)]
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let s: Identifier = value.into();
        Self::try_from(s)
    }
}

/// A general function trail object.
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(NativeCallContext, &mut FnCallArgs) -> RhaiResult;
/// A general function trail object.
#[cfg(feature = "sync")]
pub type FnAny = dyn Fn(NativeCallContext, &mut FnCallArgs) -> RhaiResult + Send + Sync;

/// A standard function that gets an iterator from a type.
pub type IteratorFn = fn(Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

#[cfg(not(feature = "sync"))]
pub type FnPlugin = dyn PluginFunction;
#[cfg(feature = "sync")]
pub type FnPlugin = dyn PluginFunction + Send + Sync;

/// A standard callback function for progress reporting.
#[cfg(not(feature = "unchecked"))]
#[cfg(not(feature = "sync"))]
pub type OnProgressCallback = Box<dyn Fn(u64) -> Option<Dynamic> + 'static>;
/// A standard callback function for progress reporting.
#[cfg(not(feature = "unchecked"))]
#[cfg(feature = "sync")]
pub type OnProgressCallback = Box<dyn Fn(u64) -> Option<Dynamic> + Send + Sync + 'static>;

/// A standard callback function for printing.
#[cfg(not(feature = "sync"))]
pub type OnPrintCallback = Box<dyn Fn(&str) + 'static>;
/// A standard callback function for printing.
#[cfg(feature = "sync")]
pub type OnPrintCallback = Box<dyn Fn(&str) + Send + Sync + 'static>;

/// A standard callback function for debugging.
#[cfg(not(feature = "sync"))]
pub type OnDebugCallback = Box<dyn Fn(&str, Option<&str>, Position) + 'static>;
/// A standard callback function for debugging.
#[cfg(feature = "sync")]
pub type OnDebugCallback = Box<dyn Fn(&str, Option<&str>, Position) + Send + Sync + 'static>;

/// A standard callback function for variable access.
#[cfg(not(feature = "sync"))]
pub type OnVarCallback =
    Box<dyn Fn(&str, usize, &EvalContext) -> Result<Option<Dynamic>, Box<EvalAltResult>> + 'static>;
/// A standard callback function for variable access.
#[cfg(feature = "sync")]
pub type OnVarCallback = Box<
    dyn Fn(&str, usize, &EvalContext) -> Result<Option<Dynamic>, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
>;

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
    #[inline(always)]
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
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pure(_) => write!(f, "NativePureFunction"),
            Self::Method(_) => write!(f, "NativeMethod"),
            Self::Iterator(_) => write!(f, "NativeIterator"),
            Self::Plugin(_) => write!(f, "PluginFunction"),

            #[cfg(not(feature = "no_function"))]
            CallableFunction::Script(s) => fmt::Display::fmt(s, f),
        }
    }
}

impl CallableFunction {
    /// Is this a pure native Rust function?
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    pub fn is_iter(&self) -> bool {
        match self {
            Self::Iterator(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Plugin(_) => false,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Is this a Rhai-scripted function?
    #[inline(always)]
    pub fn is_script(&self) -> bool {
        match self {
            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => true,

            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) | Self::Plugin(_) => false,
        }
    }
    /// Is this a plugin function?
    #[inline(always)]
    pub fn is_plugin_fn(&self) -> bool {
        match self {
            Self::Plugin(_) => true,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => false,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Is this a native Rust function?
    #[inline(always)]
    pub fn is_native(&self) -> bool {
        match self {
            Self::Pure(_) | Self::Method(_) => true,
            Self::Plugin(_) => true,
            Self::Iterator(_) => true,

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => false,
        }
    }
    /// Get the access mode.
    #[inline(always)]
    pub fn access(&self) -> FnAccess {
        match self {
            Self::Plugin(_) => FnAccess::Public,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => FnAccess::Public,

            #[cfg(not(feature = "no_function"))]
            Self::Script(f) => f.access,
        }
    }
    /// Get a shared reference to a native Rust function.
    ///
    /// # Panics
    ///
    /// Panics if the [`CallableFunction`] is not [`Pure`][CallableFunction::Pure] or
    /// [`Method`][CallableFunction::Method].
    #[inline(always)]
    pub fn get_native_fn(&self) -> &Shared<FnAny> {
        match self {
            Self::Pure(f) | Self::Method(f) => f,
            Self::Iterator(_) | Self::Plugin(_) => panic!("function should be native"),

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => panic!("function should be native"),
        }
    }
    /// Get a shared reference to a script-defined function definition.
    ///
    /// Not available under `no_function`.
    ///
    /// # Panics
    ///
    /// Panics if the [`CallableFunction`] is not [`Script`][CallableFunction::Script].
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn get_fn_def(&self) -> &Shared<crate::ast::ScriptFnDef> {
        match self {
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) | Self::Plugin(_) => {
                panic!("function should be scripted")
            }
            Self::Script(f) => f,
        }
    }
    /// Get a reference to an iterator function.
    ///
    /// # Panics
    ///
    /// Panics if the [`CallableFunction`] is not [`Iterator`][CallableFunction::Iterator].
    #[inline(always)]
    pub fn get_iter_fn(&self) -> IteratorFn {
        match self {
            Self::Iterator(f) => *f,
            Self::Pure(_) | Self::Method(_) | Self::Plugin(_) => {
                panic!("function should an iterator")
            }

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => panic!("function should be an iterator"),
        }
    }
    /// Get a shared reference to a plugin function.
    ///
    /// # Panics
    ///
    /// Panics if the [`CallableFunction`] is not [`Plugin`][CallableFunction::Plugin].
    #[inline(always)]
    pub fn get_plugin_fn<'s>(&'s self) -> &Shared<FnPlugin> {
        match self {
            Self::Plugin(f) => f,
            Self::Pure(_) | Self::Method(_) | Self::Iterator(_) => {
                panic!("function should a plugin")
            }

            #[cfg(not(feature = "no_function"))]
            Self::Script(_) => panic!("function should a plugin"),
        }
    }
    /// Create a new [`CallableFunction::Pure`].
    #[inline(always)]
    pub fn from_pure(func: Box<FnAny>) -> Self {
        Self::Pure(func.into())
    }
    /// Create a new [`CallableFunction::Method`].
    #[inline(always)]
    pub fn from_method(func: Box<FnAny>) -> Self {
        Self::Method(func.into())
    }
    /// Create a new [`CallableFunction::Plugin`].
    #[inline(always)]
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
        Self::Plugin(func.into())
    }
}
