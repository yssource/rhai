//! Module defining macros for developing _plugins_.

pub use crate::fn_native::{CallableFunction, FnCallArgs};
pub use crate::stdlib::{any::TypeId, boxed::Box, format, mem, string::ToString, vec as new_vec};
pub use crate::{
    Dynamic, Engine, EvalAltResult, FnAccess, FnNamespace, ImmutableString, Module,
    NativeCallContext, RegisterFn, RegisterResultFn,
};

#[cfg(not(features = "no_module"))]
pub use rhai_codegen::*;
#[cfg(features = "no_module")]
pub use rhai_codegen::{export_fn, register_exported_fn};

/// Trait implemented by a _plugin function_.
/// This trait should not be used directly.
///
/// Use the `#[export_module]` and `#[export_fn]` procedural attributes instead.
pub trait PluginFunction {
    /// Call the plugin function with the arguments provided.
    fn call(
        &self,
        context: NativeCallContext,
        args: &mut FnCallArgs,
    ) -> Result<Dynamic, Box<EvalAltResult>>;

    /// Is this plugin function a method?
    fn is_method_call(&self) -> bool;

    /// Is this plugin function variadic?
    fn is_variadic(&self) -> bool;

    /// Convert a plugin function into a boxed trait object.
    fn clone_boxed(&self) -> Box<dyn PluginFunction>;

    /// Return a boxed slice of the names of the function's parameters.
    fn input_names(&self) -> Box<[&'static str]>;

    /// Return a boxed slice of type ID's of the function's parameters.
    fn input_types(&self) -> Box<[TypeId]>;
}
