//! Module defining plugins in Rhai. Is exported for use by plugin authors.

use crate::stdlib::{any::TypeId, boxed::Box};

pub use crate::any::{Dynamic, Variant};
pub use crate::fn_native::{CallableFunction, FnCallArgs, IteratorFn};
pub use crate::parser::{
    FnAccess,
    FnAccess::{Private, Public},
    AST,
};
pub use crate::result::EvalAltResult;
pub use crate::scope::{Entry as ScopeEntry, EntryType as ScopeEntryType, Scope};
pub use crate::token::{Position, Token};
pub use crate::utils::StaticVec;
pub use crate::Engine;

#[cfg(features = "sync")]
/// Represents an externally-written plugin for the Rhai interpreter.
///
/// This trait should not be used directly. Use the `#[plugin]` procedural attribute instead.
pub trait Plugin: Send {
    fn register_contents(self, engine: &mut Engine);
}

#[cfg(not(features = "sync"))]
/// Represents an externally-written plugin for the Rhai interpreter.
///
/// This trait should not be used directly. Use the `#[plugin]` procedural attribute instead.
pub trait Plugin: Send + Sync {
    fn register_contents(self, engine: &mut Engine);
}

/// Represents a function that is statically defined within a plugin.
///
/// This trait should not be used directly. Use the `#[plugin]` procedural attribute instead.
pub trait PluginFunction {
    fn is_method_call(&self) -> bool;
    fn is_varadic(&self) -> bool;

    fn call(&self, args: &mut[&mut Dynamic], pos: Position) -> Result<Dynamic, Box<EvalAltResult>>;

    fn clone_boxed(&self) -> Box<dyn PluginFunction>;

    fn input_types(&self) -> Box<[TypeId]>;
}
