use crate::fn_native::SendSync;
use crate::stdlib::boxed::Box;
use crate::{Engine, EvalAltResult, Module, Position, Shared, AST};

mod dummy;
pub use dummy::DummyModuleResolver;

mod collection;
pub use collection::ModuleResolversCollection;

#[cfg(not(feature = "no_std"))]
#[cfg(not(target_arch = "wasm32"))]
mod file;

#[cfg(not(feature = "no_std"))]
#[cfg(not(target_arch = "wasm32"))]
pub use file::FileModuleResolver;

mod stat;
pub use stat::StaticModuleResolver;

/// Trait that encapsulates a module resolution service.
pub trait ModuleResolver: SendSync {
    /// Resolve a module based on a path string.
    fn resolve(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Result<Shared<Module>, Box<EvalAltResult>>;

    /// Resolve a module into an `AST` based on a path string.
    ///
    /// Returns [`None`] (default) if such resolution is not supported
    /// (e.g. if the module is Rust-based).
    ///
    /// ## Low-Level API
    ///
    /// Override the default implementation of this method if the module resolver
    /// serves modules based on compiled Rhai scripts.
    #[allow(unused_variables)]
    fn resolve_ast(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Option<Result<AST, Box<EvalAltResult>>> {
        None
    }
}
