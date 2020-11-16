use crate::fn_native::SendSync;
use crate::stdlib::boxed::Box;
use crate::{Engine, EvalAltResult, Module, Position, Shared};

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
}
