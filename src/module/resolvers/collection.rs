use crate::engine::Engine;
use crate::module::{Module, ModuleResolver};
use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{boxed::Box, vec::Vec};

/// Module resolution service that holds a collection of module resolves,
/// to be searched in sequential order.
///
/// # Examples
///
/// ```
/// use rhai::{Engine, Module};
/// use rhai::module_resolvers::{StaticModuleResolver, ModuleResolversCollection};
///
/// let mut collection = ModuleResolversCollection::new();
///
/// let resolver = StaticModuleResolver::new();
/// collection.push(resolver);
///
/// let mut engine = Engine::new();
/// engine.set_module_resolver(Some(collection));
/// ```
#[derive(Default)]
pub struct ModuleResolversCollection(Vec<Box<dyn ModuleResolver>>);

impl ModuleResolversCollection {
    /// Create a new `ModuleResolversCollection`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Engine, Module};
    /// use rhai::module_resolvers::{StaticModuleResolver, ModuleResolversCollection};
    ///
    /// let mut collection = ModuleResolversCollection::new();
    ///
    /// let resolver = StaticModuleResolver::new();
    /// collection.push(resolver);
    ///
    /// let mut engine = Engine::new();
    /// engine.set_module_resolver(Some(collection));
    /// ```
    pub fn new() -> Self {
        Default::default()
    }
}

impl ModuleResolversCollection {
    /// Add a module keyed by its path.
    pub fn push(&mut self, resolver: impl ModuleResolver + 'static) {
        self.0.push(Box::new(resolver));
    }
    /// Get an iterator of all the module resolvers.
    pub fn iter(&self) -> impl Iterator<Item = &dyn ModuleResolver> {
        self.0.iter().map(|v| v.as_ref())
    }
    /// Remove all module resolvers.
    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl ModuleResolver for ModuleResolversCollection {
    fn resolve(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Result<Module, Box<EvalAltResult>> {
        for resolver in self.0.iter() {
            match resolver.resolve(engine, path, pos) {
                Ok(module) => return Ok(module),
                Err(err) => match *err {
                    EvalAltResult::ErrorModuleNotFound(_, _) => continue,
                    EvalAltResult::ErrorInModule(_, err, _) => return Err(err),
                    _ => panic!("ModuleResolver::resolve returns error that is not ErrorModuleNotFound or ErrorInModule"),
                },
            }
        }

        EvalAltResult::ErrorModuleNotFound(path.into(), pos).into()
    }
}
