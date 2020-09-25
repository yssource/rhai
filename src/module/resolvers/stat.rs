use crate::engine::Engine;
use crate::module::{Module, ModuleResolver};
use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{boxed::Box, collections::HashMap, string::String};

/// Module resolution service that serves modules added into it.
///
/// # Examples
///
/// ```
/// use rhai::{Engine, Module};
/// use rhai::module_resolvers::StaticModuleResolver;
///
/// let mut resolver = StaticModuleResolver::new();
///
/// let module = Module::new();
/// resolver.insert("hello".to_string(), module);
///
/// let mut engine = Engine::new();
///
/// engine.set_module_resolver(Some(resolver));
/// ```
#[derive(Debug, Clone, Default)]
pub struct StaticModuleResolver(HashMap<String, Module>);

impl StaticModuleResolver {
    /// Create a new `StaticModuleResolver`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Engine, Module};
    /// use rhai::module_resolvers::StaticModuleResolver;
    ///
    /// let mut resolver = StaticModuleResolver::new();
    ///
    /// let module = Module::new();
    /// resolver.insert("hello", module);
    ///
    /// let mut engine = Engine::new();
    /// engine.set_module_resolver(Some(resolver));
    /// ```
    pub fn new() -> Self {
        Default::default()
    }
}

impl StaticModuleResolver {
    /// Add a module keyed by its path.
    pub fn insert<S: Into<String>>(&mut self, path: S, mut module: Module) {
        module.index_all_sub_modules();
        self.0.insert(path.into(), module);
    }
    /// Remove a module given its path.
    pub fn remove(&mut self, path: &str) -> Option<Module> {
        self.0.remove(path)
    }
    /// Does the path exist?
    pub fn contains_path(&self, path: &str) -> bool {
        self.0.contains_key(path)
    }
    /// Get an iterator of all the modules.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &Module)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v))
    }
    /// Get a mutable iterator of all the modules.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut Module)> {
        self.0.iter_mut().map(|(k, v)| (k.as_str(), v))
    }
    /// Get an iterator of all the module paths.
    pub fn paths(&self) -> impl Iterator<Item = &str> {
        self.0.keys().map(String::as_str)
    }
    /// Get an iterator of all the modules.
    pub fn values(&self) -> impl Iterator<Item = &Module> {
        self.0.values()
    }
    /// Get a mutable iterator of all the modules.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Module> {
        self.0.values_mut()
    }
    /// Remove all modules.
    pub fn clear(&mut self) {
        self.0.clear();
    }
}

impl ModuleResolver for StaticModuleResolver {
    fn resolve(&self, _: &Engine, path: &str, pos: Position) -> Result<Module, Box<EvalAltResult>> {
        self.0
            .get(path)
            .cloned()
            .ok_or_else(|| EvalAltResult::ErrorModuleNotFound(path.into(), pos).into())
    }
}
