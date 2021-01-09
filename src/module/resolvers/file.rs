use crate::stdlib::{
    boxed::Box,
    collections::HashMap,
    io::Error as IoError,
    path::{Path, PathBuf},
    string::String,
};
use crate::{Engine, EvalAltResult, Module, ModuleResolver, Position, Shared};

/// [Module] resolution service that loads [module][Module] script files from the file system.
///
/// Script files are cached so they are are not reloaded and recompiled in subsequent requests.
///
/// # Function Namespace
///
/// When a function within a script file module is called, all functions in the _global_ namespace
/// plus all those defined within the same module are _merged_ into a _unified_ namespace before
/// the call.  Therefore, functions in a module script can always cross-call each other.
///
/// # Example
///
/// ```
/// use rhai::Engine;
/// use rhai::module_resolvers::FileModuleResolver;
///
/// // Create a new 'FileModuleResolver' loading scripts from the 'scripts' subdirectory
/// // with file extension '.x'.
/// let resolver = FileModuleResolver::new_with_path_and_extension("./scripts", "x");
///
/// let mut engine = Engine::new();
///
/// engine.set_module_resolver(resolver);
/// ```
#[derive(Debug)]
pub struct FileModuleResolver {
    base_path: PathBuf,
    extension: String,

    #[cfg(not(feature = "sync"))]
    cache: crate::stdlib::cell::RefCell<HashMap<PathBuf, Shared<Module>>>,
    #[cfg(feature = "sync")]
    cache: crate::stdlib::sync::RwLock<HashMap<PathBuf, Shared<Module>>>,
}

impl Default for FileModuleResolver {
    #[inline(always)]
    fn default() -> Self {
        Self::new_with_path(PathBuf::default())
    }
}

impl FileModuleResolver {
    /// Create a new [`FileModuleResolver`] with a specific base path.
    ///
    /// The default extension is `.rhai`.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Engine;
    /// use rhai::module_resolvers::FileModuleResolver;
    ///
    /// // Create a new 'FileModuleResolver' loading scripts from the 'scripts' subdirectory
    /// // with file extension '.rhai' (the default).
    /// let resolver = FileModuleResolver::new_with_path("./scripts");
    ///
    /// let mut engine = Engine::new();
    /// engine.set_module_resolver(resolver);
    /// ```
    #[inline(always)]
    pub fn new_with_path(path: impl Into<PathBuf>) -> Self {
        Self::new_with_path_and_extension(path, "rhai")
    }

    /// Create a new [`FileModuleResolver`] with a specific base path and file extension.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Engine;
    /// use rhai::module_resolvers::FileModuleResolver;
    ///
    /// // Create a new 'FileModuleResolver' loading scripts from the 'scripts' subdirectory
    /// // with file extension '.x'.
    /// let resolver = FileModuleResolver::new_with_path_and_extension("./scripts", "x");
    ///
    /// let mut engine = Engine::new();
    /// engine.set_module_resolver(resolver);
    /// ```
    #[inline(always)]
    pub fn new_with_path_and_extension(
        path: impl Into<PathBuf>,
        extension: impl Into<String>,
    ) -> Self {
        Self {
            base_path: path.into(),
            extension: extension.into(),
            cache: Default::default(),
        }
    }

    /// Create a new [`FileModuleResolver`] with the current directory as base path.
    ///
    /// The default extension is `.rhai`.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Engine;
    /// use rhai::module_resolvers::FileModuleResolver;
    ///
    /// // Create a new 'FileModuleResolver' loading scripts from the current directory
    /// // with file extension '.rhai' (the default).
    /// let resolver = FileModuleResolver::new();
    ///
    /// let mut engine = Engine::new();
    /// engine.set_module_resolver(resolver);
    /// ```
    #[inline(always)]
    pub fn new() -> Self {
        Default::default()
    }

    /// Get the base path for script files.
    #[inline(always)]
    pub fn base_path(&self) -> &Path {
        self.base_path.as_ref()
    }
    /// Set the base path for script files.
    #[inline(always)]
    pub fn set_base_path(&mut self, path: impl Into<PathBuf>) -> &mut Self {
        self.base_path = path.into();
        self
    }

    /// Get the script file extension.
    #[inline(always)]
    pub fn extension(&self) -> &str {
        &self.extension
    }

    /// Set the script file extension.
    #[inline(always)]
    pub fn set_extension(&mut self, extension: impl Into<String>) -> &mut Self {
        self.extension = extension.into();
        self
    }

    /// Empty the internal cache.
    #[inline(always)]
    pub fn clear_cache(&mut self) {
        #[cfg(not(feature = "sync"))]
        self.cache.borrow_mut().clear();
        #[cfg(feature = "sync")]
        self.cache.write().unwrap().clear();
    }

    /// Remove the specified path from internal cache.
    ///
    /// The next time this path is resolved, the script file will be loaded once again.
    #[inline(always)]
    pub fn clear_cache_for_path(&mut self, path: impl AsRef<Path>) -> Option<Shared<Module>> {
        #[cfg(not(feature = "sync"))]
        return self
            .cache
            .borrow_mut()
            .remove_entry(path.as_ref())
            .map(|(_, v)| v);
        #[cfg(feature = "sync")]
        return self
            .cache
            .write()
            .unwrap()
            .remove_entry(path.as_ref())
            .map(|(_, v)| v);
    }
}

impl ModuleResolver for FileModuleResolver {
    fn resolve(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Result<Shared<Module>, Box<EvalAltResult>> {
        // Construct the script file path
        let mut file_path = self.base_path.clone();
        file_path.push(path);
        file_path.set_extension(&self.extension); // Force extension

        // See if it is cached
        {
            #[cfg(not(feature = "sync"))]
            let c = self.cache.borrow();
            #[cfg(feature = "sync")]
            let c = self.cache.read().unwrap();

            if let Some(module) = c.get(&file_path) {
                return Ok(module.clone());
            }
        }

        // Load the script file and compile it
        let scope = Default::default();

        let mut ast = engine
            .compile_file(file_path.clone())
            .map_err(|err| match *err {
                EvalAltResult::ErrorSystem(_, err) if err.is::<IoError>() => {
                    Box::new(EvalAltResult::ErrorModuleNotFound(path.to_string(), pos))
                }
                _ => Box::new(EvalAltResult::ErrorInModule(path.to_string(), err, pos)),
            })?;

        ast.set_source(path);

        // Make a module from the AST
        let m: Shared<Module> = Module::eval_ast_as_new(scope, &ast, engine)
            .map_err(|err| Box::new(EvalAltResult::ErrorInModule(path.to_string(), err, pos)))?
            .into();

        // Put it into the cache
        #[cfg(not(feature = "sync"))]
        self.cache.borrow_mut().insert(file_path, m.clone());
        #[cfg(feature = "sync")]
        self.cache.write().unwrap().insert(file_path, m.clone());

        Ok(m)
    }

    fn resolve_ast(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Result<Option<crate::AST>, Box<EvalAltResult>> {
        // Construct the script file path
        let mut file_path = self.base_path.clone();
        file_path.push(path);
        file_path.set_extension(&self.extension); // Force extension

        // Load the script file and compile it
        let mut ast = engine
            .compile_file(file_path.clone())
            .map_err(|err| match *err {
                EvalAltResult::ErrorSystem(_, err) if err.is::<IoError>() => {
                    Box::new(EvalAltResult::ErrorModuleNotFound(path.to_string(), pos))
                }
                _ => Box::new(EvalAltResult::ErrorInModule(path.to_string(), err, pos)),
            })?;

        ast.set_source(path);

        Ok(Some(ast))
    }
}
