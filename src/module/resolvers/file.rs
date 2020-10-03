use crate::engine::Engine;
use crate::module::{Module, ModuleResolver};
use crate::parser::AST;
use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{boxed::Box, collections::HashMap, path::PathBuf, string::String};

#[cfg(not(feature = "sync"))]
use crate::stdlib::cell::RefCell;

#[cfg(feature = "sync")]
use crate::stdlib::sync::RwLock;

/// Module resolution service that loads module script files from the file system.
///
/// Script files are cached so they are are not reloaded and recompiled in subsequent requests.
///
/// The `new_with_path` and `new_with_path_and_extension` constructor functions
/// allow specification of a base directory with module path used as a relative path offset
/// to the base directory. The script file is then forced to be in a specified extension
/// (default `.rhai`).
///
/// # Examples
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
/// engine.set_module_resolver(Some(resolver));
/// ```
#[derive(Debug)]
pub struct FileModuleResolver {
    path: PathBuf,
    extension: String,

    #[cfg(not(feature = "sync"))]
    cache: RefCell<HashMap<PathBuf, AST>>,

    #[cfg(feature = "sync")]
    cache: RwLock<HashMap<PathBuf, AST>>,
}

impl Default for FileModuleResolver {
    fn default() -> Self {
        Self::new_with_path(PathBuf::default())
    }
}

impl FileModuleResolver {
    /// Create a new `FileModuleResolver` with a specific base path.
    ///
    /// # Examples
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
    /// engine.set_module_resolver(Some(resolver));
    /// ```
    pub fn new_with_path<P: Into<PathBuf>>(path: P) -> Self {
        Self::new_with_path_and_extension(path, "rhai")
    }

    /// Create a new `FileModuleResolver` with a specific base path and file extension.
    ///
    /// The default extension is `.rhai`.
    ///
    /// # Examples
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
    /// engine.set_module_resolver(Some(resolver));
    /// ```
    pub fn new_with_path_and_extension<P: Into<PathBuf>, E: Into<String>>(
        path: P,
        extension: E,
    ) -> Self {
        Self {
            path: path.into(),
            extension: extension.into(),
            cache: Default::default(),
        }
    }

    /// Create a new `FileModuleResolver` with the current directory as base path.
    ///
    /// # Examples
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
    /// engine.set_module_resolver(Some(resolver));
    /// ```
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a `Module` from a file path.
    pub fn create_module<P: Into<PathBuf>>(
        &self,
        engine: &Engine,
        path: &str,
    ) -> Result<Module, Box<EvalAltResult>> {
        self.resolve(engine, path, Default::default())
    }
}

impl ModuleResolver for FileModuleResolver {
    fn resolve(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Result<Module, Box<EvalAltResult>> {
        // Construct the script file path
        let mut file_path = self.path.clone();
        file_path.push(path);
        file_path.set_extension(&self.extension); // Force extension

        let scope = Default::default();
        let module;

        // See if it is cached
        let ast = {
            #[cfg(not(feature = "sync"))]
            let c = self.cache.borrow();
            #[cfg(feature = "sync")]
            let c = self.cache.read().unwrap();

            if let Some(ast) = c.get(&file_path) {
                module = Module::eval_ast_as_new(scope, ast, engine).map_err(|err| {
                    Box::new(EvalAltResult::ErrorInModule(path.to_string(), err, pos))
                })?;
                None
            } else {
                // Load the file and compile it if not found
                let ast = engine.compile_file(file_path.clone()).map_err(|err| {
                    Box::new(EvalAltResult::ErrorInModule(path.to_string(), err, pos))
                })?;

                module = Module::eval_ast_as_new(scope, &ast, engine).map_err(|err| {
                    Box::new(EvalAltResult::ErrorInModule(path.to_string(), err, pos))
                })?;
                Some(ast)
            }
        };

        if let Some(ast) = ast {
            // Put it into the cache
            #[cfg(not(feature = "sync"))]
            self.cache.borrow_mut().insert(file_path, ast);
            #[cfg(feature = "sync")]
            self.cache.write().unwrap().insert(file_path, ast);
        }

        Ok(module)
    }
}
