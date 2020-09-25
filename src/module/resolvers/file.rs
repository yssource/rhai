use crate::any::Dynamic;
use crate::engine::Engine;
use crate::module::{Module, ModuleResolver};
use crate::parser::{FnAccess, AST};
use crate::result::EvalAltResult;
use crate::scope::Scope;
use crate::token::Position;

use crate::stdlib::{
    boxed::Box,
    collections::HashMap,
    path::PathBuf,
    string::{String, ToString},
};

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

        // See if it is cached
        let exists = {
            #[cfg(not(feature = "sync"))]
            let c = self.cache.borrow();
            #[cfg(feature = "sync")]
            let c = self.cache.read().unwrap();

            c.contains_key(&file_path)
        };

        if !exists {
            // Load the file and compile it if not found
            let ast = engine
                .compile_file(file_path.clone())
                .map_err(|err| err.new_position(pos))?;

            // Put it into the cache
            #[cfg(not(feature = "sync"))]
            self.cache.borrow_mut().insert(file_path.clone(), ast);
            #[cfg(feature = "sync")]
            self.cache.write().unwrap().insert(file_path.clone(), ast);
        }

        #[cfg(not(feature = "sync"))]
        let c = self.cache.borrow();
        #[cfg(feature = "sync")]
        let c = self.cache.read().unwrap();

        let ast = c.get(&file_path).unwrap();

        let mut _module = Module::eval_ast_as_new(Scope::new(), ast, engine)?;

        #[cfg(not(feature = "no_function"))]
        ast.iter_functions(|access, name, num_args| match access {
            FnAccess::Private => (),
            FnAccess::Public => {
                let fn_name = name.to_string();
                let ast_lib = ast.lib().clone();

                _module.set_raw_fn_as_scripted(
                    name,
                    num_args,
                    move |engine: &Engine, _, args: &mut [&mut Dynamic]| {
                        engine.call_fn_dynamic_raw(
                            &mut Scope::new(),
                            &ast_lib,
                            &fn_name,
                            &mut None,
                            args,
                        )
                    },
                );
            }
        });

        Ok(_module)
    }
}
