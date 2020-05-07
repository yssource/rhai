//! Module defining external-loaded modules for Rhai.
#![cfg(not(feature = "no_module"))]

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::{Engine, FnAny, FnCallArgs, FunctionsLib};
use crate::parser::{FnDef, AST};
use crate::result::EvalAltResult;
use crate::scope::{Entry as ScopeEntry, EntryType as ScopeEntryType, Scope};
use crate::token::Position;
use crate::token::Token;
use crate::utils::StaticVec;

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::HashMap,
    fmt, mem,
    ops::{Deref, DerefMut},
    rc::Rc,
    string::{String, ToString},
    sync::Arc,
};

/// A trait that encapsulates a module resolution service.
pub trait ModuleResolver {
    /// Resolve a module based on a path string.
    fn resolve(
        &self,
        engine: &Engine,
        path: &str,
        pos: Position,
    ) -> Result<Module, Box<EvalAltResult>>;
}

/// Return type of module-level Rust function.
type FuncReturn<T> = Result<T, Box<EvalAltResult>>;

/// An imported module, which may contain variables, sub-modules,
/// external Rust functions, and script-defined functions.
///
/// Not available under the `no_module` feature.
#[derive(Default, Clone)]
pub struct Module {
    /// Sub-modules.
    modules: HashMap<String, Module>,
    /// Module variables, including sub-modules.
    variables: HashMap<String, Dynamic>,

    /// External Rust functions.
    #[cfg(not(feature = "sync"))]
    functions: HashMap<u64, Rc<Box<FnAny>>>,
    /// External Rust functions.
    #[cfg(feature = "sync")]
    functions: HashMap<u64, Arc<Box<FnAny>>>,

    /// Script-defined functions.
    fn_lib: FunctionsLib,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<module {:?}, functions={}, lib={}>",
            self.variables,
            self.functions.len(),
            self.fn_lib.len()
        )
    }
}

impl Module {
    /// Create a new module.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var_value::<i64>("answer").unwrap(), 42);
    /// ```
    pub fn new() -> Self {
        Default::default()
    }

    /// Does a variable exist in the module?
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert!(module.contains_var("answer"));
    /// ```
    pub fn contains_var(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Get the value of a module variable.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var_value::<i64>("answer").unwrap(), 42);
    /// ```
    pub fn get_var_value<T: Variant + Clone>(&self, name: &str) -> Option<T> {
        self.get_var(name).and_then(|v| v.try_cast::<T>())
    }

    /// Get a module variable as a `Dynamic`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var("answer").unwrap().cast::<i64>(), 42);
    /// ```
    pub fn get_var(&self, name: &str) -> Option<Dynamic> {
        self.variables.get(name).cloned()
    }

    /// Get a mutable reference to a module variable.
    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut Dynamic> {
        self.variables.get_mut(name)
    }

    /// Set a variable into the module.
    ///
    /// If there is an existing variable of the same name, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// module.set_var("answer", 42_i64);
    /// assert_eq!(module.get_var_value::<i64>("answer").unwrap(), 42);
    /// ```
    pub fn set_var<K: Into<String>, T: Into<Dynamic>>(&mut self, name: K, value: T) {
        self.variables.insert(name.into(), value.into());
    }

    /// Get a mutable reference to a modules-qualified variable.
    pub(crate) fn get_qualified_var_mut(
        &mut self,
        name: &str,
        modules: &StaticVec<(String, Position)>,
        pos: Position,
    ) -> Result<&mut Dynamic, Box<EvalAltResult>> {
        Ok(self
            .get_qualified_module_mut(modules)?
            .get_var_mut(name)
            .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(name.into(), pos)))?)
    }

    /// Does a sub-module exist in the module?
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let sub_module = Module::new();
    /// module.set_sub_module("question", sub_module);
    /// assert!(module.contains_sub_module("question"));
    /// ```
    pub fn contains_sub_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    /// Get a sub-module.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let sub_module = Module::new();
    /// module.set_sub_module("question", sub_module);
    /// assert!(module.get_sub_module("question").is_some());
    /// ```
    pub fn get_sub_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }

    /// Get a mutable reference to a sub-module.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let sub_module = Module::new();
    /// module.set_sub_module("question", sub_module);
    /// assert!(module.get_sub_module_mut("question").is_some());
    /// ```
    pub fn get_sub_module_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.modules.get_mut(name)
    }

    /// Set a sub-module into the module.
    ///
    /// If there is an existing sub-module of the same name, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let sub_module = Module::new();
    /// module.set_sub_module("question", sub_module);
    /// assert!(module.get_sub_module("question").is_some());
    /// ```
    pub fn set_sub_module<K: Into<String>>(&mut self, name: K, sub_module: Module) {
        self.modules.insert(name.into(), sub_module.into());
    }

    /// Get a mutable reference to a modules chain.
    /// The first module is always skipped and assumed to be the same as `self`.
    pub(crate) fn get_qualified_module_mut(
        &mut self,
        modules: &StaticVec<(String, Position)>,
    ) -> Result<&mut Module, Box<EvalAltResult>> {
        let mut drain = modules.iter();
        drain.next().unwrap(); // Skip first module

        let mut module = self;

        for (id, id_pos) in drain {
            module = module
                .get_sub_module_mut(id)
                .ok_or_else(|| Box::new(EvalAltResult::ErrorModuleNotFound(id.into(), *id_pos)))?;
        }

        Ok(module)
    }

    /// Does the particular Rust function exist in the module?
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_0("calc", || Ok(42_i64));
    /// assert!(module.contains_fn(hash));
    /// ```
    pub fn contains_fn(&self, hash: u64) -> bool {
        self.functions.contains_key(&hash)
    }

    /// Set a Rust function into the module, returning a hash key.
    ///
    /// If there is an existing Rust function of the same hash, it is replaced.
    pub fn set_fn(&mut self, fn_name: &str, params: &[TypeId], func: Box<FnAny>) -> u64 {
        let hash = calc_fn_hash(fn_name, params.iter().cloned());

        #[cfg(not(feature = "sync"))]
        self.functions.insert(hash, Rc::new(func));
        #[cfg(feature = "sync")]
        self.functions.insert(hash, Arc::new(func));

        hash
    }

    /// Set a Rust function taking no parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_0("calc", || Ok(42_i64));
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_0<T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn() -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn() -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |_: &mut FnCallArgs, pos| {
            func()
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Set a Rust function taking one parameter into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_1("calc", |x: i64| Ok(x + 1));
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_1<A: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(A) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, pos| {
            func(mem::take(args[0]).cast::<A>())
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[TypeId::of::<A>()];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Set a Rust function taking one mutable parameter into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_1_mut("calc", |x: &mut i64| { *x += 1; Ok(*x) });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_1_mut<A: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, pos| {
            func(args[0].downcast_mut::<A>().unwrap())
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[TypeId::of::<A>()];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Set a Rust function taking two parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_2("calc", |x: i64, y: String| {
    ///     Ok(x + y.len() as i64)
    /// });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_2<A: Variant + Clone, B: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(A, B) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A, B) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, pos| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();

            func(a, b)
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Set a Rust function taking two parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_2_mut("calc", |x: &mut i64, y: String| {
    ///     *x += y.len() as i64; Ok(*x)
    /// });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_2_mut<A: Variant + Clone, B: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A, B) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, pos| {
            let b = mem::take(args[1]).cast::<B>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b)
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Set a Rust function taking three parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_3("calc", |x: i64, y: String, z: i64| {
    ///     Ok(x + y.len() as i64 + z)
    /// });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_3<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Into<Dynamic>,
    >(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(A, B, C) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A, B, C) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, pos| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();

            func(a, b, c)
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Set a Rust function taking three parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_3_mut("calc", |x: &mut i64, y: String, z: i64| {
    ///     *x += y.len() as i64 + z; Ok(*x)
    /// });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_3_mut<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Into<Dynamic>,
    >(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B, C) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A, B, C) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, pos| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c)
                .map(|v| v.into())
                .map_err(|err| EvalAltResult::set_position(err, pos))
        };
        let arg_types = &[TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(fn_name, arg_types, Box::new(f))
    }

    /// Get a Rust function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_1("calc", |x: i64| Ok(x + 1));
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn get_fn(&self, hash: u64) -> Option<&Box<FnAny>> {
        self.functions.get(&hash).map(|v| v.as_ref())
    }

    /// Get a modules-qualified function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
    pub(crate) fn get_qualified_fn(
        &mut self,
        name: &str,
        hash: u64,
        modules: &StaticVec<(String, Position)>,
        pos: Position,
    ) -> Result<&Box<FnAny>, Box<EvalAltResult>> {
        Ok(self
            .get_qualified_module_mut(modules)?
            .get_fn(hash)
            .ok_or_else(|| {
                let mut fn_name: String = Default::default();

                modules.iter().for_each(|(n, _)| {
                    fn_name.push_str(n);
                    fn_name.push_str(Token::DoubleColon.syntax().as_ref());
                });

                fn_name.push_str(name);

                Box::new(EvalAltResult::ErrorFunctionNotFound(fn_name, pos))
            })?)
    }

    /// Get the script-defined functions.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// assert_eq!(module.get_fn_lib().len(), 0);
    /// ```
    pub fn get_fn_lib(&self) -> &FunctionsLib {
        &self.fn_lib
    }

    /// Get a modules-qualified functions library.
    pub(crate) fn get_qualified_fn_lib(
        &mut self,
        name: &str,
        args: usize,
        modules: &StaticVec<(String, Position)>,
    ) -> Result<Option<&FnDef>, Box<EvalAltResult>> {
        Ok(self
            .get_qualified_module_mut(modules)?
            .fn_lib
            .get_function(name, args))
    }

    /// Create a new `Module` by evaluating an `AST`.
    ///
    /// # Examples
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Module};
    ///
    /// let engine = Engine::new();
    /// let ast = engine.compile("let answer = 42;")?;
    /// let module = Module::eval_ast_as_new(&ast, &engine)?;
    /// assert!(module.contains_var("answer"));
    /// assert_eq!(module.get_var_value::<i64>("answer").unwrap(), 42);
    /// # Ok(())
    /// # }
    /// ```
    pub fn eval_ast_as_new(ast: &AST, engine: &Engine) -> FuncReturn<Self> {
        // Use new scope
        let mut scope = Scope::new();

        // Run the script
        engine.eval_ast_with_scope_raw(&mut scope, &ast)?;

        // Create new module
        let mut module = Module::new();

        scope.into_iter().for_each(
            |ScopeEntry {
                 name, typ, value, ..
             }| {
                match typ {
                    // Variables left in the scope become module variables
                    ScopeEntryType::Normal | ScopeEntryType::Constant => {
                        module.variables.insert(name.into_owned(), value);
                    }
                    // Modules left in the scope become sub-modules
                    ScopeEntryType::Module => {
                        module
                            .modules
                            .insert(name.into_owned(), value.cast::<Module>());
                    }
                }
            },
        );

        module.fn_lib = module.fn_lib.merge(ast.fn_lib());

        Ok(module)
    }
}

/// Re-export module resolvers.
pub mod resolvers {
    #[cfg(not(feature = "no_std"))]
    pub use super::file::FileModuleResolver;
    pub use super::stat::StaticModuleResolver;
}

/// Script file-based module resolver.
#[cfg(not(feature = "no_std"))]
mod file {
    use super::*;
    use crate::stdlib::path::PathBuf;

    /// A module resolution service that loads module script files from the file system.
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
    /// engine.set_module_resolver(Some(resolver));
    /// ```
    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
    pub struct FileModuleResolver {
        path: PathBuf,
        extension: String,
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

            // Compile it
            let ast = engine
                .compile_file(file_path)
                .map_err(|err| EvalAltResult::set_position(err, pos))?;

            Module::eval_ast_as_new(&ast, engine)
                .map_err(|err| EvalAltResult::set_position(err, pos))
        }
    }
}

/// Static module resolver.
mod stat {
    use super::*;

    /// A module resolution service that serves modules added into it.
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
        /// resolver.insert("hello".to_string(), module);
        ///
        /// let mut engine = Engine::new();
        /// engine.set_module_resolver(Some(resolver));
        /// ```
        pub fn new() -> Self {
            Default::default()
        }
    }

    impl Deref for StaticModuleResolver {
        type Target = HashMap<String, Module>;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl DerefMut for StaticModuleResolver {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.0
        }
    }

    impl ModuleResolver for StaticModuleResolver {
        fn resolve(
            &self,
            _: &Engine,
            path: &str,
            pos: Position,
        ) -> Result<Module, Box<EvalAltResult>> {
            self.0
                .get(path)
                .cloned()
                .ok_or_else(|| Box::new(EvalAltResult::ErrorModuleNotFound(path.to_string(), pos)))
        }
    }
}
