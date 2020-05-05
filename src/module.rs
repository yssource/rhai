//! Module defining external-loaded modules for Rhai.

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::{Engine, FnAny, FnCallArgs, FunctionsLib};
use crate::parser::FnDef;
use crate::result::EvalAltResult;
use crate::scope::{EntryType as ScopeEntryType, Scope};
use crate::token::Position;
use crate::token::Token;
use crate::utils::StaticVec;

use crate::stdlib::{
    any::TypeId, collections::HashMap, fmt, iter::empty, mem, rc::Rc, string::String, sync::Arc,
};

/// A trait that encapsulates a module resolution service.
pub trait ModuleResolver {
    /// Resolve a module based on a path string.
    fn resolve(&self, engine: &Engine, path: &str) -> Result<Module, Box<EvalAltResult>>;
}

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
    pub fn new() -> Self {
        Default::default()
    }

    /// Does a variable exist in the module?
    pub fn contains_var(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Get the value of a module variable.
    pub fn get_var_value<T: Variant + Clone>(&self, name: &str) -> Option<T> {
        self.get_var(name).and_then(|v| v.try_cast::<T>())
    }

    /// Get a module variable.
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
    pub fn contains_sub_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    /// Get a sub-module.
    pub fn get_sub_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }

    /// Get a mutable reference to a sub-module.
    pub fn get_sub_module_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.modules.get_mut(name)
    }

    /// Set a sub-module into the module.
    ///
    /// If there is an existing sub-module of the same name, it is replaced.
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
    pub fn set_fn_0<T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn() -> Result<T, Box<EvalAltResult>> + 'static,
        #[cfg(feature = "sync")] func: impl Fn() -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |_: &mut FnCallArgs, _: Position| func().map(|v| v.into());
        self.set_fn(fn_name, &[], Box::new(f))
    }

    /// Set a Rust function taking one parameter into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    pub fn set_fn_1<A: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(A) -> Result<T, Box<EvalAltResult>> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A) -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, _: Position| {
            func(mem::take(args[0]).cast::<A>()).map(|v| v.into())
        };
        self.set_fn(fn_name, &[TypeId::of::<A>()], Box::new(f))
    }

    /// Set a Rust function taking one mutable parameter into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    pub fn set_fn_1_mut<A: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A) -> Result<T, Box<EvalAltResult>> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A) -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, _: Position| {
            func(args[0].downcast_mut::<A>().unwrap()).map(|v| v.into())
        };
        self.set_fn(fn_name, &[TypeId::of::<A>()], Box::new(f))
    }

    /// Set a Rust function taking two parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    pub fn set_fn_2<A: Variant + Clone, B: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(A, B) -> Result<T, Box<EvalAltResult>> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A, B) -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, _: Position| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();

            func(a, b).map(|v| v.into())
        };
        self.set_fn(
            fn_name,
            &[TypeId::of::<A>(), TypeId::of::<B>()],
            Box::new(f),
        )
    }

    /// Set a Rust function taking two parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    pub fn set_fn_2_mut<A: Variant + Clone, B: Variant + Clone, T: Into<Dynamic>>(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B) -> Result<T, Box<EvalAltResult>>
            + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A, B) -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, _: Position| {
            let b = mem::take(args[1]).cast::<B>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b).map(|v| v.into())
        };
        self.set_fn(
            fn_name,
            &[TypeId::of::<A>(), TypeId::of::<B>()],
            Box::new(f),
        )
    }

    /// Set a Rust function taking three parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    pub fn set_fn_3<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Into<Dynamic>,
    >(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(A, B, C) -> Result<T, Box<EvalAltResult>> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A, B, C) -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, _: Position| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();

            func(a, b, c).map(|v| v.into())
        };
        self.set_fn(
            fn_name,
            &[TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()],
            Box::new(f),
        )
    }

    /// Set a Rust function taking three parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    pub fn set_fn_3_mut<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Into<Dynamic>,
    >(
        &mut self,
        fn_name: &str,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B, C) -> Result<T, Box<EvalAltResult>>
            + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A, B, C) -> Result<T, Box<EvalAltResult>>
            + Send
            + Sync
            + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs, _: Position| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c).map(|v| v.into())
        };
        self.set_fn(
            fn_name,
            &[TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()],
            Box::new(f),
        )
    }

    /// Get a Rust function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
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

    /// Get a script-defined function.
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
}

pub mod resolvers {
    use super::*;

    #[cfg(not(feature = "no_std"))]
    use crate::stdlib::path::PathBuf;

    /// A module resolution service that loads module script files (assumed `.rhai` extension).
    #[cfg(not(feature = "no_std"))]
    #[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub struct FileModuleResolver(PathBuf);

    #[cfg(not(feature = "no_std"))]
    impl FileModuleResolver {
        /// Create a new `FileModuleResolver` with a specific base path.
        pub fn new_with_path(path: PathBuf) -> Self {
            Self(path)
        }
        /// Create a new `FileModuleResolver` with the current directory as base path.
        pub fn new() -> Self {
            Default::default()
        }
    }

    #[cfg(not(feature = "no_std"))]
    impl Default for FileModuleResolver {
        fn default() -> Self {
            Self::new_with_path(".".into())
        }
    }

    #[cfg(not(feature = "no_std"))]
    impl ModuleResolver for FileModuleResolver {
        fn resolve(&self, engine: &Engine, path: &str) -> Result<Module, Box<EvalAltResult>> {
            // Load the script file (attaching `.rhai`)
            let mut file_path = self.0.clone();
            file_path.push(path);
            file_path.set_extension("rhai");

            // Compile it
            let ast = engine.compile_file(file_path)?;

            // Use new scope
            let mut scope = Scope::new();

            // Run the script
            engine.eval_ast_with_scope_raw(&mut scope, &ast)?;

            // Create new module
            let mut module = Module::new();

            // Variables left in the scope become module variables
            for entry in scope.into_iter() {
                match entry.typ {
                    ScopeEntryType::Normal | ScopeEntryType::Constant => {
                        module
                            .variables
                            .insert(entry.name.into_owned(), entry.value);
                    }
                    ScopeEntryType::Module => {
                        module
                            .modules
                            .insert(entry.name.into_owned(), entry.value.cast::<Module>());
                    }
                }
            }

            module.fn_lib = FunctionsLib::new().merge(ast.fn_lib());

            Ok(module)
        }
    }

    /// A module resolution service that serves modules added into it.
    #[derive(Debug, Clone, Default)]
    pub struct StaticModuleResolver(HashMap<String, Module>);

    impl StaticModuleResolver {
        /// Create a new `StaticModuleResolver`.
        pub fn new() -> Self {
            Default::default()
        }
        /// Add a named module.
        pub fn add_module(&mut self, name: &str, module: Module) {
            self.0.insert(name.to_string(), module);
        }
    }

    impl ModuleResolver for StaticModuleResolver {
        fn resolve(&self, _: &Engine, path: &str) -> Result<Module, Box<EvalAltResult>> {
            self.0.get(path).cloned().ok_or_else(|| {
                Box::new(EvalAltResult::ErrorModuleNotFound(
                    path.to_string(),
                    Position::none(),
                ))
            })
        }
    }

    /// A module resolution service that always returns a not-found error.
    #[derive(Debug, Clone, PartialEq, Eq, Copy, Default)]
    pub struct NullModuleResolver;

    impl NullModuleResolver {
        /// Create a new `NullModuleResolver`.
        pub fn new() -> Self {
            Default::default()
        }
    }

    impl ModuleResolver for NullModuleResolver {
        fn resolve(&self, _: &Engine, path: &str) -> Result<Module, Box<EvalAltResult>> {
            Err(Box::new(EvalAltResult::ErrorModuleNotFound(
                path.to_string(),
                Position::none(),
            )))
        }
    }
}
