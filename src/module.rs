//! Module defining external-loaded modules for Rhai.

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::{Engine, FunctionsLib};
use crate::fn_native::{
    FnAny, FnCallArgs, IteratorFn, NativeCallable, NativeFunction, NativeFunctionABI,
    NativeFunctionABI::*, SharedIteratorFunction, SharedNativeFunction,
};
use crate::parser::{FnAccess, FnDef, SharedFnDef, AST};
use crate::result::EvalAltResult;
use crate::scope::{Entry as ScopeEntry, EntryType as ScopeEntryType, Scope};
use crate::token::{Position, Token};
use crate::utils::{StaticVec, EMPTY_TYPE_ID};

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::HashMap,
    fmt,
    iter::{empty, repeat},
    mem,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    rc::Rc,
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};

/// Default function access mode.
const DEF_ACCESS: FnAccess = FnAccess::Public;

/// Return type of module-level Rust function.
pub type FuncReturn<T> = Result<T, Box<EvalAltResult>>;

/// An imported module, which may contain variables, sub-modules,
/// external Rust functions, and script-defined functions.
///
/// Not available under the `no_module` feature.
#[derive(Clone, Default)]
pub struct Module {
    /// Sub-modules.
    modules: HashMap<String, Module>,

    /// Module variables.
    variables: HashMap<String, Dynamic>,

    /// Flattened collection of all module variables, including those in sub-modules.
    all_variables: HashMap<u64, Dynamic>,

    /// External Rust functions.
    functions: HashMap<u64, (String, FnAccess, StaticVec<TypeId>, SharedNativeFunction)>,

    /// Flattened collection of all external Rust functions, including those in sub-modules.
    all_functions: HashMap<u64, SharedNativeFunction>,

    /// Script-defined functions.
    fn_lib: FunctionsLib,

    /// Flattened collection of all script-defined functions, including those in sub-modules.
    all_fn_lib: FunctionsLib,

    /// Iterator functions, keyed by the type producing the iterator.
    type_iterators: HashMap<TypeId, SharedIteratorFunction>,
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

    /// Create a new module with a specified capacity for native Rust functions.
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
    pub fn new_with_capacity(capacity: usize) -> Self {
        Self {
            functions: HashMap::with_capacity(capacity),
            ..Default::default()
        }
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
        self.get_var(name).and_then(Dynamic::try_cast::<T>)
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
    pub fn set_var<K: Into<String>, T: Variant + Clone>(&mut self, name: K, value: T) {
        self.variables.insert(name.into(), Dynamic::from(value));
    }

    /// Get a mutable reference to a modules-qualified variable.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    pub(crate) fn get_qualified_var_mut(
        &mut self,
        name: &str,
        hash_var: u64,
        pos: Position,
    ) -> Result<&mut Dynamic, Box<EvalAltResult>> {
        self.all_variables
            .get_mut(&hash_var)
            .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(name.to_string(), pos)))
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
    pub fn contains_fn(&self, hash_fn: u64) -> bool {
        self.functions.contains_key(&hash_fn)
    }

    /// Set a Rust function into the module, returning a hash key.
    ///
    /// If there is an existing Rust function of the same hash, it is replaced.
    pub fn set_fn(
        &mut self,
        name: String,
        abi: NativeFunctionABI,
        access: FnAccess,
        params: &[TypeId],
        func: Box<FnAny>,
    ) -> u64 {
        let hash_fn = calc_fn_hash(empty(), &name, params.iter().cloned());

        let f = Box::new(NativeFunction::from((func, abi))) as Box<dyn NativeCallable>;

        #[cfg(not(feature = "sync"))]
        let func = Rc::new(f);
        #[cfg(feature = "sync")]
        let func = Arc::new(f);

        let params = params.into_iter().cloned().collect();

        self.functions.insert(hash_fn, (name, access, params, func));

        hash_fn
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
    pub fn set_fn_0<K: Into<String>, T: Variant + Clone>(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn() -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn() -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |_: &mut FnCallArgs| func().map(Dynamic::from);
        let arg_types = [];
        self.set_fn(name.into(), Pure, DEF_ACCESS, &arg_types, Box::new(f))
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
    pub fn set_fn_1<K: Into<String>, A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn(A) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f =
            move |args: &mut FnCallArgs| func(mem::take(args[0]).cast::<A>()).map(Dynamic::from);
        let arg_types = [TypeId::of::<A>()];
        self.set_fn(name.into(), Pure, DEF_ACCESS, &arg_types, Box::new(f))
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
    pub fn set_fn_1_mut<K: Into<String>, A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            func(args[0].downcast_mut::<A>().unwrap()).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>()];
        self.set_fn(name.into(), Method, DEF_ACCESS, &arg_types, Box::new(f))
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
    pub fn set_fn_2<K: Into<String>, A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn(A, B) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A, B) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();

            func(a, b).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(name.into(), Pure, DEF_ACCESS, &arg_types, Box::new(f))
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
    pub fn set_fn_2_mut<
        K: Into<String>,
        A: Variant + Clone,
        B: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A, B) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let b = mem::take(args[1]).cast::<B>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(name.into(), Method, DEF_ACCESS, &arg_types, Box::new(f))
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
        K: Into<String>,
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn(A, B, C) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(A, B, C) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();

            func(a, b, c).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(name.into(), Pure, DEF_ACCESS, &arg_types, Box::new(f))
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
        K: Into<String>,
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: K,
        #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B, C) -> FuncReturn<T> + 'static,
        #[cfg(feature = "sync")] func: impl Fn(&mut A, B, C) -> FuncReturn<T> + Send + Sync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(name.into(), Method, DEF_ACCESS, &arg_types, Box::new(f))
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
    pub fn get_fn(&self, hash_fn: u64) -> Option<&Box<dyn NativeCallable>> {
        self.functions.get(&hash_fn).map(|(_, _, _, v)| v.as_ref())
    }

    /// Get a modules-qualified function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
    pub(crate) fn get_qualified_fn(
        &mut self,
        name: &str,
        hash_fn_native: u64,
    ) -> Result<&Box<dyn NativeCallable>, Box<EvalAltResult>> {
        self.all_functions
            .get(&hash_fn_native)
            .map(|f| f.as_ref())
            .ok_or_else(|| {
                Box::new(EvalAltResult::ErrorFunctionNotFound(
                    name.to_string(),
                    Position::none(),
                ))
            })
    }

    /// Get a modules-qualified script-defined functions.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    pub(crate) fn get_qualified_scripted_fn(&mut self, hash_fn_def: u64) -> Option<&FnDef> {
        self.all_fn_lib.get_function(hash_fn_def)
    }

    /// Create a new `Module` by evaluating an `AST`.
    ///
    /// # Examples
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Module, Scope};
    ///
    /// let engine = Engine::new();
    /// let ast = engine.compile("let answer = 42; export answer;")?;
    /// let module = Module::eval_ast_as_new(Scope::new(), &ast, &engine)?;
    /// assert!(module.contains_var("answer"));
    /// assert_eq!(module.get_var_value::<i64>("answer").unwrap(), 42);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_module"))]
    pub fn eval_ast_as_new(mut scope: Scope, ast: &AST, engine: &Engine) -> FuncReturn<Self> {
        // Run the script
        engine.eval_ast_with_scope_raw(&mut scope, &ast)?;

        // Create new module
        let mut module = Module::new();

        scope.into_iter().for_each(
            |ScopeEntry {
                 typ, value, alias, ..
             }| {
                match typ {
                    // Variables with an alias left in the scope become module variables
                    ScopeEntryType::Normal | ScopeEntryType::Constant if alias.is_some() => {
                        module.variables.insert(*alias.unwrap(), value);
                    }
                    // Modules left in the scope become sub-modules
                    ScopeEntryType::Module if alias.is_some() => {
                        module
                            .modules
                            .insert(*alias.unwrap(), value.cast::<Module>());
                    }
                    // Variables and modules with no alias are private and not exported
                    _ => (),
                }
            },
        );

        module.fn_lib = module.fn_lib.merge(ast.fn_lib());

        Ok(module)
    }

    /// Scan through all the sub-modules in the `Module` build an index of all
    /// variables and external Rust functions via hashing.
    pub(crate) fn index_all_sub_modules(&mut self) {
        // Collect a particular module.
        fn index_module<'a>(
            module: &'a Module,
            qualifiers: &mut Vec<&'a str>,
            variables: &mut Vec<(u64, Dynamic)>,
            functions: &mut Vec<(u64, SharedNativeFunction)>,
            fn_lib: &mut Vec<(u64, SharedFnDef)>,
        ) {
            for (name, m) in &module.modules {
                // Index all the sub-modules first.
                qualifiers.push(name);
                index_module(m, qualifiers, variables, functions, fn_lib);
                qualifiers.pop();
            }

            // Index all variables
            for (var_name, value) in &module.variables {
                // Qualifiers + variable name
                let hash_var = calc_fn_hash(qualifiers.iter().map(|&v| v), var_name, empty());
                variables.push((hash_var, value.clone()));
            }
            // Index all Rust functions
            for (name, access, params, func) in module.functions.values() {
                match access {
                    // Private functions are not exported
                    FnAccess::Private => continue,
                    FnAccess::Public => (),
                }
                // Rust functions are indexed in two steps:
                // 1) Calculate a hash in a similar manner to script-defined functions,
                //    i.e. qualifiers + function name + dummy parameter types (one for each parameter).
                let hash_fn_def = calc_fn_hash(
                    qualifiers.iter().map(|&v| v),
                    name,
                    repeat(EMPTY_TYPE_ID()).take(params.len()),
                );
                // 2) Calculate a second hash with no qualifiers, empty function name, and
                //    the actual list of parameter `TypeId`'.s
                let hash_fn_args = calc_fn_hash(empty(), "", params.iter().cloned());
                // 3) The final hash is the XOR of the two hashes.
                let hash_fn_native = hash_fn_def ^ hash_fn_args;

                functions.push((hash_fn_native, func.clone()));
            }
            // Index all script-defined functions
            for fn_def in module.fn_lib.values() {
                match fn_def.access {
                    // Private functions are not exported
                    FnAccess::Private => continue,
                    DEF_ACCESS => (),
                }
                // Qualifiers + function name + placeholders (one for each parameter)
                let hash_fn_def = calc_fn_hash(
                    qualifiers.iter().map(|&v| v),
                    &fn_def.name,
                    repeat(EMPTY_TYPE_ID()).take(fn_def.params.len()),
                );
                fn_lib.push((hash_fn_def, fn_def.clone()));
            }
        }

        let mut variables = Vec::new();
        let mut functions = Vec::new();
        let mut fn_lib = Vec::new();

        index_module(
            self,
            &mut vec!["root"],
            &mut variables,
            &mut functions,
            &mut fn_lib,
        );

        self.all_variables = variables.into_iter().collect();
        self.all_functions = functions.into_iter().collect();
        self.all_fn_lib = fn_lib.into();
    }

    /// Does a type iterator exist in the module?
    pub fn contains_iter(&self, id: TypeId) -> bool {
        self.type_iterators.contains_key(&id)
    }

    /// Set a type iterator into the module.
    pub fn set_iter(&mut self, typ: TypeId, func: Box<IteratorFn>) {
        #[cfg(not(feature = "sync"))]
        self.type_iterators.insert(typ, Rc::new(func));
        #[cfg(feature = "sync")]
        self.type_iterators.insert(typ, Arc::new(func));
    }

    /// Get the specified type iterator.
    pub fn get_iter(&self, id: TypeId) -> Option<&SharedIteratorFunction> {
        self.type_iterators.get(&id)
    }
}

/// A chain of module names to qualify a variable or function call.
/// A `u64` hash key is kept for quick search purposes.
///
/// A `StaticVec` is used because most module-level access contains only one level,
/// and it is wasteful to always allocate a `Vec` with one element.
#[derive(Clone, Eq, PartialEq, Default)]
pub struct ModuleRef(StaticVec<(String, Position)>, Option<NonZeroUsize>);

impl fmt::Debug for ModuleRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)?;

        if let Some(index) = self.1 {
            write!(f, " -> {}", index)
        } else {
            Ok(())
        }
    }
}

impl Deref for ModuleRef {
    type Target = StaticVec<(String, Position)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ModuleRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Display for ModuleRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (m, _) in self.0.iter() {
            write!(f, "{}{}", m, Token::DoubleColon.syntax())?;
        }
        Ok(())
    }
}

impl From<StaticVec<(String, Position)>> for ModuleRef {
    fn from(modules: StaticVec<(String, Position)>) -> Self {
        Self(modules, None)
    }
}

impl ModuleRef {
    pub(crate) fn index(&self) -> Option<NonZeroUsize> {
        self.1
    }
    pub(crate) fn set_index(&mut self, index: Option<NonZeroUsize>) {
        self.1 = index
    }
}

/// Trait that encapsulates a module resolution service.
#[cfg(not(feature = "no_module"))]
#[cfg(not(feature = "sync"))]
pub trait ModuleResolver {
    /// Resolve a module based on a path string.
    fn resolve(
        &self,
        engine: &Engine,
        scope: Scope,
        path: &str,
        pos: Position,
    ) -> Result<Module, Box<EvalAltResult>>;
}

/// Trait that encapsulates a module resolution service.
#[cfg(not(feature = "no_module"))]
#[cfg(feature = "sync")]
pub trait ModuleResolver: Send + Sync {
    /// Resolve a module based on a path string.
    fn resolve(
        &self,
        engine: &Engine,
        scope: Scope,
        path: &str,
        pos: Position,
    ) -> Result<Module, Box<EvalAltResult>>;
}

/// Re-export module resolvers.
#[cfg(not(feature = "no_module"))]
pub mod resolvers {
    #[cfg(not(feature = "no_std"))]
    pub use super::file::FileModuleResolver;
    pub use super::stat::StaticModuleResolver;
}

/// Script file-based module resolver.
#[cfg(not(feature = "no_module"))]
#[cfg(not(feature = "no_std"))]
mod file {
    use super::*;
    use crate::stdlib::path::PathBuf;

    /// Module resolution service that loads module script files from the file system.
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
    #[derive(Debug, Eq, PartialEq, PartialOrd, Ord, Clone, Hash)]
    pub struct FileModuleResolver {
        path: PathBuf,
        extension: String,
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
            scope: Scope,
            path: &str,
        ) -> Result<Module, Box<EvalAltResult>> {
            self.resolve(engine, scope, path, Default::default())
        }
    }

    impl ModuleResolver for FileModuleResolver {
        fn resolve(
            &self,
            engine: &Engine,
            scope: Scope,
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
                .map_err(|err| err.new_position(pos))?;

            Module::eval_ast_as_new(scope, &ast, engine).map_err(|err| err.new_position(pos))
        }
    }
}

/// Static module resolver.
#[cfg(not(feature = "no_module"))]
mod stat {
    use super::*;

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
            _: Scope,
            path: &str,
            pos: Position,
        ) -> Result<Module, Box<EvalAltResult>> {
            self.0
                .get(path)
                .cloned()
                .ok_or_else(|| Box::new(EvalAltResult::ErrorModuleNotFound(path.into(), pos)))
        }
    }
}
