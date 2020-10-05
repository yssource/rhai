//! Module defining external-loaded modules for Rhai.

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::Engine;
use crate::fn_native::{CallableFunction, FnCallArgs, IteratorFn, SendSync};
use crate::fn_register::by_value as cast_arg;
use crate::parser::FnAccess;
use crate::result::EvalAltResult;
use crate::token::{Position, Token};
use crate::utils::{ImmutableString, StaticVec, StraightHasherBuilder};

#[cfg(not(feature = "no_function"))]
use crate::{fn_native::Shared, parser::ScriptFnDef};

#[cfg(not(feature = "no_module"))]
use crate::{
    engine::Imports,
    parser::AST,
    scope::{Entry as ScopeEntry, Scope},
};

#[cfg(not(feature = "no_index"))]
use crate::engine::{Array, FN_IDX_GET, FN_IDX_SET};

#[cfg(not(feature = "no_object"))]
use crate::engine::{make_getter, make_setter, Map};

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::HashMap,
    fmt, format,
    iter::empty,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    string::{String, ToString},
    vec::Vec,
};

/// Return type of module-level Rust function.
pub type FuncReturn<T> = Result<T, Box<EvalAltResult>>;

pub type FuncInfo = (
    String,
    FnAccess,
    usize,
    Option<StaticVec<TypeId>>,
    CallableFunction,
);

/// An imported module, which may contain variables, sub-modules,
/// external Rust functions, and script-defined functions.
///
/// Not available under the `no_module` feature.
#[derive(Default)]
pub struct Module {
    /// Sub-modules.
    modules: HashMap<String, Module>,

    /// Module variables.
    variables: HashMap<String, Dynamic>,

    /// Flattened collection of all module variables, including those in sub-modules.
    all_variables: HashMap<u64, Dynamic, StraightHasherBuilder>,

    /// External Rust functions.
    functions: HashMap<u64, FuncInfo, StraightHasherBuilder>,

    /// Iterator functions, keyed by the type producing the iterator.
    type_iterators: HashMap<TypeId, IteratorFn>,

    /// Flattened collection of all external Rust functions, native or scripted,
    /// including those in sub-modules.
    all_functions: HashMap<u64, CallableFunction, StraightHasherBuilder>,

    /// Is the module indexed?
    indexed: bool,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Module(\n    modules: {}\n    vars: {}\n    functions: {}\n)",
            self.modules
                .keys()
                .map(String::as_str)
                .collect::<Vec<_>>()
                .join(", "),
            self.variables
                .iter()
                .map(|(k, v)| format!("{}={:?}", k, v))
                .collect::<Vec<_>>()
                .join(", "),
            self.functions
                .values()
                .map(|(_, _, _, _, f)| f.to_string())
                .collect::<Vec<_>>()
                .join(", "),
        )
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        // Only clone the index at the top level
        Self {
            all_variables: self.all_variables.clone(),
            all_functions: self.all_functions.clone(),
            indexed: self.indexed,
            ..self.do_clone(false)
        }
    }
}

impl AsRef<Module> for Module {
    fn as_ref(&self) -> &Module {
        self
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
            functions: HashMap::with_capacity_and_hasher(capacity, StraightHasherBuilder),
            ..Default::default()
        }
    }

    /// Is the module empty?
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let module = Module::new();
    /// assert!(module.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
            && self.all_functions.is_empty()
            && self.variables.is_empty()
            && self.all_variables.is_empty()
            && self.modules.is_empty()
            && self.type_iterators.is_empty()
    }

    /// Clone the module, optionally skipping the index.
    fn do_clone(&self, clone_index: bool) -> Self {
        Self {
            modules: if clone_index {
                self.modules.clone()
            } else {
                self.modules
                    .iter()
                    .map(|(k, m)| (k.clone(), m.do_clone(clone_index)))
                    .collect()
            },
            variables: self.variables.clone(),
            functions: self.functions.clone(),
            type_iterators: self.type_iterators.clone(),
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
    pub fn set_var(&mut self, name: impl Into<String>, value: impl Variant + Clone) -> &mut Self {
        self.variables.insert(name.into(), Dynamic::from(value));
        self.indexed = false;
        self
    }

    /// Get a mutable reference to a modules-qualified variable.
    /// Name and Position in `EvalAltResult` are None and must be set afterwards.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    pub(crate) fn get_qualified_var_mut(
        &mut self,
        hash_var: u64,
    ) -> Result<&mut Dynamic, Box<EvalAltResult>> {
        if hash_var == 0 {
            Err(EvalAltResult::ErrorVariableNotFound(String::new(), Position::none()).into())
        } else {
            self.all_variables.get_mut(&hash_var).ok_or_else(|| {
                EvalAltResult::ErrorVariableNotFound(String::new(), Position::none()).into()
            })
        }
    }

    /// Set a script-defined function into the module.
    ///
    /// If there is an existing function of the same name and number of arguments, it is replaced.
    #[cfg(not(feature = "no_function"))]
    pub(crate) fn set_script_fn(&mut self, fn_def: ScriptFnDef) -> u64 {
        // None + function name + number of arguments.
        let num_params = fn_def.params.len();
        let hash_script = calc_fn_hash(empty(), &fn_def.name, num_params, empty());
        self.functions.insert(
            hash_script,
            (
                fn_def.name.to_string(),
                fn_def.access,
                num_params,
                None,
                fn_def.into(),
            ),
        );
        self.indexed = false;
        hash_script
    }

    /// Get a script-defined function in the module based on name and number of parameters.
    #[cfg(not(feature = "no_function"))]
    pub fn get_script_fn(
        &self,
        name: &str,
        num_params: usize,
        public_only: bool,
    ) -> Option<&Shared<ScriptFnDef>> {
        self.functions
            .values()
            .find(|(fn_name, access, num, _, _)| {
                (!public_only || *access == FnAccess::Public)
                    && *num == num_params
                    && fn_name == name
            })
            .map(|(_, _, _, _, f)| f.get_shared_fn_def())
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
    pub fn set_sub_module(&mut self, name: impl Into<String>, sub_module: Module) -> &mut Self {
        self.modules.insert(name.into(), sub_module.into());
        self.indexed = false;
        self
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
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn contains_fn(&self, hash_fn: u64, public_only: bool) -> bool {
        if hash_fn == 0 {
            false
        } else if public_only {
            self.functions
                .get(&hash_fn)
                .map(|(_, access, _, _, _)| access.is_public())
                .unwrap_or(false)
        } else {
            self.functions.contains_key(&hash_fn)
        }
    }

    /// Set a Rust function into the module, returning a hash key.
    ///
    /// If there is an existing Rust function of the same hash, it is replaced.
    ///
    /// ## WARNING - Low Level API
    ///
    /// This function is very low level.
    pub fn set_fn(
        &mut self,
        name: impl Into<String>,
        access: FnAccess,
        arg_types: &[TypeId],
        func: CallableFunction,
    ) -> u64 {
        let name = name.into();

        let args_len = if arg_types.is_empty() {
            // Distinguish between a script function and a function with no parameters
            usize::MAX
        } else {
            arg_types.len()
        };

        let params = arg_types
            .into_iter()
            .cloned()
            .map(|id| {
                if id == TypeId::of::<&str>() {
                    TypeId::of::<ImmutableString>()
                } else if id == TypeId::of::<String>() {
                    TypeId::of::<ImmutableString>()
                } else {
                    id
                }
            })
            .collect();

        let hash_fn = calc_fn_hash(empty(), &name, args_len, arg_types.iter().cloned());

        self.functions
            .insert(hash_fn, (name, access, args_len, Some(params), func.into()));

        self.indexed = false;

        hash_fn
    }

    /// Set a Rust function taking a reference to the scripting `Engine`, the current set of functions,
    /// plus a list of mutable `Dynamic` references into the module, returning a hash key.
    ///
    /// Use this to register a built-in function which must reference settings on the scripting
    /// `Engine` (e.g. to prevent growing an array beyond the allowed maximum size), or to call a
    /// script-defined function in the current evaluation context.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// ## WARNING - Low Level API
    ///
    /// This function is very low level.
    ///
    /// A list of `TypeId`'s is taken as the argument types.
    ///
    /// Arguments are simply passed in as a mutable array of `&mut Dynamic`,
    /// which is guaranteed to contain enough arguments of the correct types.
    ///
    /// The function is assumed to be a _method_, meaning that the first argument should not be consumed.
    /// All other arguments can be consumed.
    ///
    /// To access a primary parameter value (i.e. cloning is cheap), use: `args[n].clone().cast::<T>()`
    ///
    /// To access a parameter value and avoid cloning, use `std::mem::take(args[n]).cast::<T>()`.
    /// Notice that this will _consume_ the argument, replacing it with `()`.
    ///
    /// To access the first mutable parameter, use `args.get_mut(0).unwrap()`
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_raw_fn("double_or_not",
    ///                 // Pass parameter types via a slice with TypeId's
    ///                 &[std::any::TypeId::of::<i64>(), std::any::TypeId::of::<bool>()],
    ///                 // Fixed closure signature
    ///                 |engine, lib, args| {
    ///                     // 'args' is guaranteed to be the right length and of the correct types
    ///
    ///                     // Get the second parameter by 'consuming' it
    ///                     let double = std::mem::take(args[1]).cast::<bool>();
    ///                     // Since it is a primary type, it can also be cheaply copied
    ///                     let double = args[1].clone().cast::<bool>();
    ///                     // Get a mutable reference to the first argument.
    ///                     let mut x = args[0].write_lock::<i64>().unwrap();
    ///
    ///                     let orig = *x;
    ///
    ///                     if double {
    ///                         *x *= 2;            // the first argument can be mutated
    ///                     }
    ///
    ///                     Ok(orig)                // return Result<T, Box<EvalAltResult>>
    ///                 });
    ///
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_raw_fn<T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        arg_types: &[TypeId],
        func: impl Fn(&Engine, &Module, &mut [&mut Dynamic]) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |engine: &Engine, lib: &Module, args: &mut FnCallArgs| {
            func(engine, lib, args).map(Dynamic::from)
        };
        self.set_fn(
            name,
            FnAccess::Public,
            arg_types,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Set a raw function but with a signature that is a scripted function (meaning that the types
    /// are not determined), but the implementation is in Rust.
    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_module"))]
    pub(crate) fn set_raw_fn_as_scripted(
        &mut self,
        name: impl Into<String>,
        num_params: usize,
        func: impl Fn(&Engine, &Module, &mut [&mut Dynamic]) -> FuncReturn<Dynamic> + SendSync + 'static,
    ) -> u64 {
        // None + function name + number of arguments.
        let name = name.into();
        let hash_script = calc_fn_hash(empty(), &name, num_params, empty());
        let f = move |engine: &Engine, lib: &Module, args: &mut FnCallArgs| func(engine, lib, args);
        self.functions.insert(
            hash_script,
            (
                name,
                FnAccess::Public,
                num_params,
                None,
                CallableFunction::from_pure(Box::new(f)),
            ),
        );
        self.indexed = false;
        hash_script
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
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_0<T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn() -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, _: &mut FnCallArgs| func().map(Dynamic::from);
        let arg_types = [];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_pure(Box::new(f)),
        )
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
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_1<A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(A) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            func(cast_arg::<A>(&mut args[0])).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>()];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_pure(Box::new(f)),
        )
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
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_1_mut<A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            func(&mut args[0].write_lock::<A>().unwrap()).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>()];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Set a Rust getter function taking one mutable parameter, returning a hash key.
    ///
    /// If there is a similar existing Rust getter function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::Module;
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_getter_fn("value", |x: &mut i64| { Ok(*x) });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    #[cfg(not(feature = "no_object"))]
    pub fn set_getter_fn<A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        self.set_fn_1_mut(make_getter(&name.into()), func)
    }

    /// Set a Rust function taking two parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_2("calc", |x: i64, y: ImmutableString| {
    ///     Ok(x + y.len() as i64)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_2<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(A, B) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let a = cast_arg::<A>(&mut args[0]);
            let b = cast_arg::<B>(&mut args[1]);

            func(a, b).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_pure(Box::new(f)),
        )
    }

    /// Set a Rust function taking two parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_2_mut("calc", |x: &mut i64, y: ImmutableString| {
    ///     *x += y.len() as i64; Ok(*x)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_2_mut<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A, B) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let b = cast_arg::<B>(&mut args[1]);
            let a = &mut args[0].write_lock::<A>().unwrap();

            func(a, b).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Set a Rust setter function taking two parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing setter Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_setter_fn("value", |x: &mut i64, y: ImmutableString| {
    ///     *x = y.len() as i64;
    ///     Ok(())
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    #[cfg(not(feature = "no_object"))]
    pub fn set_setter_fn<A: Variant + Clone, B: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A, B) -> FuncReturn<()> + SendSync + 'static,
    ) -> u64 {
        self.set_fn_2_mut(make_setter(&name.into()), func)
    }

    /// Set a Rust index getter taking two parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing setter Rust function, it is replaced.
    ///
    /// # Panics
    ///
    /// Panics if the type is `Array` or `Map`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_indexer_get_fn(|x: &mut i64, y: ImmutableString| {
    ///     Ok(*x + y.len() as i64)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    #[cfg(not(feature = "no_index"))]
    pub fn set_indexer_get_fn<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        func: impl Fn(&mut A, B) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        if TypeId::of::<A>() == TypeId::of::<Array>() {
            panic!("Cannot register indexer for arrays.");
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<A>() == TypeId::of::<Map>() {
            panic!("Cannot register indexer for object maps.");
        }
        if TypeId::of::<A>() == TypeId::of::<String>()
            || TypeId::of::<A>() == TypeId::of::<&str>()
            || TypeId::of::<A>() == TypeId::of::<ImmutableString>()
        {
            panic!("Cannot register indexer for strings.");
        }

        self.set_fn_2_mut(FN_IDX_GET, func)
    }

    /// Set a Rust function taking three parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_3("calc", |x: i64, y: ImmutableString, z: i64| {
    ///     Ok(x + y.len() as i64 + z)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_3<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(A, B, C) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let a = cast_arg::<A>(&mut args[0]);
            let b = cast_arg::<B>(&mut args[1]);
            let c = cast_arg::<C>(&mut args[2]);

            func(a, b, c).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_pure(Box::new(f)),
        )
    }

    /// Set a Rust function taking three parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_3_mut("calc", |x: &mut i64, y: ImmutableString, z: i64| {
    ///     *x += y.len() as i64 + z; Ok(*x)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_3_mut<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A, B, C) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let b = cast_arg::<B>(&mut args[2]);
            let c = cast_arg::<C>(&mut args[3]);
            let a = &mut args[0].write_lock::<A>().unwrap();

            func(a, b, c).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Set a Rust index setter taking three parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Panics
    ///
    /// Panics if the type is `Array` or `Map`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_indexer_set_fn(|x: &mut i64, y: ImmutableString, value: i64| {
    ///     *x = y.len() as i64 + value;
    ///     Ok(())
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    #[cfg(not(feature = "no_index"))]
    pub fn set_indexer_set_fn<A: Variant + Clone, B: Variant + Clone, C: Variant + Clone>(
        &mut self,
        func: impl Fn(&mut A, B, C) -> FuncReturn<()> + SendSync + 'static,
    ) -> u64 {
        if TypeId::of::<A>() == TypeId::of::<Array>() {
            panic!("Cannot register indexer for arrays.");
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<A>() == TypeId::of::<Map>() {
            panic!("Cannot register indexer for object maps.");
        }
        if TypeId::of::<A>() == TypeId::of::<String>()
            || TypeId::of::<A>() == TypeId::of::<&str>()
            || TypeId::of::<A>() == TypeId::of::<ImmutableString>()
        {
            panic!("Cannot register indexer for strings.");
        }

        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let b = cast_arg::<B>(&mut args[1]);
            let c = cast_arg::<C>(&mut args[2]);
            let a = &mut args[0].write_lock::<A>().unwrap();

            func(a, b, c).map(Dynamic::from)
        };
        let arg_types = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(
            FN_IDX_SET,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Set a pair of Rust index getter and setter functions, returning both hash keys.
    /// This is a short-hand for `set_indexer_get_fn` and `set_indexer_set_fn`.
    ///
    /// If there are similar existing Rust functions, they are replaced.
    ///
    /// # Panics
    ///
    /// Panics if the type is `Array` or `Map`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let (hash_get, hash_set) = module.set_indexer_get_set_fn(
    ///     |x: &mut i64, y: ImmutableString| {
    ///         Ok(*x + y.len() as i64)
    ///     },
    ///     |x: &mut i64, y: ImmutableString, value: i64| {
    ///         *x = y.len() as i64 + value;
    ///         Ok(())
    ///     }
    /// );
    /// assert!(module.contains_fn(hash_get, true));
    /// assert!(module.contains_fn(hash_set, true));
    /// ```
    #[cfg(not(feature = "no_index"))]
    pub fn set_indexer_get_set_fn<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        getter: impl Fn(&mut A, B) -> FuncReturn<T> + SendSync + 'static,
        setter: impl Fn(&mut A, B, T) -> FuncReturn<()> + SendSync + 'static,
    ) -> (u64, u64) {
        (
            self.set_indexer_get_fn(getter),
            self.set_indexer_set_fn(setter),
        )
    }

    /// Set a Rust function taking four parameters into the module, returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_4("calc", |x: i64, y: ImmutableString, z: i64, _w: ()| {
    ///     Ok(x + y.len() as i64 + z)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_4<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        D: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(A, B, C, D) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let a = cast_arg::<A>(&mut args[0]);
            let b = cast_arg::<B>(&mut args[1]);
            let c = cast_arg::<C>(&mut args[2]);
            let d = cast_arg::<D>(&mut args[3]);

            func(a, b, c, d).map(Dynamic::from)
        };
        let arg_types = [
            TypeId::of::<A>(),
            TypeId::of::<B>(),
            TypeId::of::<C>(),
            TypeId::of::<D>(),
        ];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_pure(Box::new(f)),
        )
    }

    /// Set a Rust function taking four parameters (the first one mutable) into the module,
    /// returning a hash key.
    ///
    /// If there is a similar existing Rust function, it is replaced.
    ///
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_fn_4_mut("calc", |x: &mut i64, y: ImmutableString, z: i64, _w: ()| {
    ///     *x += y.len() as i64 + z; Ok(*x)
    /// });
    /// assert!(module.contains_fn(hash, true));
    /// ```
    pub fn set_fn_4_mut<
        A: Variant + Clone,
        B: Variant + Clone,
        C: Variant + Clone,
        D: Variant + Clone,
        T: Variant + Clone,
    >(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A, B, C, D) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &Engine, _: &Module, args: &mut FnCallArgs| {
            let b = cast_arg::<B>(&mut args[1]);
            let c = cast_arg::<C>(&mut args[2]);
            let d = cast_arg::<D>(&mut args[3]);
            let a = &mut args[0].write_lock::<A>().unwrap();

            func(a, b, c, d).map(Dynamic::from)
        };
        let arg_types = [
            TypeId::of::<A>(),
            TypeId::of::<B>(),
            TypeId::of::<C>(),
            TypeId::of::<D>(),
        ];
        self.set_fn(
            name,
            FnAccess::Public,
            &arg_types,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Get a Rust function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
    pub(crate) fn get_fn(&self, hash_fn: u64, public_only: bool) -> Option<&CallableFunction> {
        if hash_fn == 0 {
            None
        } else {
            self.functions
                .get(&hash_fn)
                .and_then(|(_, access, _, _, f)| match access {
                    _ if !public_only => Some(f),
                    FnAccess::Public => Some(f),
                    FnAccess::Private => None,
                })
        }
    }

    /// Get a modules-qualified function.
    /// Name and Position in `EvalAltResult` are None and must be set afterwards.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash` and must match
    /// the hash calculated by `index_all_sub_modules`.
    pub(crate) fn get_qualified_fn(&self, hash_qualified_fn: u64) -> Option<&CallableFunction> {
        self.all_functions.get(&hash_qualified_fn)
    }

    /// Combine another module into this module.
    /// The other module is consumed to merge into this module.
    pub fn combine(&mut self, other: Self) -> &mut Self {
        if !other.modules.is_empty() {
            self.modules.extend(other.modules.into_iter());
        }
        if !other.variables.is_empty() {
            self.variables.extend(other.variables.into_iter());
        }
        if !other.functions.is_empty() {
            self.functions.extend(other.functions.into_iter());
        }
        if !other.type_iterators.is_empty() {
            self.type_iterators.extend(other.type_iterators.into_iter());
        }
        self.all_functions.clear();
        self.all_variables.clear();
        self.indexed = false;
        self
    }

    /// Combine another module into this module.
    /// The other module is consumed to merge into this module.
    /// Sub-modules are flattened onto the root module, with higher level overriding lower level.
    pub fn combine_flatten(&mut self, other: Self) -> &mut Self {
        if !other.modules.is_empty() {
            other.modules.into_iter().for_each(|(_, m)| {
                self.combine_flatten(m);
            });
        }
        if !other.variables.is_empty() {
            self.variables.extend(other.variables.into_iter());
        }
        if !other.functions.is_empty() {
            self.functions.extend(other.functions.into_iter());
        }
        if !other.type_iterators.is_empty() {
            self.type_iterators.extend(other.type_iterators.into_iter());
        }
        self.all_functions.clear();
        self.all_variables.clear();
        self.indexed = false;
        self
    }

    /// Merge another module into this module.
    pub fn merge(&mut self, other: &Self) -> &mut Self {
        self.merge_filtered(other, &mut |_, _, _| true)
    }

    /// Merge another module into this module, with only selected script-defined functions based on a filter predicate.
    pub(crate) fn merge_filtered(
        &mut self,
        other: &Self,
        mut _filter: &mut impl FnMut(FnAccess, &str, usize) -> bool,
    ) -> &mut Self {
        #[cfg(not(feature = "no_function"))]
        if !other.modules.is_empty() {
            other.modules.iter().for_each(|(k, v)| {
                let mut m = Self::new();
                m.merge_filtered(v, _filter);
                self.modules.insert(k.clone(), m);
            });
        }
        #[cfg(feature = "no_function")]
        if !other.modules.is_empty() {
            self.modules
                .extend(other.modules.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        if !other.variables.is_empty() {
            self.variables
                .extend(other.variables.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
        if !other.functions.is_empty() {
            self.functions.extend(
                other
                    .functions
                    .iter()
                    .filter(|(_, (_, _, _, _, v))| match v {
                        #[cfg(not(feature = "no_function"))]
                        CallableFunction::Script(f) => {
                            _filter(f.access, f.name.as_str(), f.params.len())
                        }
                        _ => true,
                    })
                    .map(|(&k, v)| (k, v.clone())),
            );
        }

        if !other.type_iterators.is_empty() {
            self.type_iterators
                .extend(other.type_iterators.iter().map(|(&k, v)| (k, v.clone())));
        }
        self.all_functions.clear();
        self.all_variables.clear();
        self.indexed = false;
        self
    }

    /// Filter out the functions, retaining only some based on a filter predicate.
    #[cfg(not(feature = "no_function"))]
    pub(crate) fn retain_functions(
        &mut self,
        mut filter: impl FnMut(FnAccess, &str, usize) -> bool,
    ) -> &mut Self {
        self.functions.retain(|_, (_, _, _, _, v)| match v {
            CallableFunction::Script(f) => filter(f.access, f.name.as_str(), f.params.len()),
            _ => true,
        });

        self.all_functions.clear();
        self.all_variables.clear();
        self.indexed = false;
        self
    }

    /// Get the number of variables in the module.
    pub fn num_var(&self) -> usize {
        self.variables.len()
    }
    /// Get the number of functions in the module.
    pub fn num_fn(&self) -> usize {
        self.variables.len()
    }
    /// Get the number of type iterators in the module.
    pub fn num_iter(&self) -> usize {
        self.variables.len()
    }

    /// Get an iterator to the variables in the module.
    pub fn iter_var(&self) -> impl Iterator<Item = (&String, &Dynamic)> {
        self.variables.iter()
    }

    /// Get an iterator to the functions in the module.
    pub(crate) fn iter_fn(&self) -> impl Iterator<Item = &FuncInfo> {
        self.functions.values()
    }

    /// Get an iterator over all script-defined functions in the module.
    #[cfg(not(feature = "no_function"))]
    pub fn iter_script_fn<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FnAccess, &str, usize, Shared<ScriptFnDef>)> + 'a {
        self.functions
            .values()
            .map(|(_, _, _, _, f)| f)
            .filter(|f| f.is_script())
            .map(CallableFunction::get_shared_fn_def)
            .map(|f| {
                let func = f.clone();
                (f.access, f.name.as_str(), f.params.len(), func)
            })
    }

    /// Create a new `Module` by evaluating an `AST`.
    ///
    /// The entire `AST` is encapsulated into each function, allowing functions
    /// to cross-call each other.  Functions in the global namespace, plus all functions
    /// defined in the module, are _merged_ into a _unified_ namespace before each call.
    /// Therefore, all functions will be found.
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
        let mut mods = Imports::new();

        // Run the script
        engine.eval_ast_with_scope_raw(&mut scope, &mut mods, &ast)?;

        // Create new module
        let mut module = Module::new();

        scope
            .into_iter()
            .for_each(|ScopeEntry { value, alias, .. }| {
                // Variables with an alias left in the scope become module variables
                if let Some(alias) = alias {
                    module.variables.insert(*alias, value);
                }
            });

        // Modules left in the scope become sub-modules
        mods.into_iter().for_each(|(alias, m)| {
            module.modules.insert(alias.to_string(), m);
        });

        #[cfg(not(feature = "no_function"))]
        {
            let ast_lib: Shared<Module> = ast.lib().clone().into();

            ast.iter_functions()
                .filter(|(access, _, _, _)| access.is_public())
                .for_each(|(_, name, num_params, func)| {
                    let ast_lib = ast_lib.clone();

                    module.set_raw_fn_as_scripted(
                        name,
                        num_params,
                        move |engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]| {
                            let mut lib_merged;

                            let unified_lib = if lib.is_empty() {
                                // In the special case of the main script not defining any function
                                &ast_lib
                            } else {
                                lib_merged = lib.clone();
                                lib_merged.merge(&ast_lib);
                                &lib_merged
                            };

                            engine
                                .call_script_fn(
                                    &mut Default::default(),
                                    &mut Default::default(),
                                    &mut Default::default(),
                                    unified_lib,
                                    &mut None,
                                    &func.name,
                                    func.as_ref(),
                                    args,
                                    0,
                                )
                                .map_err(|err| {
                                    // Wrap the error in a module-error
                                    Box::new(EvalAltResult::ErrorInModule(
                                        "".to_string(),
                                        err,
                                        Position::none(),
                                    ))
                                })
                        },
                    );
                });
        }

        Ok(module)
    }

    /// Scan through all the sub-modules in the module build an index of all
    /// variables and external Rust functions via hashing.
    #[cfg(not(feature = "no_module"))]
    pub(crate) fn index_all_sub_modules(&mut self) {
        // Collect a particular module.
        fn index_module<'a>(
            module: &'a Module,
            qualifiers: &mut Vec<&'a str>,
            variables: &mut Vec<(u64, Dynamic)>,
            functions: &mut Vec<(u64, CallableFunction)>,
        ) {
            module.modules.iter().for_each(|(name, m)| {
                // Index all the sub-modules first.
                qualifiers.push(name);
                index_module(m, qualifiers, variables, functions);
                qualifiers.pop();
            });

            // Index all variables
            module.variables.iter().for_each(|(var_name, value)| {
                // Qualifiers + variable name
                let hash_var = calc_fn_hash(qualifiers.iter().map(|&v| v), var_name, 0, empty());
                variables.push((hash_var, value.clone()));
            });
            // Index all Rust functions
            module
                .functions
                .iter()
                .filter(|(_, (_, access, _, _, _))| access.is_public())
                .for_each(|(&_hash, (name, _, _num_params, params, func))| {
                    if let Some(params) = params {
                        // Qualified Rust functions are indexed in two steps:
                        // 1) Calculate a hash in a similar manner to script-defined functions,
                        //    i.e. qualifiers + function name + number of arguments.
                        let hash_qualified_script =
                            calc_fn_hash(qualifiers.iter().cloned(), name, params.len(), empty());
                        // 2) Calculate a second hash with no qualifiers, empty function name,
                        //    zero number of arguments, and the actual list of argument `TypeId`'.s
                        let hash_fn_args = calc_fn_hash(empty(), "", 0, params.iter().cloned());
                        // 3) The final hash is the XOR of the two hashes.
                        let hash_qualified_fn = hash_qualified_script ^ hash_fn_args;

                        functions.push((hash_qualified_fn, func.clone()));
                    } else if cfg!(not(feature = "no_function")) {
                        let hash_qualified_script = if qualifiers.is_empty() {
                            _hash
                        } else {
                            // Qualifiers + function name + number of arguments.
                            calc_fn_hash(
                                qualifiers.iter().map(|&v| v),
                                &name,
                                *_num_params,
                                empty(),
                            )
                        };
                        functions.push((hash_qualified_script, func.clone()));
                    }
                });
        }

        if !self.indexed {
            let mut qualifiers: Vec<_> = Default::default();
            let mut variables: Vec<_> = Default::default();
            let mut functions: Vec<_> = Default::default();

            qualifiers.push("root");

            index_module(self, &mut qualifiers, &mut variables, &mut functions);

            self.all_variables = variables.into_iter().collect();
            self.all_functions = functions.into_iter().collect();
            self.indexed = true;
        }
    }

    /// Does a type iterator exist in the module?
    pub fn contains_iter(&self, id: TypeId) -> bool {
        self.type_iterators.contains_key(&id)
    }

    /// Set a type iterator into the module.
    pub fn set_iter(&mut self, typ: TypeId, func: IteratorFn) -> &mut Self {
        self.type_iterators.insert(typ, func);
        self.indexed = false;
        self
    }

    /// Get the specified type iterator.
    pub(crate) fn get_iter(&self, id: TypeId) -> Option<IteratorFn> {
        self.type_iterators.get(&id).cloned()
    }
}

/// _[INTERNALS]_ A chain of module names to qualify a variable or function call.
/// Exported under the `internals` feature only.
///
/// A `u64` hash key is cached for quick search purposes.
///
/// A `StaticVec` is used because most module-level access contains only one level,
/// and it is wasteful to always allocate a `Vec` with one element.
///
/// ## WARNING
///
/// This type is volatile and may change.
#[derive(Clone, Eq, PartialEq, Default, Hash)]
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

/// Re-export module resolver trait.
#[cfg(not(feature = "no_module"))]
pub use resolvers::ModuleResolver;

/// Re-export module resolvers.
#[cfg(not(feature = "no_module"))]
pub mod resolvers;
