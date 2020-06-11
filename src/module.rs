//! Module defining external-loaded modules for Rhai.

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::{make_getter, make_setter, Engine, FUNC_INDEXER_GET, FUNC_INDEXER_SET};
use crate::fn_native::{CallableFunction, FnCallArgs, IteratorFn, SendSync};
use crate::parser::{
    FnAccess,
    FnAccess::{Private, Public},
    ScriptFnDef, AST,
};
use crate::result::EvalAltResult;
use crate::scope::{Entry as ScopeEntry, EntryType as ScopeEntryType, Scope};
use crate::token::{Position, Token};
use crate::utils::{StaticVec, StraightHasherBuilder};

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::HashMap,
    fmt,
    iter::empty,
    mem,
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    string::{String, ToString},
    vec,
    vec::Vec,
};

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
    all_variables: HashMap<u64, Dynamic, StraightHasherBuilder>,

    /// External Rust functions.
    functions: HashMap<
        u64,
        (String, FnAccess, StaticVec<TypeId>, CallableFunction),
        StraightHasherBuilder,
    >,

    /// Iterator functions, keyed by the type producing the iterator.
    type_iterators: HashMap<TypeId, IteratorFn>,

    /// Flattened collection of all external Rust functions, native or scripted,
    /// including those in sub-modules.
    all_functions: HashMap<u64, CallableFunction, StraightHasherBuilder>,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<module {:?}, functions={}>",
            self.variables,
            self.functions.len(),
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
            functions: HashMap::with_capacity_and_hasher(capacity, StraightHasherBuilder),
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
    pub fn set_var(&mut self, name: impl Into<String>, value: impl Variant + Clone) {
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

    /// Set a script-defined function into the module.
    ///
    /// If there is an existing function of the same name and number of arguments, it is replaced.
    pub(crate) fn set_script_fn(&mut self, fn_def: ScriptFnDef) {
        // None + function name + number of arguments.
        let hash_script = calc_fn_hash(empty(), &fn_def.name, fn_def.params.len(), empty());
        self.functions.insert(
            hash_script,
            (
                fn_def.name.to_string(),
                fn_def.access,
                Default::default(),
                fn_def.into(),
            ),
        );
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
    pub fn set_sub_module(&mut self, name: impl Into<String>, sub_module: Module) {
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
        name: impl Into<String>,
        access: FnAccess,
        params: &[TypeId],
        func: CallableFunction,
    ) -> u64 {
        let name = name.into();

        let hash_fn = calc_fn_hash(empty(), &name, params.len(), params.iter().cloned());

        let params = params.into_iter().cloned().collect();

        self.functions
            .insert(hash_fn, (name, access, params, func.into()));

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
    pub fn set_fn_0<T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn() -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |_: &mut FnCallArgs| func().map(Dynamic::from);
        let args = [];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_1<A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(A) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f =
            move |args: &mut FnCallArgs| func(mem::take(args[0]).cast::<A>()).map(Dynamic::from);
        let args = [TypeId::of::<A>()];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_1_mut<A: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            func(args[0].downcast_mut::<A>().unwrap()).map(Dynamic::from)
        };
        let args = [TypeId::of::<A>()];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
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
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_2<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(A, B) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();

            func(a, b).map(Dynamic::from)
        };
        let args = [TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_fn_2_mut<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        name: impl Into<String>,
        func: impl Fn(&mut A, B) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let b = mem::take(args[1]).cast::<B>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b).map(Dynamic::from)
        };
        let args = [TypeId::of::<A>(), TypeId::of::<B>()];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
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
    /// # Examples
    ///
    /// ```
    /// use rhai::{Module, ImmutableString};
    ///
    /// let mut module = Module::new();
    /// let hash = module.set_indexer_get_fn(|x: &mut i64, y: ImmutableString| {
    ///     Ok(*x + y.len() as i64)
    /// });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[cfg(not(feature = "no_index"))]
    pub fn set_indexer_get_fn<A: Variant + Clone, B: Variant + Clone, T: Variant + Clone>(
        &mut self,
        func: impl Fn(&mut A, B) -> FuncReturn<T> + SendSync + 'static,
    ) -> u64 {
        self.set_fn_2_mut(FUNC_INDEXER_GET, func)
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
    /// assert!(module.get_fn(hash).is_some());
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
        let f = move |args: &mut FnCallArgs| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();

            func(a, b, c).map(Dynamic::from)
        };
        let args = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
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
        let f = move |args: &mut FnCallArgs| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c).map(Dynamic::from)
        };
        let args = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()];
        self.set_fn(
            name,
            Public,
            &args,
            CallableFunction::from_method(Box::new(f)),
        )
    }

    /// Set a Rust index setter taking three parameters (the first one mutable) into the module,
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
    /// let hash = module.set_indexer_set_fn(|x: &mut i64, y: ImmutableString, value: i64| {
    ///     *x = y.len() as i64 + value;
    ///     Ok(())
    /// });
    /// assert!(module.get_fn(hash).is_some());
    /// ```
    pub fn set_indexer_set_fn<A: Variant + Clone, B: Variant + Clone>(
        &mut self,
        func: impl Fn(&mut A, B, A) -> FuncReturn<()> + SendSync + 'static,
    ) -> u64 {
        let f = move |args: &mut FnCallArgs| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<A>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c).map(Dynamic::from)
        };
        let args = [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<A>()];
        self.set_fn(
            FUNC_INDEXER_SET,
            Public,
            &args,
            CallableFunction::from_method(Box::new(f)),
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
    /// assert!(module.get_fn(hash).is_some());
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
        let f = move |args: &mut FnCallArgs| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let d = mem::take(args[3]).cast::<D>();

            func(a, b, c, d).map(Dynamic::from)
        };
        let args = [
            TypeId::of::<A>(),
            TypeId::of::<B>(),
            TypeId::of::<C>(),
            TypeId::of::<D>(),
        ];
        self.set_fn(
            name,
            Public,
            &args,
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
    /// assert!(module.get_fn(hash).is_some());
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
        let f = move |args: &mut FnCallArgs| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let d = mem::take(args[3]).cast::<D>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c, d).map(Dynamic::from)
        };
        let args = [
            TypeId::of::<A>(),
            TypeId::of::<B>(),
            TypeId::of::<C>(),
            TypeId::of::<C>(),
        ];
        self.set_fn(
            name,
            Public,
            &args,
            CallableFunction::from_method(Box::new(f)),
        )
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
    pub fn get_fn(&self, hash_fn: u64) -> Option<&CallableFunction> {
        self.functions.get(&hash_fn).map(|(_, _, _, v)| v)
    }

    /// Get a modules-qualified function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash` and must match
    /// the hash calculated by `index_all_sub_modules`.
    pub(crate) fn get_qualified_fn(
        &mut self,
        name: &str,
        hash_qualified_fn: u64,
    ) -> Result<&CallableFunction, Box<EvalAltResult>> {
        self.all_functions.get(&hash_qualified_fn).ok_or_else(|| {
            Box::new(EvalAltResult::ErrorFunctionNotFound(
                name.to_string(),
                Position::none(),
            ))
        })
    }

    /// Merge another module into this module.
    pub fn merge(&mut self, other: &Self) {
        self.variables
            .extend(other.variables.iter().map(|(k, v)| (k.clone(), v.clone())));
        self.functions
            .extend(other.functions.iter().map(|(&k, v)| (k, v.clone())));
        self.type_iterators
            .extend(other.type_iterators.iter().map(|(&k, v)| (k, v.clone())));
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
    pub(crate) fn iter_fn(
        &self,
    ) -> impl Iterator<Item = &(String, FnAccess, StaticVec<TypeId>, CallableFunction)> {
        self.functions.values()
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

        module.merge(ast.lib());

        Ok(module)
    }

    /// Scan through all the sub-modules in the module build an index of all
    /// variables and external Rust functions via hashing.
    pub(crate) fn index_all_sub_modules(&mut self) {
        // Collect a particular module.
        fn index_module<'a>(
            module: &'a Module,
            qualifiers: &mut Vec<&'a str>,
            variables: &mut Vec<(u64, Dynamic)>,
            functions: &mut Vec<(u64, CallableFunction)>,
        ) {
            for (name, m) in &module.modules {
                // Index all the sub-modules first.
                qualifiers.push(name);
                index_module(m, qualifiers, variables, functions);
                qualifiers.pop();
            }

            // Index all variables
            for (var_name, value) in &module.variables {
                // Qualifiers + variable name
                let hash_var = calc_fn_hash(qualifiers.iter().map(|&v| v), var_name, 0, empty());
                variables.push((hash_var, value.clone()));
            }
            // Index all Rust functions
            for (name, access, params, func) in module.functions.values() {
                match access {
                    // Private functions are not exported
                    Private => continue,
                    Public => (),
                }

                if func.is_script() {
                    let fn_def = func.get_shared_fn_def();
                    // Qualifiers + function name + number of arguments.
                    let hash_qualified_script = calc_fn_hash(
                        qualifiers.iter().map(|&v| v),
                        &fn_def.name,
                        fn_def.params.len(),
                        empty(),
                    );
                    functions.push((hash_qualified_script, fn_def.into()));
                } else {
                    // Qualified Rust functions are indexed in two steps:
                    // 1) Calculate a hash in a similar manner to script-defined functions,
                    //    i.e. qualifiers + function name + number of arguments.
                    let hash_qualified_script =
                        calc_fn_hash(qualifiers.iter().map(|&v| v), name, params.len(), empty());
                    // 2) Calculate a second hash with no qualifiers, empty function name,
                    //    zero number of arguments, and the actual list of argument `TypeId`'.s
                    let hash_fn_args = calc_fn_hash(empty(), "", 0, params.iter().cloned());
                    // 3) The final hash is the XOR of the two hashes.
                    let hash_qualified_fn = hash_qualified_script ^ hash_fn_args;

                    functions.push((hash_qualified_fn, func.clone()));
                }
            }
        }

        let mut variables = Vec::new();
        let mut functions = Vec::new();

        index_module(self, &mut vec!["root"], &mut variables, &mut functions);

        self.all_variables = variables.into_iter().collect();
        self.all_functions = functions.into_iter().collect();
    }

    /// Does a type iterator exist in the module?
    pub fn contains_iter(&self, id: TypeId) -> bool {
        self.type_iterators.contains_key(&id)
    }

    /// Set a type iterator into the module.
    pub fn set_iter(&mut self, typ: TypeId, func: IteratorFn) {
        self.type_iterators.insert(typ, func);
    }

    /// Get the specified type iterator.
    pub fn get_iter(&self, id: TypeId) -> Option<IteratorFn> {
        self.type_iterators.get(&id).cloned()
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
pub trait ModuleResolver: SendSync {
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
#[cfg(feature = "no_module")]
pub mod resolvers {}

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
    /// `StaticModuleResolver` is a smart pointer to a `HashMap<String, Module>`.
    /// It can simply be treated as `&HashMap<String, Module>`.
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
