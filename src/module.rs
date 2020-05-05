//! Module defining external-loaded modules for Rhai.

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::{FnAny, FnCallArgs, FunctionsLib};
use crate::result::EvalAltResult;
use crate::token::Position;
use crate::token::Token;
use crate::utils::StaticVec;

use crate::stdlib::{any::TypeId, collections::HashMap, fmt, iter::empty, mem, string::String};

/// An imported module, which may contain variables, sub-modules,
/// external Rust functions, and script-defined functions.
///
/// Not available under the `no_module` feature.
#[derive(Default)]
pub struct Module {
    /// Sub-modules.
    modules: HashMap<String, Module>,
    /// Module variables, including sub-modules.
    variables: HashMap<String, Dynamic>,
    /// External Rust functions.
    functions: HashMap<u64, Box<FnAny>>,
    /// Script-defined functions.
    lib: FunctionsLib,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<module {:?}, functions={}, lib={}>",
            self.variables,
            self.functions.len(),
            self.lib.len()
        )
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        // `Module` implements `Clone` so it can fit inside a `Dynamic`
        // but we should never actually clone it.
        unimplemented!()
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
        self.functions.insert(hash, func);
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
        let hash = calc_fn_hash(fn_name, empty());
        let f = move |_: &mut FnCallArgs, _: Position| func().map(|v| v.into());
        self.functions.insert(hash, Box::new(f));
        hash
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
        let hash = calc_fn_hash(fn_name, [TypeId::of::<A>()].iter().cloned());

        let f = move |args: &mut FnCallArgs, _: Position| {
            func(mem::take(args[0]).cast::<A>()).map(|v| v.into())
        };
        self.functions.insert(hash, Box::new(f));
        hash
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
        let hash = calc_fn_hash(fn_name, [TypeId::of::<A>()].iter().cloned());

        let f = move |args: &mut FnCallArgs, _: Position| {
            func(args[0].downcast_mut::<A>().unwrap()).map(|v| v.into())
        };
        self.functions.insert(hash, Box::new(f));
        hash
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
        let hash = calc_fn_hash(
            fn_name,
            [TypeId::of::<A>(), TypeId::of::<B>()].iter().cloned(),
        );

        let f = move |args: &mut FnCallArgs, _: Position| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();

            func(a, b).map(|v| v.into())
        };
        self.functions.insert(hash, Box::new(f));
        hash
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
        let hash = calc_fn_hash(
            fn_name,
            [TypeId::of::<A>(), TypeId::of::<B>()].iter().cloned(),
        );

        let f = move |args: &mut FnCallArgs, _: Position| {
            let b = mem::take(args[1]).cast::<B>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b).map(|v| v.into())
        };
        self.functions.insert(hash, Box::new(f));
        hash
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
        let hash = calc_fn_hash(
            fn_name,
            [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()]
                .iter()
                .cloned(),
        );

        let f = move |args: &mut FnCallArgs, _: Position| {
            let a = mem::take(args[0]).cast::<A>();
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();

            func(a, b, c).map(|v| v.into())
        };
        self.functions.insert(hash, Box::new(f));
        hash
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
        let hash = calc_fn_hash(
            fn_name,
            [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()]
                .iter()
                .cloned(),
        );

        let f = move |args: &mut FnCallArgs, _: Position| {
            let b = mem::take(args[1]).cast::<B>();
            let c = mem::take(args[2]).cast::<C>();
            let a = args[0].downcast_mut::<A>().unwrap();

            func(a, b, c).map(|v| v.into())
        };
        self.functions.insert(hash, Box::new(f));
        hash
    }

    /// Get a Rust function.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    /// It is also returned by the `set_fn_XXX` calls.
    pub fn get_fn(&self, hash: u64) -> Option<&Box<FnAny>> {
        self.functions.get(&hash)
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
}
