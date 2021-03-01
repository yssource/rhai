//! Module that defines the extern API of [`Engine`].

use crate::dynamic::Variant;
use crate::engine::{EvalContext, Imports, State};
use crate::fn_native::{FnCallArgs, SendSync};
use crate::optimize::OptimizationLevel;
use crate::stdlib::{
    any::{type_name, TypeId},
    boxed::Box,
    format,
    string::String,
    vec::Vec,
};
use crate::{
    scope::Scope, Dynamic, Engine, EvalAltResult, FnAccess, FnNamespace, Module, NativeCallContext,
    ParseError, Position, Shared, AST,
};

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_object"))]
use crate::Map;

/// Engine public API
impl Engine {
    /// Register a function of the [`Engine`].
    ///
    /// # WARNING - Low Level API
    ///
    /// This function is very low level.  It takes a list of [`TypeId`][std::any::TypeId]'s indicating the actual types of the parameters.
    ///
    /// Arguments are simply passed in as a mutable array of [`&mut Dynamic`][Dynamic],
    /// The arguments are guaranteed to be of the correct types matching the [`TypeId`][std::any::TypeId]'s.
    ///
    /// To access a primary parameter value (i.e. cloning is cheap), use: `args[n].clone().cast::<T>()`
    ///
    /// To access a parameter value and avoid cloning, use `std::mem::take(args[n]).cast::<T>()`.
    /// Notice that this will _consume_ the argument, replacing it with `()`.
    ///
    /// To access the first mutable parameter, use `args.get_mut(0).unwrap()`
    #[deprecated = "this function is volatile and may change"]
    #[inline(always)]
    pub fn register_raw_fn<T: Variant + Clone>(
        &mut self,
        name: &str,
        arg_types: &[TypeId],
        func: impl Fn(NativeCallContext, &mut FnCallArgs) -> Result<T, Box<EvalAltResult>>
            + SendSync
            + 'static,
    ) -> &mut Self {
        self.global_namespace.set_raw_fn(
            name,
            FnNamespace::Global,
            FnAccess::Public,
            arg_types,
            func,
        );
        self
    }
    /// Register a custom type for use with the [`Engine`].
    /// The type must implement [`Clone`].
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Debug, Clone, Eq, PartialEq)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self                    { Self { field: 1 } }
    ///     fn update(&mut self, offset: i64)   { self.field += offset; }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Use `register_fn` to register methods on the type.
    ///     .register_fn("update", TestStruct::update);
    ///
    /// assert_eq!(
    ///     engine.eval::<TestStruct>("let x = new_ts(); x.update(41); x")?,
    ///     TestStruct { field: 42 }
    /// );
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_type<T: Variant + Clone>(&mut self) -> &mut Self {
        self.register_type_with_name::<T>(type_name::<T>())
    }
    /// Register a custom type for use with the [`Engine`], with a pretty-print name
    /// for the `type_of` function. The type must implement [`Clone`].
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { field: 1 } }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new);
    ///
    /// assert_eq!(
    ///     engine.eval::<String>("let x = new_ts(); type_of(x)")?,
    ///     "rust_out::TestStruct"
    /// );
    ///
    /// // Re-register the custom type with a name.
    /// engine.register_type_with_name::<TestStruct>("Hello");
    ///
    /// assert_eq!(
    ///     engine.eval::<String>("let x = new_ts(); type_of(x)")?,
    ///     "Hello"
    /// );
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_type_with_name<T: Variant + Clone>(&mut self, name: &str) -> &mut Self {
        // Add the pretty-print type name into the map
        self.type_names.insert(type_name::<T>().into(), name.into());
        self
    }
    /// Register an type iterator for an iterable type with the [`Engine`].
    /// This is an advanced feature.
    #[inline(always)]
    pub fn register_iterator<T>(&mut self) -> &mut Self
    where
        T: Variant + Clone + IntoIterator,
        <T as IntoIterator>::Item: Variant + Clone,
    {
        self.global_namespace.set_iterable::<T>();
        self
    }
    /// Register a getter function for a member of a registered type with the [`Engine`].
    ///
    /// The function signature must start with `&mut self` and not `&self`.
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self                { Self { field: 1 } }
    ///     // Even a getter must start with `&mut self` and not `&self`.
    ///     fn get_field(&mut self) -> i64  { self.field }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register a getter on a property (notice it doesn't have to be the same name).
    ///     .register_get("xyz", TestStruct::get_field);
    ///
    /// assert_eq!(engine.eval::<i64>("let a = new_ts(); a.xyz")?, 1);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_get<T: Variant + Clone, U: Variant + Clone>(
        &mut self,
        name: &str,
        get_fn: impl Fn(&mut T) -> U + SendSync + 'static,
    ) -> &mut Self {
        use crate::{engine::make_getter, RegisterFn};
        self.register_fn(&make_getter(name), get_fn)
    }
    /// Register a getter function for a member of a registered type with the [`Engine`].
    ///
    /// The function signature must start with `&mut self` and not `&self`.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Engine, Dynamic, EvalAltResult, RegisterFn};
    ///
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { field: 1 } }
    ///     // Even a getter must start with `&mut self` and not `&self`.
    ///     fn get_field(&mut self) -> Result<Dynamic, Box<EvalAltResult>> {
    ///         Ok(self.field.into())
    ///     }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register a getter on a property (notice it doesn't have to be the same name).
    ///     .register_get_result("xyz", TestStruct::get_field);
    ///
    /// assert_eq!(engine.eval::<i64>("let a = new_ts(); a.xyz")?, 1);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_get_result<T: Variant + Clone>(
        &mut self,
        name: &str,
        get_fn: impl Fn(&mut T) -> Result<Dynamic, Box<EvalAltResult>> + SendSync + 'static,
    ) -> &mut Self {
        use crate::{engine::make_getter, RegisterResultFn};
        self.register_result_fn(&make_getter(name), get_fn)
    }
    /// Register a setter function for a member of a registered type with the [`Engine`].
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Debug, Clone, Eq, PartialEq)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self                        { Self { field: 1 } }
    ///     fn set_field(&mut self, new_val: i64)   { self.field = new_val; }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register a setter on a property (notice it doesn't have to be the same name)
    ///     .register_set("xyz", TestStruct::set_field);
    ///
    /// // Notice that, with a getter, there is no way to get the property value
    /// assert_eq!(
    ///     engine.eval::<TestStruct>("let a = new_ts(); a.xyz = 42; a")?,
    ///     TestStruct { field: 42 }
    /// );
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_set<T: Variant + Clone, U: Variant + Clone>(
        &mut self,
        name: &str,
        set_fn: impl Fn(&mut T, U) + SendSync + 'static,
    ) -> &mut Self {
        use crate::{engine::make_setter, RegisterFn};
        self.register_fn(&make_setter(name), set_fn)
    }
    /// Register a setter function for a member of a registered type with the [`Engine`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Engine, Dynamic, EvalAltResult, RegisterFn};
    ///
    /// #[derive(Debug, Clone, Eq, PartialEq)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { field: 1 } }
    ///     fn set_field(&mut self, new_val: i64) -> Result<(), Box<EvalAltResult>> {
    ///         self.field = new_val;
    ///         Ok(())
    ///     }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register a setter on a property (notice it doesn't have to be the same name)
    ///     .register_set_result("xyz", TestStruct::set_field);
    ///
    /// // Notice that, with a getter, there is no way to get the property value
    /// assert_eq!(
    ///     engine.eval::<TestStruct>("let a = new_ts(); a.xyz = 42; a")?,
    ///     TestStruct { field: 42 }
    /// );
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_set_result<T: Variant + Clone, U: Variant + Clone>(
        &mut self,
        name: &str,
        set_fn: impl Fn(&mut T, U) -> Result<(), Box<EvalAltResult>> + SendSync + 'static,
    ) -> &mut Self {
        use crate::{engine::make_setter, RegisterResultFn};
        self.register_result_fn(&make_setter(name), move |obj: &mut T, value: U| {
            set_fn(obj, value).map(Into::into)
        })
    }
    /// Short-hand for registering both getter and setter functions
    /// of a registered type with the [`Engine`].
    ///
    /// All function signatures must start with `&mut self` and not `&self`.
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     field: i64
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self                        { Self { field: 1 } }
    ///     // Even a getter must start with `&mut self` and not `&self`.
    ///     fn get_field(&mut self) -> i64          { self.field }
    ///     fn set_field(&mut self, new_val: i64)   { self.field = new_val; }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// engine
    ///     .register_type::<TestStruct>()
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register both a getter and a setter on a property
    ///     // (notice it doesn't have to be the same name)
    ///     .register_get_set("xyz", TestStruct::get_field, TestStruct::set_field);
    ///
    /// assert_eq!(engine.eval::<i64>("let a = new_ts(); a.xyz = 42; a.xyz")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn register_get_set<T: Variant + Clone, U: Variant + Clone>(
        &mut self,
        name: &str,
        get_fn: impl Fn(&mut T) -> U + SendSync + 'static,
        set_fn: impl Fn(&mut T, U) + SendSync + 'static,
    ) -> &mut Self {
        self.register_get(name, get_fn).register_set(name, set_fn)
    }
    /// Register an index getter for a custom type with the [`Engine`].
    ///
    /// The function signature must start with `&mut self` and not `&self`.
    ///
    /// # Panics
    ///
    /// Panics if the type is [`Array`], [`Map`], [`String`], [`ImmutableString`][crate::ImmutableString] or `&str`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     fields: Vec<i64>
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { fields: vec![1, 2, 3, 4, 5] } }
    ///     // Even a getter must start with `&mut self` and not `&self`.
    ///     fn get_field(&mut self, index: i64) -> i64 { self.fields[index as usize] }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// # #[cfg(not(feature = "no_object"))]
    /// engine.register_type::<TestStruct>();
    ///
    /// engine
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register an indexer.
    ///     .register_indexer_get(TestStruct::get_field);
    ///
    /// assert_eq!(engine.eval::<i64>("let a = new_ts(); a[2]")?, 3);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn register_indexer_get<T: Variant + Clone, X: Variant + Clone, U: Variant + Clone>(
        &mut self,
        get_fn: impl Fn(&mut T, X) -> U + SendSync + 'static,
    ) -> &mut Self {
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            panic!("Cannot register indexer for arrays.");
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            panic!("Cannot register indexer for object maps.");
        }
        if TypeId::of::<T>() == TypeId::of::<String>()
            || TypeId::of::<T>() == TypeId::of::<&str>()
            || TypeId::of::<T>() == TypeId::of::<crate::ImmutableString>()
        {
            panic!("Cannot register indexer for strings.");
        }

        use crate::{engine::FN_IDX_GET, RegisterFn};
        self.register_fn(FN_IDX_GET, get_fn)
    }
    /// Register an index getter for a custom type with the [`Engine`].
    ///
    /// The function signature must start with `&mut self` and not `&self`.
    ///
    /// # Panics
    ///
    /// Panics if the type is [`Array`], [`Map`], [`String`], [`ImmutableString`][crate::ImmutableString] or `&str`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Engine, Dynamic, EvalAltResult, RegisterFn};
    ///
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     fields: Vec<i64>
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { fields: vec![1, 2, 3, 4, 5] } }
    ///     // Even a getter must start with `&mut self` and not `&self`.
    ///     fn get_field(&mut self, index: i64) -> Result<Dynamic, Box<EvalAltResult>> {
    ///         Ok(self.fields[index as usize].into())
    ///     }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// # #[cfg(not(feature = "no_object"))]
    /// engine.register_type::<TestStruct>();
    ///
    /// engine
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register an indexer.
    ///     .register_indexer_get_result(TestStruct::get_field);
    ///
    /// assert_eq!(engine.eval::<i64>("let a = new_ts(); a[2]")?, 3);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn register_indexer_get_result<T: Variant + Clone, X: Variant + Clone>(
        &mut self,
        get_fn: impl Fn(&mut T, X) -> Result<Dynamic, Box<EvalAltResult>> + SendSync + 'static,
    ) -> &mut Self {
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            panic!("Cannot register indexer for arrays.");
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            panic!("Cannot register indexer for object maps.");
        }
        if TypeId::of::<T>() == TypeId::of::<String>()
            || TypeId::of::<T>() == TypeId::of::<&str>()
            || TypeId::of::<T>() == TypeId::of::<crate::ImmutableString>()
        {
            panic!("Cannot register indexer for strings.");
        }

        use crate::{engine::FN_IDX_GET, RegisterResultFn};
        self.register_result_fn(FN_IDX_GET, get_fn)
    }
    /// Register an index setter for a custom type with the [`Engine`].
    ///
    /// # Panics
    ///
    /// Panics if the type is [`Array`], [`Map`], [`String`], [`ImmutableString`][crate::ImmutableString] or `&str`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     fields: Vec<i64>
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { fields: vec![1, 2, 3, 4, 5] } }
    ///     fn set_field(&mut self, index: i64, value: i64) { self.fields[index as usize] = value; }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// # #[cfg(not(feature = "no_object"))]
    /// engine.register_type::<TestStruct>();
    ///
    /// engine
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register an indexer.
    ///     .register_indexer_set(TestStruct::set_field);
    ///
    /// assert_eq!(
    ///     engine.eval::<TestStruct>("let a = new_ts(); a[2] = 42; a")?.fields[2],
    ///     42
    /// );
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn register_indexer_set<T: Variant + Clone, X: Variant + Clone, U: Variant + Clone>(
        &mut self,
        set_fn: impl Fn(&mut T, X, U) + SendSync + 'static,
    ) -> &mut Self {
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            panic!("Cannot register indexer for arrays.");
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            panic!("Cannot register indexer for object maps.");
        }
        if TypeId::of::<T>() == TypeId::of::<String>()
            || TypeId::of::<T>() == TypeId::of::<&str>()
            || TypeId::of::<T>() == TypeId::of::<crate::ImmutableString>()
        {
            panic!("Cannot register indexer for strings.");
        }

        use crate::{engine::FN_IDX_SET, RegisterFn};
        self.register_fn(FN_IDX_SET, set_fn)
    }
    /// Register an index setter for a custom type with the [`Engine`].
    ///
    /// # Panics
    ///
    /// Panics if the type is [`Array`], [`Map`], [`String`], [`ImmutableString`][crate::ImmutableString] or `&str`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Engine, Dynamic, EvalAltResult, RegisterFn};
    ///
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     fields: Vec<i64>
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self { Self { fields: vec![1, 2, 3, 4, 5] } }
    ///     fn set_field(&mut self, index: i64, value: i64) -> Result<(), Box<EvalAltResult>> {
    ///         self.fields[index as usize] = value;
    ///         Ok(())
    ///     }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// # #[cfg(not(feature = "no_object"))]
    /// engine.register_type::<TestStruct>();
    ///
    /// engine
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register an indexer.
    ///     .register_indexer_set_result(TestStruct::set_field);
    ///
    /// assert_eq!(
    ///     engine.eval::<TestStruct>("let a = new_ts(); a[2] = 42; a")?.fields[2],
    ///     42
    /// );
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn register_indexer_set_result<
        T: Variant + Clone,
        X: Variant + Clone,
        U: Variant + Clone,
    >(
        &mut self,
        set_fn: impl Fn(&mut T, X, U) -> Result<(), Box<EvalAltResult>> + SendSync + 'static,
    ) -> &mut Self {
        if TypeId::of::<T>() == TypeId::of::<Array>() {
            panic!("Cannot register indexer for arrays.");
        }
        #[cfg(not(feature = "no_object"))]
        if TypeId::of::<T>() == TypeId::of::<Map>() {
            panic!("Cannot register indexer for object maps.");
        }
        if TypeId::of::<T>() == TypeId::of::<String>()
            || TypeId::of::<T>() == TypeId::of::<&str>()
            || TypeId::of::<T>() == TypeId::of::<crate::ImmutableString>()
        {
            panic!("Cannot register indexer for strings.");
        }

        use crate::{engine::FN_IDX_SET, RegisterResultFn};
        self.register_result_fn(FN_IDX_SET, move |obj: &mut T, index: X, value: U| {
            set_fn(obj, index, value).map(Into::into)
        })
    }
    /// Short-hand for register both index getter and setter functions for a custom type with the [`Engine`].
    ///
    /// # Panics
    ///
    /// Panics if the type is [`Array`], [`Map`], [`String`], [`ImmutableString`][crate::ImmutableString] or `&str`.
    /// Indexers for arrays, object maps and strings cannot be registered.
    ///
    /// # Example
    ///
    /// ```
    /// #[derive(Clone)]
    /// struct TestStruct {
    ///     fields: Vec<i64>
    /// }
    ///
    /// impl TestStruct {
    ///     fn new() -> Self                                { Self { fields: vec![1, 2, 3, 4, 5] } }
    ///     // Even a getter must start with `&mut self` and not `&self`.
    ///     fn get_field(&mut self, index: i64) -> i64      { self.fields[index as usize] }
    ///     fn set_field(&mut self, index: i64, value: i64) { self.fields[index as usize] = value; }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register API for the custom type.
    /// # #[cfg(not(feature = "no_object"))]
    /// engine.register_type::<TestStruct>();
    ///
    /// engine
    ///     .register_fn("new_ts", TestStruct::new)
    ///     // Register an indexer.
    ///     .register_indexer_get_set(TestStruct::get_field, TestStruct::set_field);
    ///
    /// assert_eq!(engine.eval::<i64>("let a = new_ts(); a[2] = 42; a[2]")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_index"))]
    #[inline(always)]
    pub fn register_indexer_get_set<T: Variant + Clone, X: Variant + Clone, U: Variant + Clone>(
        &mut self,
        get_fn: impl Fn(&mut T, X) -> U + SendSync + 'static,
        set_fn: impl Fn(&mut T, X, U) -> () + SendSync + 'static,
    ) -> &mut Self {
        self.register_indexer_get(get_fn)
            .register_indexer_set(set_fn)
    }
    /// Register a shared [`Module`] into the global namespace of [`Engine`].
    ///
    /// All functions and type iterators are automatically available to scripts without namespace
    /// qualifications.
    ///
    /// Sub-modules and variables are **ignored**.
    ///
    /// When searching for functions, modules loaded later are preferred. In other words, loaded
    /// modules are searched in reverse order.
    #[inline(always)]
    pub fn register_global_module(&mut self, module: Shared<Module>) -> &mut Self {
        // Insert the module into the front
        self.global_modules.insert(0, module);
        self
    }
    /// Register a shared [`Module`] into the global namespace of [`Engine`].
    ///
    /// # Deprecated
    ///
    /// Use [`register_global_module`][Engine::register_global_module] instead.
    #[inline(always)]
    #[deprecated = "use `register_global_module` instead"]
    pub fn load_package(&mut self, module: impl Into<Shared<Module>>) -> &mut Self {
        self.register_global_module(module.into())
    }
    /// Register a shared [`Module`] as a static module namespace with the
    /// [`Engine`].
    ///
    /// Functions marked [`FnNamespace::Global`] and type iterators are exposed to scripts without
    /// namespace qualifications.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Shared, Module};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Create the module
    /// let mut module = Module::new();
    /// module.set_fn_1("calc", |x: i64| Ok(x + 1));
    ///
    /// let module: Shared<Module> = module.into();
    ///
    /// engine
    ///     // Register the module as a fixed sub-module
    ///     .register_static_module("foo::bar::baz", module.clone())
    ///     // Multiple registrations to the same partial path is also OK!
    ///     .register_static_module("foo::bar::hello", module.clone())
    ///     .register_static_module("CalcService", module);
    ///
    /// assert_eq!(engine.eval::<i64>("foo::bar::baz::calc(41)")?, 42);
    /// assert_eq!(engine.eval::<i64>("foo::bar::hello::calc(41)")?, 42);
    /// assert_eq!(engine.eval::<i64>("CalcService::calc(41)")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_module"))]
    pub fn register_static_module(
        &mut self,
        name: impl AsRef<str>,
        module: Shared<Module>,
    ) -> &mut Self {
        fn register_static_module_raw(
            root: &mut crate::stdlib::collections::HashMap<crate::ImmutableString, Shared<Module>>,
            name: impl AsRef<str>,
            module: Shared<Module>,
        ) {
            let separator = crate::token::Token::DoubleColon.syntax();

            if !name.as_ref().contains(separator.as_ref()) {
                if !module.is_indexed() {
                    // Index the module (making a clone copy if necessary) if it is not indexed
                    let mut module = crate::fn_native::shared_take_or_clone(module);
                    module.build_index();
                    root.insert(name.as_ref().trim().into(), module.into());
                } else {
                    root.insert(name.as_ref().trim().into(), module);
                }
            } else {
                let mut iter = name.as_ref().splitn(2, separator.as_ref());
                let sub_module = iter.next().unwrap().trim();
                let remainder = iter.next().unwrap().trim();

                if !root.contains_key(sub_module) {
                    let mut m: Module = Default::default();
                    register_static_module_raw(m.sub_modules_mut(), remainder, module);
                    m.build_index();
                    root.insert(sub_module.into(), m.into());
                } else {
                    let m = root.remove(sub_module).unwrap();
                    let mut m = crate::fn_native::shared_take_or_clone(m);
                    register_static_module_raw(m.sub_modules_mut(), remainder, module);
                    m.build_index();
                    root.insert(sub_module.into(), m.into());
                }
            }
        }

        register_static_module_raw(&mut self.global_sub_modules, name.as_ref(), module);
        self
    }

    /// Register a shared [`Module`] as a static module namespace with the [`Engine`].
    ///
    /// # Deprecated
    ///
    /// Use [`register_static_module`][Engine::register_static_module] instead.
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    #[deprecated = "use `register_static_module` instead"]
    pub fn register_module(
        &mut self,
        name: impl AsRef<str>,
        module: impl Into<Shared<Module>>,
    ) -> &mut Self {
        self.register_static_module(name, module.into())
    }
    /// Compile a string into an [`AST`], which can be used later for evaluation.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// // Compile a script to an AST and store it for later evaluation
    /// let ast = engine.compile("40 + 2")?;
    ///
    /// for _ in 0..42 {
    ///     assert_eq!(engine.eval_ast::<i64>(&ast)?, 42);
    /// }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn compile(&self, script: &str) -> Result<AST, ParseError> {
        self.compile_with_scope(&Default::default(), script)
    }
    /// Compile a string into an [`AST`] using own scope, which can be used later for evaluation.
    ///
    /// The scope is useful for passing constants into the script for optimization
    /// when using [`OptimizationLevel::Full`].
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_optimize"))]
    /// # {
    /// use rhai::{Engine, Scope, OptimizationLevel};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Set optimization level to 'Full' so the Engine can fold constants
    /// // into function calls and operators.
    /// engine.set_optimization_level(OptimizationLevel::Full);
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push_constant("x", 42_i64);   // 'x' is a constant
    ///
    /// // Compile a script to an AST and store it for later evaluation.
    /// // Notice that `Full` optimization is on, so constants are folded
    /// // into function calls and operators.
    /// let ast = engine.compile_with_scope(&mut scope,
    ///             "if x > 40 { x } else { 0 }"    // all 'x' are replaced with 42
    /// )?;
    ///
    /// // Normally this would have failed because no scope is passed into the 'eval_ast'
    /// // call and so the variable 'x' does not exist.  Here, it passes because the script
    /// // has been optimized and all references to 'x' are already gone.
    /// assert_eq!(engine.eval_ast::<i64>(&ast)?, 42);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn compile_with_scope(&self, scope: &Scope, script: &str) -> Result<AST, ParseError> {
        self.compile_scripts_with_scope(scope, &[script])
    }
    /// Compile a string into an [`AST`] using own scope, which can be used later for evaluation,
    /// embedding all imported modules.
    ///
    /// Modules referred by `import` statements containing literal string paths are eagerly resolved
    /// via the current [module resolver][crate::ModuleResolver] and embedded into the resultant
    /// [`AST`]. When it is evaluated later, `import` statement directly recall pre-resolved
    /// [modules][Module] and the resolution process is not performed again.
    #[cfg(not(feature = "no_module"))]
    pub fn compile_into_self_contained(
        &self,
        scope: &Scope,
        script: &str,
    ) -> Result<AST, Box<EvalAltResult>> {
        use crate::{
            ast::{ASTNode, Expr, Stmt},
            fn_native::shared_take_or_clone,
            module::resolvers::StaticModuleResolver,
            stdlib::collections::HashSet,
            ImmutableString,
        };

        fn collect_imports(
            ast: &AST,
            resolver: &StaticModuleResolver,
            imports: &mut HashSet<ImmutableString>,
        ) {
            ast.walk(&mut |path| match path.last().unwrap() {
                // Collect all `import` statements with a string constant path
                ASTNode::Stmt(Stmt::Import(Expr::StringConstant(s, _), _, _))
                    if !resolver.contains_path(s) && !imports.contains(s) =>
                {
                    imports.insert(s.clone());
                }
                _ => (),
            });
        }

        let mut resolver = StaticModuleResolver::new();
        let mut ast = self.compile_scripts_with_scope(scope, &[script])?;
        let mut imports = HashSet::<ImmutableString>::new();

        collect_imports(&ast, &mut resolver, &mut imports);

        if !imports.is_empty() {
            while let Some(path) = imports.iter().next() {
                let path = path.clone();

                match self
                    .module_resolver
                    .resolve_ast(self, &path, Position::NONE)
                {
                    Some(Ok(module_ast)) => {
                        collect_imports(&module_ast, &mut resolver, &mut imports)
                    }
                    Some(err @ Err(_)) => return err,
                    None => (),
                }

                let module = shared_take_or_clone(self.module_resolver.resolve(
                    self,
                    &path,
                    Position::NONE,
                )?);

                imports.remove(&path);
                resolver.insert(path, module);
            }
            ast.set_resolver(resolver);
        }

        Ok(ast)
    }
    /// When passed a list of strings, first join the strings into one large script,
    /// and then compile them into an [`AST`] using own scope, which can be used later for evaluation.
    ///
    /// The scope is useful for passing constants into the script for optimization
    /// when using [`OptimizationLevel::Full`].
    ///
    /// ## Note
    ///
    /// All strings are simply parsed one after another with nothing inserted in between, not even
    /// a newline or space.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_optimize"))]
    /// # {
    /// use rhai::{Engine, Scope, OptimizationLevel};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Set optimization level to 'Full' so the Engine can fold constants
    /// // into function calls and operators.
    /// engine.set_optimization_level(OptimizationLevel::Full);
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push_constant("x", 42_i64);   // 'x' is a constant
    ///
    /// // Compile a script made up of script segments to an AST and store it for later evaluation.
    /// // Notice that `Full` optimization is on, so constants are folded
    /// // into function calls and operators.
    /// let ast = engine.compile_scripts_with_scope(&mut scope, &[
    ///             "if x > 40",            // all 'x' are replaced with 42
    ///             "{ x } el",
    ///             "se { 0 }"              // segments do not need to be valid scripts!
    /// ])?;
    ///
    /// // Normally this would have failed because no scope is passed into the 'eval_ast'
    /// // call and so the variable 'x' does not exist.  Here, it passes because the script
    /// // has been optimized and all references to 'x' are already gone.
    /// assert_eq!(engine.eval_ast::<i64>(&ast)?, 42);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn compile_scripts_with_scope(
        &self,
        scope: &Scope,
        scripts: &[&str],
    ) -> Result<AST, ParseError> {
        self.compile_with_scope_and_optimization_level(scope, scripts, self.optimization_level)
    }
    /// Join a list of strings and compile into an [`AST`] using own scope at a specific optimization level.
    #[inline(always)]
    pub(crate) fn compile_with_scope_and_optimization_level(
        &self,
        scope: &Scope,
        scripts: &[&str],
        optimization_level: OptimizationLevel,
    ) -> Result<AST, ParseError> {
        let stream = self.lex(scripts);
        self.parse(&mut stream.peekable(), scope, optimization_level)
    }
    /// Read the contents of a file into a string.
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline]
    fn read_file(path: crate::stdlib::path::PathBuf) -> Result<String, Box<EvalAltResult>> {
        use crate::stdlib::io::Read;

        let mut f = crate::stdlib::fs::File::open(path.clone()).map_err(|err| {
            EvalAltResult::ErrorSystem(
                format!("Cannot open script file '{}'", path.to_string_lossy()),
                err.into(),
            )
        })?;

        let mut contents = String::new();

        f.read_to_string(&mut contents).map_err(|err| {
            EvalAltResult::ErrorSystem(
                format!("Cannot read script file '{}'", path.to_string_lossy()),
                err.into(),
            )
        })?;

        Ok(contents)
    }
    /// Compile a script file into an [`AST`], which can be used later for evaluation.
    ///
    /// Not available under `no_std` or `WASM`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// // Compile a script file to an AST and store it for later evaluation.
    /// // Notice that a PathBuf is required which can easily be constructed from a string.
    /// let ast = engine.compile_file("script.rhai".into())?;
    ///
    /// for _ in 0..42 {
    ///     engine.eval_ast::<i64>(&ast)?;
    /// }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline(always)]
    pub fn compile_file(
        &self,
        path: crate::stdlib::path::PathBuf,
    ) -> Result<AST, Box<EvalAltResult>> {
        self.compile_file_with_scope(&Default::default(), path)
    }
    /// Compile a script file into an [`AST`] using own scope, which can be used later for evaluation.
    ///
    /// Not available under `no_std` or `WASM`.
    ///
    /// The scope is useful for passing constants into the script for optimization
    /// when using [`OptimizationLevel::Full`].
    ///
    /// # Example
    ///
    /// ```no_run
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_optimize"))]
    /// # {
    /// use rhai::{Engine, Scope, OptimizationLevel};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Set optimization level to 'Full' so the Engine can fold constants.
    /// engine.set_optimization_level(OptimizationLevel::Full);
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push_constant("x", 42_i64);   // 'x' is a constant
    ///
    /// // Compile a script to an AST and store it for later evaluation.
    /// // Notice that a PathBuf is required which can easily be constructed from a string.
    /// let ast = engine.compile_file_with_scope(&mut scope, "script.rhai".into())?;
    ///
    /// let result = engine.eval_ast::<i64>(&ast)?;
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline(always)]
    pub fn compile_file_with_scope(
        &self,
        scope: &Scope,
        path: crate::stdlib::path::PathBuf,
    ) -> Result<AST, Box<EvalAltResult>> {
        Self::read_file(path).and_then(|contents| Ok(self.compile_with_scope(scope, &contents)?))
    }
    /// Parse a JSON string into an [object map][`Map`].
    /// This is a light-weight alternative to using, say, [`serde_json`][https://crates.io/crates/serde_json] to deserialize the JSON.
    ///
    /// The JSON string must be an object hash.  It cannot be a simple scalar value.
    ///
    /// Set `has_null` to `true` in order to map `null` values to `()`.
    /// Setting it to `false` will cause an [`ErrorVariableNotFound`][EvalAltResult::ErrorVariableNotFound] error during parsing.
    ///
    /// # JSON With Sub-Objects
    ///
    /// This method assumes no sub-objects in the JSON string.  That is because the syntax
    /// of a JSON sub-object (or object hash), `{ .. }`, is different from Rhai's syntax, `#{ .. }`.
    /// Parsing a JSON string with sub-objects will cause a syntax error.
    ///
    /// If it is certain that the character `{` never appears in any text string within the JSON object,
    /// which is a valid assumption for many use cases, then globally replace `{` with `#{` before calling this method.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Map};
    ///
    /// let engine = Engine::new();
    ///
    /// let map = engine.parse_json(
    ///     r#"{"a":123, "b":42, "c":{"x":false, "y":true}, "d":null}"#
    ///         .replace("{", "#{").as_str(), true)?;
    ///
    /// assert_eq!(map.len(), 4);
    /// assert_eq!(map["a"].as_int().unwrap(), 123);
    /// assert_eq!(map["b"].as_int().unwrap(), 42);
    /// assert!(map["d"].is::<()>());
    ///
    /// let c = map["c"].read_lock::<Map>().unwrap();
    /// assert_eq!(c["x"].as_bool().unwrap(), false);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_object"))]
    pub fn parse_json(&self, json: &str, has_null: bool) -> Result<Map, Box<EvalAltResult>> {
        use crate::token::Token;

        let mut scope = Default::default();

        // Trims the JSON string and add a '#' in front
        let json_text = json.trim_start();
        let scripts = if json_text.starts_with(Token::MapStart.syntax().as_ref()) {
            [json_text, ""]
        } else if json_text.starts_with(Token::LeftBrace.syntax().as_ref()) {
            ["#", json_text]
        } else {
            return Err(crate::ParseErrorType::MissingToken(
                Token::LeftBrace.syntax().into(),
                "to start a JSON object hash".into(),
            )
            .into_err(Position::new(1, (json.len() - json_text.len() + 1) as u16))
            .into());
        };

        let stream = self.lex_with_map(
            &scripts,
            if has_null {
                |token| match token {
                    // If `null` is present, make sure `null` is treated as a variable
                    Token::Reserved(s) if s == "null" => Token::Identifier(s),
                    _ => token,
                }
            } else {
                |t| t
            },
        );

        let ast =
            self.parse_global_expr(&mut stream.peekable(), &scope, OptimizationLevel::None)?;

        // Handle null - map to ()
        if has_null {
            scope.push_constant("null", ());
        }

        self.eval_ast_with_scope(&mut scope, &ast)
    }
    /// Compile a string containing an expression into an [`AST`],
    /// which can be used later for evaluation.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// // Compile a script to an AST and store it for later evaluation
    /// let ast = engine.compile_expression("40 + 2")?;
    ///
    /// for _ in 0..42 {
    ///     assert_eq!(engine.eval_ast::<i64>(&ast)?, 42);
    /// }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn compile_expression(&self, script: &str) -> Result<AST, ParseError> {
        self.compile_expression_with_scope(&Default::default(), script)
    }
    /// Compile a string containing an expression into an [`AST`] using own scope,
    /// which can be used later for evaluation.
    ///
    /// The scope is useful for passing constants into the script for optimization
    /// when using [`OptimizationLevel::Full`].
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_optimize"))]
    /// # {
    /// use rhai::{Engine, Scope, OptimizationLevel};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Set optimization level to 'Full' so the Engine can fold constants
    /// // into function calls and operators.
    /// engine.set_optimization_level(OptimizationLevel::Full);
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push_constant("x", 10_i64);   // 'x' is a constant
    ///
    /// // Compile a script to an AST and store it for later evaluation.
    /// // Notice that `Full` optimization is on, so constants are folded
    /// // into function calls and operators.
    /// let ast = engine.compile_expression_with_scope(&mut scope,
    ///             "2 + (x + x) * 2"    // all 'x' are replaced with 10
    /// )?;
    ///
    /// // Normally this would have failed because no scope is passed into the 'eval_ast'
    /// // call and so the variable 'x' does not exist.  Here, it passes because the script
    /// // has been optimized and all references to 'x' are already gone.
    /// assert_eq!(engine.eval_ast::<i64>(&ast)?, 42);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn compile_expression_with_scope(
        &self,
        scope: &Scope,
        script: &str,
    ) -> Result<AST, ParseError> {
        let scripts = [script];
        let stream = self.lex(&scripts);

        let mut peekable = stream.peekable();
        self.parse_global_expr(&mut peekable, scope, self.optimization_level)
    }
    /// Evaluate a script file.
    ///
    /// Not available under `no_std` or `WASM`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// // Notice that a PathBuf is required which can easily be constructed from a string.
    /// let result = engine.eval_file::<i64>("script.rhai".into())?;
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline(always)]
    pub fn eval_file<T: Variant + Clone>(
        &self,
        path: crate::stdlib::path::PathBuf,
    ) -> Result<T, Box<EvalAltResult>> {
        Self::read_file(path).and_then(|contents| self.eval::<T>(&contents))
    }
    /// Evaluate a script file with own scope.
    ///
    /// Not available under `no_std` or `WASM`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Scope};
    ///
    /// let engine = Engine::new();
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push("x", 42_i64);
    ///
    /// // Notice that a PathBuf is required which can easily be constructed from a string.
    /// let result = engine.eval_file_with_scope::<i64>(&mut scope, "script.rhai".into())?;
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline(always)]
    pub fn eval_file_with_scope<T: Variant + Clone>(
        &self,
        scope: &mut Scope,
        path: crate::stdlib::path::PathBuf,
    ) -> Result<T, Box<EvalAltResult>> {
        Self::read_file(path).and_then(|contents| self.eval_with_scope::<T>(scope, &contents))
    }
    /// Evaluate a string.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// assert_eq!(engine.eval::<i64>("40 + 2")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn eval<T: Variant + Clone>(&self, script: &str) -> Result<T, Box<EvalAltResult>> {
        self.eval_with_scope(&mut Default::default(), script)
    }
    /// Evaluate a string with own scope.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Scope};
    ///
    /// let engine = Engine::new();
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push("x", 40_i64);
    ///
    /// assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x += 2; x")?, 42);
    /// assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x += 2; x")?, 44);
    ///
    /// // The variable in the scope is modified
    /// assert_eq!(scope.get_value::<i64>("x").expect("variable x should exist"), 44);
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn eval_with_scope<T: Variant + Clone>(
        &self,
        scope: &mut Scope,
        script: &str,
    ) -> Result<T, Box<EvalAltResult>> {
        let ast = self.compile_with_scope_and_optimization_level(
            scope,
            &[script],
            self.optimization_level,
        )?;
        self.eval_ast_with_scope(scope, &ast)
    }
    /// Evaluate a string containing an expression.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// assert_eq!(engine.eval_expression::<i64>("40 + 2")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn eval_expression<T: Variant + Clone>(
        &self,
        script: &str,
    ) -> Result<T, Box<EvalAltResult>> {
        self.eval_expression_with_scope(&mut Default::default(), script)
    }
    /// Evaluate a string containing an expression with own scope.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Scope};
    ///
    /// let engine = Engine::new();
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push("x", 40_i64);
    ///
    /// assert_eq!(engine.eval_expression_with_scope::<i64>(&mut scope, "x + 2")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn eval_expression_with_scope<T: Variant + Clone>(
        &self,
        scope: &mut Scope,
        script: &str,
    ) -> Result<T, Box<EvalAltResult>> {
        let scripts = [script];
        let stream = self.lex(&scripts);

        // No need to optimize a lone expression
        let ast = self.parse_global_expr(&mut stream.peekable(), scope, OptimizationLevel::None)?;

        self.eval_ast_with_scope(scope, &ast)
    }
    /// Evaluate an [`AST`].
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// // Compile a script to an AST and store it for later evaluation
    /// let ast = engine.compile("40 + 2")?;
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<i64>(&ast)?, 42);
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn eval_ast<T: Variant + Clone>(&self, ast: &AST) -> Result<T, Box<EvalAltResult>> {
        self.eval_ast_with_scope(&mut Default::default(), ast)
    }
    /// Evaluate an [`AST`] with own scope.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, Scope};
    ///
    /// let engine = Engine::new();
    ///
    /// // Compile a script to an AST and store it for later evaluation
    /// let ast = engine.compile("x + 2")?;
    ///
    /// // Create initialized scope
    /// let mut scope = Scope::new();
    /// scope.push("x", 40_i64);
    ///
    /// // Compile a script to an AST and store it for later evaluation
    /// let ast = engine.compile("x += 2; x")?;
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast_with_scope::<i64>(&mut scope, &ast)?, 42);
    /// assert_eq!(engine.eval_ast_with_scope::<i64>(&mut scope, &ast)?, 44);
    ///
    /// // The variable in the scope is modified
    /// assert_eq!(scope.get_value::<i64>("x").expect("variable x should exist"), 44);
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn eval_ast_with_scope<T: Variant + Clone>(
        &self,
        scope: &mut Scope,
        ast: &AST,
    ) -> Result<T, Box<EvalAltResult>> {
        let mods = &mut Default::default();

        let result = self.eval_ast_with_scope_raw(scope, mods, ast, 0)?;

        let typ = self.map_type_name(result.type_name());

        return result.try_cast::<T>().ok_or_else(|| {
            EvalAltResult::ErrorMismatchOutputType(
                self.map_type_name(type_name::<T>()).into(),
                typ.into(),
                Position::NONE,
            )
            .into()
        });
    }
    /// Evaluate an [`AST`] with own scope.
    #[inline(always)]
    pub(crate) fn eval_ast_with_scope_raw<'a>(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        ast: &'a AST,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut state: State = Default::default();
        state.source = ast.clone_source();
        #[cfg(not(feature = "no_module"))]
        {
            state.resolver = ast.resolver();
        }

        let statements = ast.statements();
        let lib = &[ast.lib()];
        self.eval_global_statements(scope, mods, &mut state, statements, lib, level)
    }
    /// Evaluate a file, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// Not available under `no_std` or `WASM`.
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline(always)]
    pub fn consume_file(
        &self,
        path: crate::stdlib::path::PathBuf,
    ) -> Result<(), Box<EvalAltResult>> {
        Self::read_file(path).and_then(|contents| self.consume(&contents))
    }
    /// Evaluate a file with own scope, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// Not available under `no_std` or `WASM`.
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    #[inline(always)]
    pub fn consume_file_with_scope(
        &self,
        scope: &mut Scope,
        path: crate::stdlib::path::PathBuf,
    ) -> Result<(), Box<EvalAltResult>> {
        Self::read_file(path).and_then(|contents| self.consume_with_scope(scope, &contents))
    }
    /// Evaluate a string, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    #[inline(always)]
    pub fn consume(&self, script: &str) -> Result<(), Box<EvalAltResult>> {
        self.consume_with_scope(&mut Default::default(), script)
    }
    /// Evaluate a string with own scope, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    #[inline]
    pub fn consume_with_scope(
        &self,
        scope: &mut Scope,
        script: &str,
    ) -> Result<(), Box<EvalAltResult>> {
        let scripts = [script];
        let stream = self.lex(&scripts);
        let ast = self.parse(&mut stream.peekable(), scope, self.optimization_level)?;
        self.consume_ast_with_scope(scope, &ast)
    }
    /// Evaluate an AST, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    #[inline(always)]
    pub fn consume_ast(&self, ast: &AST) -> Result<(), Box<EvalAltResult>> {
        self.consume_ast_with_scope(&mut Default::default(), ast)
    }
    /// Evaluate an [`AST`] with own scope, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    #[inline(always)]
    pub fn consume_ast_with_scope(
        &self,
        scope: &mut Scope,
        ast: &AST,
    ) -> Result<(), Box<EvalAltResult>> {
        let mods = &mut Default::default();
        let mut state: State = Default::default();
        state.source = ast.clone_source();
        #[cfg(not(feature = "no_module"))]
        {
            state.resolver = ast.resolver();
        }
        let statements = ast.statements();
        let lib = &[ast.lib()];
        self.eval_global_statements(scope, mods, &mut state, statements, lib, 0)?;
        Ok(())
    }
    /// Call a script function defined in an [`AST`] with multiple arguments.
    /// Arguments are passed as a tuple.
    ///
    /// ## Warning
    ///
    /// The [`AST`] is _not_ evaluated before calling the function.  The function is called as-is.
    ///
    /// If the [`AST`] needs to be evaluated before calling the function (usually to load external modules),
    /// use [`call_fn_dynamic`][Engine::call_fn_dynamic].
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::{Engine, Scope};
    ///
    /// let engine = Engine::new();
    ///
    /// let ast = engine.compile(r"
    ///     fn add(x, y) { len(x) + y + foo }
    ///     fn add1(x)   { len(x) + 1 + foo }
    ///     fn bar()     { foo/2 }
    /// ")?;
    ///
    /// let mut scope = Scope::new();
    /// scope.push("foo", 42_i64);
    ///
    /// // Call the script-defined function
    /// let result: i64 = engine.call_fn(&mut scope, &ast, "add", ( "abc", 123_i64 ) )?;
    /// assert_eq!(result, 168);
    ///
    /// let result: i64 = engine.call_fn(&mut scope, &ast, "add1", ( "abc", ) )?;
    /// //                                                         ^^^^^^^^^^ tuple of one
    /// assert_eq!(result, 46);
    ///
    /// let result: i64 = engine.call_fn(&mut scope, &ast, "bar", () )?;
    /// assert_eq!(result, 21);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline]
    pub fn call_fn<T: Variant + Clone>(
        &self,
        scope: &mut Scope,
        ast: &AST,
        name: &str,
        args: impl crate::fn_args::FuncArgs,
    ) -> Result<T, Box<EvalAltResult>> {
        let mut arg_values: crate::StaticVec<_> = Default::default();
        args.parse(&mut arg_values);
        let mut args: crate::StaticVec<_> = arg_values.as_mut().iter_mut().collect();

        let result = self.call_fn_dynamic_raw(scope, ast, false, name, &mut None, args.as_mut())?;

        let typ = self.map_type_name(result.type_name());

        return result.try_cast().ok_or_else(|| {
            EvalAltResult::ErrorMismatchOutputType(
                self.map_type_name(type_name::<T>()).into(),
                typ.into(),
                Position::NONE,
            )
            .into()
        });
    }
    /// Call a script function defined in an [`AST`] with multiple [`Dynamic`] arguments
    /// and optionally a value for binding to the `this` pointer.
    ///
    /// There is also an option to evaluate the [`AST`] (e.g. to configuration the environment)
    /// before calling the function.
    ///
    /// # WARNING
    ///
    /// All the arguments are _consumed_, meaning that they're replaced by `()`.
    /// This is to avoid unnecessarily cloning the arguments.
    /// Do not use the arguments after this call. If they are needed afterwards,
    /// clone them _before_ calling this function.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::{Engine, Scope, Dynamic};
    ///
    /// let engine = Engine::new();
    ///
    /// let ast = engine.compile(r"
    ///     fn add(x, y) { len(x) + y + foo }
    ///     fn add1(x)   { len(x) + 1 + foo }
    ///     fn bar()     { foo/2 }
    ///     fn action(x) { this += x; }         // function using 'this' pointer
    /// ")?;
    ///
    /// let mut scope = Scope::new();
    /// scope.push("foo", 42_i64);
    ///
    /// // Call the script-defined function
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, false, "add", None, [ "abc".into(), 123_i64.into() ])?;
    /// //                                                                  ^^^^ no 'this' pointer
    /// assert_eq!(result.cast::<i64>(), 168);
    ///
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, false, "add1", None, [ "abc".into() ])?;
    /// assert_eq!(result.cast::<i64>(), 46);
    ///
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, false, "bar", None, [])?;
    /// assert_eq!(result.cast::<i64>(), 21);
    ///
    /// let mut value: Dynamic = 1_i64.into();
    /// let result = engine.call_fn_dynamic(&mut scope, &ast, false, "action", Some(&mut value), [ 41_i64.into() ])?;
    /// //                                                                     ^^^^^^^^^^^^^^^^ binding the 'this' pointer
    /// assert_eq!(value.as_int().unwrap(), 42);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn call_fn_dynamic(
        &self,
        scope: &mut Scope,
        ast: &AST,
        eval_ast: bool,
        name: &str,
        mut this_ptr: Option<&mut Dynamic>,
        mut arg_values: impl AsMut<[Dynamic]>,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut args: crate::StaticVec<_> = arg_values.as_mut().iter_mut().collect();

        self.call_fn_dynamic_raw(scope, ast, eval_ast, name, &mut this_ptr, args.as_mut())
    }
    /// Call a script function defined in an [`AST`] with multiple [`Dynamic`] arguments.
    ///
    /// # WARNING
    ///
    /// All the arguments are _consumed_, meaning that they're replaced by `()`.
    /// This is to avoid unnecessarily cloning the arguments.
    /// Do not use the arguments after this call. If they are needed afterwards,
    /// clone them _before_ calling this function.
    #[cfg(not(feature = "no_function"))]
    #[inline]
    pub(crate) fn call_fn_dynamic_raw(
        &self,
        scope: &mut Scope,
        ast: &AST,
        eval_ast: bool,
        name: &str,
        this_ptr: &mut Option<&mut Dynamic>,
        args: &mut FnCallArgs,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let state = &mut Default::default();
        let mods = &mut Default::default();
        let lib = &[ast.lib()];

        if eval_ast {
            self.eval_global_statements(scope, mods, state, ast.statements(), lib, 0)?;
        }

        let fn_def = ast
            .lib()
            .get_script_fn(name, args.len(), false)
            .ok_or_else(|| EvalAltResult::ErrorFunctionNotFound(name.into(), Position::NONE))?;

        // Check for data race.
        if cfg!(not(feature = "no_closure")) {
            crate::fn_call::ensure_no_data_race(name, args, false)?;
        }

        self.call_script_fn(
            scope,
            mods,
            state,
            lib,
            this_ptr,
            fn_def,
            args,
            Position::NONE,
            0,
        )
    }
    /// Optimize the [`AST`] with constants defined in an external Scope.
    /// An optimized copy of the [`AST`] is returned while the original [`AST`] is consumed.
    ///
    /// Although optimization is performed by default during compilation, sometimes it is necessary to
    /// _re_-optimize an [`AST`]. For example, when working with constants that are passed in via an
    /// external scope, it will be more efficient to optimize the [`AST`] once again to take advantage
    /// of the new constants.
    ///
    /// With this method, it is no longer necessary to recompile a large script.
    /// The script [`AST`] can be compiled just once. Before evaluation,
    /// constants are passed into the [`Engine`] via an external scope
    /// (i.e. with [`Scope::push_constant`]).
    /// Then, the [`AST`] is cloned and the copy re-optimized before running.
    #[cfg(not(feature = "no_optimize"))]
    #[inline]
    pub fn optimize_ast(
        &self,
        scope: &Scope,
        mut ast: AST,
        optimization_level: OptimizationLevel,
    ) -> AST {
        #[cfg(not(feature = "no_function"))]
        let lib = ast
            .lib()
            .iter_fn()
            .filter(|f| f.func.is_script())
            .map(|f| f.func.get_fn_def().clone())
            .collect();

        #[cfg(feature = "no_function")]
        let lib = Default::default();

        let stmt = crate::stdlib::mem::take(ast.statements_mut());
        crate::optimize::optimize_into_ast(self, scope, stmt, lib, optimization_level)
    }
    /// Generate a list of all registered functions.
    ///
    /// Functions from the following sources are included, in order:
    /// 1) Functions registered into the global namespace
    /// 2) Functions in registered sub-modules
    /// 3) Functions in packages (optional)
    pub fn gen_fn_signatures(&self, include_packages: bool) -> Vec<String> {
        let mut signatures: Vec<_> = Default::default();

        signatures.extend(self.global_namespace.gen_fn_signatures());

        self.global_sub_modules.iter().for_each(|(name, m)| {
            signatures.extend(m.gen_fn_signatures().map(|f| format!("{}::{}", name, f)))
        });

        if include_packages {
            signatures.extend(
                self.global_modules
                    .iter()
                    .flat_map(|m| m.gen_fn_signatures()),
            );
        }

        signatures
    }
    /// Provide a callback that will be invoked before each variable access.
    ///
    /// # Return Value of Callback
    ///
    /// Return `Ok(None)` to continue with normal variable access.  
    /// Return `Ok(Some(Dynamic))` as the variable's value.
    ///
    /// # Errors in Callback
    ///
    /// Return `Err(...)` if there is an error.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::Engine;
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register a variable resolver.
    /// engine.on_var(|name, _, _| {
    ///     match name {
    ///         "MYSTIC_NUMBER" => Ok(Some(42_i64.into())),
    ///         _ => Ok(None)
    ///     }
    /// });
    ///
    /// engine.eval::<i64>("MYSTIC_NUMBER")?;
    ///
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn on_var(
        &mut self,
        callback: impl Fn(&str, usize, &EvalContext) -> Result<Option<Dynamic>, Box<EvalAltResult>>
            + SendSync
            + 'static,
    ) -> &mut Self {
        self.resolve_var = Some(Box::new(callback));
        self
    }
    /// Register a callback for script evaluation progress.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # use std::sync::RwLock;
    /// # use std::sync::Arc;
    /// use rhai::Engine;
    ///
    /// let result = Arc::new(RwLock::new(0_u64));
    /// let logger = result.clone();
    ///
    /// let mut engine = Engine::new();
    ///
    /// engine.on_progress(move |ops| {
    ///     if ops > 10000 {
    ///         Some("Over 10,000 operations!".into())
    ///     } else if ops % 800 == 0 {
    ///         *logger.write().unwrap() = ops;
    ///         None
    ///     } else {
    ///         None
    ///     }
    /// });
    ///
    /// engine.consume("for x in range(0, 50000) {}")
    ///     .expect_err("should error");
    ///
    /// assert_eq!(*result.read().unwrap(), 9600);
    ///
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn on_progress(
        &mut self,
        callback: impl Fn(u64) -> Option<Dynamic> + SendSync + 'static,
    ) -> &mut Self {
        self.progress = Some(Box::new(callback));
        self
    }
    /// Override default action of `print` (print to stdout using [`println!`])
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # use std::sync::RwLock;
    /// # use std::sync::Arc;
    /// use rhai::Engine;
    ///
    /// let result = Arc::new(RwLock::new(String::new()));
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Override action of 'print' function
    /// let logger = result.clone();
    /// engine.on_print(move |s| logger.write().unwrap().push_str(s));
    ///
    /// engine.consume("print(40 + 2);")?;
    ///
    /// assert_eq!(*result.read().unwrap(), "42");
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn on_print(&mut self, callback: impl Fn(&str) + SendSync + 'static) -> &mut Self {
        self.print = Box::new(callback);
        self
    }
    /// Override default action of `debug` (print to stdout using [`println!`])
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # use std::sync::RwLock;
    /// # use std::sync::Arc;
    /// use rhai::Engine;
    ///
    /// let result = Arc::new(RwLock::new(String::new()));
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Override action of 'print' function
    /// let logger = result.clone();
    /// engine.on_debug(move |s, src, pos| logger.write().unwrap().push_str(
    ///                     &format!("{} @ {:?} > {}", src.unwrap_or("unknown"), pos, s)
    ///                ));
    ///
    /// let mut ast = engine.compile(r#"let x = "hello"; debug(x);"#)?;
    /// ast.set_source("world");
    /// engine.consume_ast(&ast)?;
    ///
    /// assert_eq!(*result.read().unwrap(), r#"world @ 1:18 > "hello""#);
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn on_debug(
        &mut self,
        callback: impl Fn(&str, Option<&str>, Position) + SendSync + 'static,
    ) -> &mut Self {
        self.debug = Box::new(callback);
        self
    }
}
