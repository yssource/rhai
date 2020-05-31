//! Main module defining the script evaluation `Engine`.

use crate::any::{Dynamic, Union};
use crate::calc_fn_hash;
use crate::error::ParseErrorType;
use crate::fn_native::{CallableFunction, FnCallArgs, Shared};
use crate::module::Module;
use crate::optimize::OptimizationLevel;
use crate::packages::{CorePackage, Package, PackageLibrary, PackagesCollection, StandardPackage};
use crate::parser::{Expr, FnAccess, FnDef, ImmutableString, ReturnType, Stmt, AST, INT};
use crate::r#unsafe::{unsafe_cast_var_name_to_lifetime, unsafe_mut_cast_to_lifetime};
use crate::result::EvalAltResult;
use crate::scope::{EntryType as ScopeEntryType, Scope};
use crate::token::Position;
use crate::utils::{StaticVec, StraightHasherBuilder};

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

#[cfg(not(feature = "no_module"))]
use crate::module::{resolvers, ModuleResolver};

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::HashMap,
    format,
    iter::{empty, once},
    mem,
    ops::{Deref, DerefMut},
    string::{String, ToString},
    vec::Vec,
};

/// Variable-sized array of `Dynamic` values.
///
/// Not available under the `no_index` feature.
#[cfg(not(feature = "no_index"))]
pub type Array = Vec<Dynamic>;

/// Hash map of `Dynamic` values with `String` keys.
///
/// Not available under the `no_object` feature.
#[cfg(not(feature = "no_object"))]
pub type Map = HashMap<String, Dynamic>;

#[cfg(not(feature = "unchecked"))]
#[cfg(debug_assertions)]
pub const MAX_CALL_STACK_DEPTH: usize = 16;
#[cfg(not(feature = "unchecked"))]
#[cfg(debug_assertions)]
pub const MAX_EXPR_DEPTH: usize = 32;
#[cfg(not(feature = "unchecked"))]
#[cfg(debug_assertions)]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = 16;

#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_CALL_STACK_DEPTH: usize = 128;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_EXPR_DEPTH: usize = 128;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = 32;

#[cfg(feature = "unchecked")]
pub const MAX_CALL_STACK_DEPTH: usize = usize::MAX;
#[cfg(feature = "unchecked")]
pub const MAX_EXPR_DEPTH: usize = usize::MAX;
#[cfg(feature = "unchecked")]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = usize::MAX;

pub const KEYWORD_PRINT: &str = "print";
pub const KEYWORD_DEBUG: &str = "debug";
pub const KEYWORD_TYPE_OF: &str = "type_of";
pub const KEYWORD_EVAL: &str = "eval";
pub const FUNC_TO_STRING: &str = "to_string";
pub const FUNC_GETTER: &str = "get$";
pub const FUNC_SETTER: &str = "set$";
pub const FUNC_INDEXER: &str = "$index$";

/// A type that encapsulates a mutation target for an expression with side effects.
enum Target<'a> {
    /// The target is a mutable reference to a `Dynamic` value somewhere.
    Ref(&'a mut Dynamic),
    /// The target is a temporary `Dynamic` value (i.e. the mutation can cause no side effects).
    Value(Dynamic),
    /// The target is a character inside a String.
    /// This is necessary because directly pointing to a char inside a String is impossible.
    StringChar(&'a mut Dynamic, usize, Dynamic),
}

impl Target<'_> {
    /// Is the `Target` a reference pointing to other data?
    pub fn is_ref(&self) -> bool {
        match self {
            Target::Ref(_) => true,
            Target::Value(_) | Target::StringChar(_, _, _) => false,
        }
    }

    /// Get the value of the `Target` as a `Dynamic`, cloning a referenced value if necessary.
    pub fn clone_into_dynamic(self) -> Dynamic {
        match self {
            Target::Ref(r) => r.clone(),        // Referenced value is cloned
            Target::Value(v) => v,              // Owned value is simply taken
            Target::StringChar(_, _, ch) => ch, // Character is taken
        }
    }

    /// Get a mutable reference from the `Target`.
    pub fn as_mut(&mut self) -> &mut Dynamic {
        match self {
            Target::Ref(r) => *r,
            Target::Value(ref mut r) => r,
            Target::StringChar(_, _, ref mut r) => r,
        }
    }

    /// Update the value of the `Target`.
    pub fn set_value(&mut self, new_val: Dynamic, pos: Position) -> Result<(), Box<EvalAltResult>> {
        match self {
            Target::Ref(r) => **r = new_val,
            Target::Value(_) => {
                return Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(pos)))
            }
            Target::StringChar(Dynamic(Union::Str(ref mut s)), index, _) => {
                // Replace the character at the specified index position
                let new_ch = new_val
                    .as_char()
                    .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;

                let mut chars: StaticVec<char> = s.chars().collect();
                let ch = chars[*index];

                // See if changed - if so, update the String
                if ch != new_ch {
                    chars[*index] = new_ch;
                    *s = chars.iter().collect::<String>().into();
                }
            }
            _ => unreachable!(),
        }

        Ok(())
    }
}

impl<'a> From<&'a mut Dynamic> for Target<'a> {
    fn from(value: &'a mut Dynamic) -> Self {
        Self::Ref(value)
    }
}
impl<T: Into<Dynamic>> From<T> for Target<'_> {
    fn from(value: T) -> Self {
        Self::Value(value.into())
    }
}

/// A type that holds all the current states of the Engine.
///
/// # Safety
///
/// This type uses some unsafe code, mainly for avoiding cloning of local variable names via
/// direct lifetime casting.
#[derive(Debug, Clone, Default)]
pub struct State {
    /// Normally, access to variables are parsed with a relative offset into the scope to avoid a lookup.
    /// In some situation, e.g. after running an `eval` statement, subsequent offsets become mis-aligned.
    /// When that happens, this flag is turned on to force a scope lookup by name.
    pub always_search: bool,

    /// Level of the current scope.  The global (root) level is zero, a new block (or function call)
    /// is one level higher, and so on.
    pub scope_level: usize,

    /// Number of operations performed.
    pub operations: u64,

    /// Number of modules loaded.
    pub modules: u64,
}

impl State {
    /// Create a new `State`.
    pub fn new() -> Self {
        Default::default()
    }
}

/// A type that holds a library (`HashMap`) of script-defined functions.
///
/// Since script-defined functions have `Dynamic` parameters, functions with the same name
/// and number of parameters are considered equivalent.
///
/// The key of the `HashMap` is a `u64` hash calculated by the function `calc_fn_hash`.
#[derive(Debug, Clone, Default)]
pub struct FunctionsLib(HashMap<u64, Shared<FnDef>, StraightHasherBuilder>);

impl FunctionsLib {
    /// Create a new `FunctionsLib` from a collection of `FnDef`.
    pub fn from_iter(vec: impl IntoIterator<Item = FnDef>) -> Self {
        FunctionsLib(
            vec.into_iter()
                .map(|fn_def| {
                    // Qualifiers (none) + function name + number of arguments.
                    let hash = calc_fn_hash(empty(), &fn_def.name, fn_def.params.len(), empty());
                    (hash, fn_def.into())
                })
                .collect(),
        )
    }
    /// Does a certain function exist in the `FunctionsLib`?
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    pub fn has_function(&self, hash_fn_def: u64) -> bool {
        self.contains_key(&hash_fn_def)
    }
    /// Get a function definition from the `FunctionsLib`.
    ///
    /// The `u64` hash is calculated by the function `crate::calc_fn_hash`.
    pub fn get_function(&self, hash_fn_def: u64) -> Option<&FnDef> {
        self.get(&hash_fn_def).map(|fn_def| fn_def.as_ref())
    }
    /// Get a function definition from the `FunctionsLib`.
    pub fn get_function_by_signature(
        &self,
        name: &str,
        params: usize,
        public_only: bool,
    ) -> Option<&FnDef> {
        // Qualifiers (none) + function name + number of arguments.
        let hash_fn_def = calc_fn_hash(empty(), name, params, empty());
        let fn_def = self.get_function(hash_fn_def);

        match fn_def.as_ref().map(|f| f.access) {
            None => None,
            Some(FnAccess::Private) if public_only => None,
            Some(FnAccess::Private) | Some(FnAccess::Public) => fn_def,
        }
    }
    /// Merge another `FunctionsLib` into this `FunctionsLib`.
    pub fn merge(&self, other: &Self) -> Self {
        if self.is_empty() {
            other.clone()
        } else if other.is_empty() {
            self.clone()
        } else {
            let mut functions = self.clone();
            functions.extend(other.iter().map(|(hash, fn_def)| (*hash, fn_def.clone())));
            functions
        }
    }
}

impl From<Vec<(u64, Shared<FnDef>)>> for FunctionsLib {
    fn from(values: Vec<(u64, Shared<FnDef>)>) -> Self {
        FunctionsLib(values.into_iter().collect())
    }
}

impl Deref for FunctionsLib {
    type Target = HashMap<u64, Shared<FnDef>, StraightHasherBuilder>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FunctionsLib {
    fn deref_mut(&mut self) -> &mut HashMap<u64, Shared<FnDef>, StraightHasherBuilder> {
        &mut self.0
    }
}

/// Rhai main scripting engine.
///
/// ```
/// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
/// use rhai::Engine;
///
/// let engine = Engine::new();
///
/// let result = engine.eval::<i64>("40 + 2")?;
///
/// println!("Answer: {}", result);  // prints 42
/// # Ok(())
/// # }
/// ```
///
/// Currently, `Engine` is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
pub struct Engine {
    /// A module containing all functions directly loaded into the Engine.
    pub(crate) global_module: Module,
    /// A collection of all library packages loaded into the Engine.
    pub(crate) packages: PackagesCollection,

    /// A module resolution service.
    #[cfg(not(feature = "no_module"))]
    pub(crate) module_resolver: Option<Box<dyn ModuleResolver>>,

    /// A hashmap mapping type names to pretty-print names.
    pub(crate) type_names: HashMap<String, String>,

    /// Closure for implementing the `print` command.
    #[cfg(not(feature = "sync"))]
    pub(crate) print: Box<dyn Fn(&str) + 'static>,
    /// Closure for implementing the `print` command.
    #[cfg(feature = "sync")]
    pub(crate) print: Box<dyn Fn(&str) + Send + Sync + 'static>,

    /// Closure for implementing the `debug` command.
    #[cfg(not(feature = "sync"))]
    pub(crate) debug: Box<dyn Fn(&str) + 'static>,
    /// Closure for implementing the `debug` command.
    #[cfg(feature = "sync")]
    pub(crate) debug: Box<dyn Fn(&str) + Send + Sync + 'static>,

    /// Closure for progress reporting.
    #[cfg(not(feature = "sync"))]
    pub(crate) progress: Option<Box<dyn Fn(u64) -> bool + 'static>>,
    /// Closure for progress reporting.
    #[cfg(feature = "sync")]
    pub(crate) progress: Option<Box<dyn Fn(u64) -> bool + Send + Sync + 'static>>,

    /// Optimize the AST after compilation.
    pub(crate) optimization_level: OptimizationLevel,
    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Defaults to 16 for debug builds and 128 for non-debug builds.
    pub(crate) max_call_stack_depth: usize,
    /// Maximum depth of statements/expressions at global level.
    pub(crate) max_expr_depth: usize,
    /// Maximum depth of statements/expressions in functions.
    pub(crate) max_function_expr_depth: usize,
    /// Maximum number of operations allowed to run.
    pub(crate) max_operations: u64,
    /// Maximum number of modules allowed to load.
    pub(crate) max_modules: u64,
}

impl Default for Engine {
    fn default() -> Self {
        // Create the new scripting Engine
        let mut engine = Self {
            packages: Default::default(),
            global_module: Default::default(),

            #[cfg(not(feature = "no_module"))]
            #[cfg(not(feature = "no_std"))]
            module_resolver: Some(Box::new(resolvers::FileModuleResolver::new())),
            #[cfg(not(feature = "no_module"))]
            #[cfg(feature = "no_std")]
            module_resolver: None,

            type_names: HashMap::new(),

            // default print/debug implementations
            print: Box::new(default_print),
            debug: Box::new(default_print),

            // progress callback
            progress: None,

            // optimization level
            #[cfg(feature = "no_optimize")]
            optimization_level: OptimizationLevel::None,

            #[cfg(not(feature = "no_optimize"))]
            optimization_level: OptimizationLevel::Simple,

            max_call_stack_depth: MAX_CALL_STACK_DEPTH,
            max_expr_depth: MAX_EXPR_DEPTH,
            max_function_expr_depth: MAX_FUNCTION_EXPR_DEPTH,
            max_operations: u64::MAX,
            max_modules: u64::MAX,
        };

        engine.load_package(StandardPackage::new().get());

        engine
    }
}

/// Make getter function
pub fn make_getter(id: &str) -> String {
    format!("{}{}", FUNC_GETTER, id)
}

/// Extract the property name from a getter function name.
fn extract_prop_from_getter(fn_name: &str) -> Option<&str> {
    #[cfg(not(feature = "no_object"))]
    {
        if fn_name.starts_with(FUNC_GETTER) {
            Some(&fn_name[FUNC_GETTER.len()..])
        } else {
            None
        }
    }
    #[cfg(feature = "no_object")]
    {
        None
    }
}

/// Make setter function
pub fn make_setter(id: &str) -> String {
    format!("{}{}", FUNC_SETTER, id)
}

/// Extract the property name from a setter function name.
fn extract_prop_from_setter(fn_name: &str) -> Option<&str> {
    #[cfg(not(feature = "no_object"))]
    {
        if fn_name.starts_with(FUNC_SETTER) {
            Some(&fn_name[FUNC_SETTER.len()..])
        } else {
            None
        }
    }
    #[cfg(feature = "no_object")]
    {
        None
    }
}

/// Print/debug to stdout
fn default_print(s: &str) {
    #[cfg(not(feature = "no_std"))]
    println!("{}", s);
}

/// Search for a variable within the scope
fn search_scope<'s, 'a>(
    scope: &'s mut Scope,
    state: &mut State,
    expr: &'a Expr,
) -> Result<(&'s mut Dynamic, &'a str, ScopeEntryType, Position), Box<EvalAltResult>> {
    let ((name, pos), modules, hash_var, index) = match expr {
        Expr::Variable(x) => x.as_ref(),
        _ => unreachable!(),
    };

    #[cfg(not(feature = "no_module"))]
    {
        if let Some(modules) = modules.as_ref() {
            let module = if let Some(index) = modules.index() {
                scope
                    .get_mut(scope.len() - index.get())
                    .0
                    .downcast_mut::<Module>()
                    .unwrap()
            } else {
                let (id, root_pos) = modules.get(0);

                scope.find_module(id).ok_or_else(|| {
                    Box::new(EvalAltResult::ErrorModuleNotFound(id.into(), *root_pos))
                })?
            };

            return Ok((
                module.get_qualified_var_mut(name, *hash_var, *pos)?,
                name,
                // Module variables are constant
                ScopeEntryType::Constant,
                *pos,
            ));
        }
    }

    let index = if state.always_search { None } else { *index };

    let index = if let Some(index) = index {
        scope.len() - index.get()
    } else {
        scope
            .get_index(name)
            .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(name.into(), *pos)))?
            .0
    };

    let (val, typ) = scope.get_mut(index);
    Ok((val, name, typ, *pos))
}

impl Engine {
    /// Create a new `Engine`
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new `Engine` with minimal built-in functions.
    /// Use the `load_package` method to load additional packages of functions.
    pub fn new_raw() -> Self {
        Self {
            packages: Default::default(),
            global_module: Default::default(),

            #[cfg(not(feature = "no_module"))]
            module_resolver: None,

            type_names: HashMap::new(),
            print: Box::new(|_| {}),
            debug: Box::new(|_| {}),
            progress: None,

            #[cfg(feature = "no_optimize")]
            optimization_level: OptimizationLevel::None,

            #[cfg(not(feature = "no_optimize"))]
            optimization_level: OptimizationLevel::Simple,

            max_call_stack_depth: MAX_CALL_STACK_DEPTH,
            max_expr_depth: MAX_EXPR_DEPTH,
            max_function_expr_depth: MAX_FUNCTION_EXPR_DEPTH,
            max_operations: u64::MAX,
            max_modules: u64::MAX,
        }
    }

    /// Load a new package into the `Engine`.
    ///
    /// When searching for functions, packages loaded later are preferred.
    /// In other words, loaded packages are searched in reverse order.
    pub fn load_package(&mut self, package: PackageLibrary) {
        // Push the package to the top - packages are searched in reverse order
        self.packages.push(package);
    }

    /// Load a new package into the `Engine`.
    ///
    /// When searching for functions, packages loaded later are preferred.
    /// In other words, loaded packages are searched in reverse order.
    pub fn load_packages(&mut self, package: PackageLibrary) {
        // Push the package to the top - packages are searched in reverse order
        self.packages.push(package);
    }

    /// Control whether and how the `Engine` will optimize an AST after compilation.
    ///
    /// Not available under the `no_optimize` feature.
    #[cfg(not(feature = "no_optimize"))]
    pub fn set_optimization_level(&mut self, optimization_level: OptimizationLevel) {
        self.optimization_level = optimization_level
    }

    /// Set the maximum levels of function calls allowed for a script in order to avoid
    /// infinite recursion and stack overflows.
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_call_levels(&mut self, levels: usize) {
        self.max_call_stack_depth = levels
    }

    /// Set the maximum number of operations allowed for a script to run to avoid
    /// consuming too much resources (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_operations(&mut self, operations: u64) {
        self.max_operations = if operations == 0 {
            u64::MAX
        } else {
            operations
        };
    }

    /// Set the maximum number of imported modules allowed for a script (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_modules(&mut self, modules: u64) {
        self.max_modules = if modules == 0 { u64::MAX } else { modules };
    }

    /// Set the depth limits for expressions/statements.
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_expr_depths(&mut self, max_expr_depth: usize, max_function_expr_depth: usize) {
        self.max_expr_depth = max_expr_depth;
        self.max_function_expr_depth = max_function_expr_depth;
    }

    /// Set the module resolution service used by the `Engine`.
    ///
    /// Not available under the `no_module` feature.
    #[cfg(not(feature = "no_module"))]
    pub fn set_module_resolver(&mut self, resolver: Option<impl ModuleResolver + 'static>) {
        self.module_resolver = resolver.map(|f| Box::new(f) as Box<dyn ModuleResolver>);
    }

    /// Universal method for calling functions either registered with the `Engine` or written in Rhai.
    ///
    /// ## WARNING
    ///
    /// Function call arguments be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_fn_raw(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        fn_name: &str,
        hashes: (u64, u64),
        args: &mut FnCallArgs,
        is_ref: bool,
        def_val: Option<&Dynamic>,
        pos: Position,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        self.inc_operations(state, pos)?;

        let native_only = hashes.1 == 0;

        // Check for stack overflow
        #[cfg(not(feature = "no_function"))]
        #[cfg(not(feature = "unchecked"))]
        {
            if level > self.max_call_stack_depth {
                return Err(Box::new(EvalAltResult::ErrorStackOverflow(pos)));
            }
        }

        let mut this_copy: Dynamic = Default::default();
        let mut old_this_ptr: Option<&mut Dynamic> = None;

        /// This function replaces the first argument of a method call with a clone copy.
        /// This is to prevent a pure function unintentionally consuming the first argument.
        fn normalize_first_arg<'a>(
            normalize: bool,
            this_copy: &mut Dynamic,
            old_this_ptr: &mut Option<&'a mut Dynamic>,
            args: &mut FnCallArgs<'a>,
        ) {
            // Only do it for method calls with arguments.
            if !normalize || args.is_empty() {
                return;
            }

            // Clone the original value.
            *this_copy = args[0].clone();

            // Replace the first reference with a reference to the clone, force-casting the lifetime.
            // Keep the original reference.  Must remember to restore it later with `restore_first_arg_of_method_call`.
            let this_pointer = mem::replace(
                args.get_mut(0).unwrap(),
                unsafe_mut_cast_to_lifetime(this_copy),
            );

            *old_this_ptr = Some(this_pointer);
        }

        /// This function restores the first argument that was replaced by `normalize_first_arg_of_method_call`.
        fn restore_first_arg<'a>(old_this_ptr: Option<&'a mut Dynamic>, args: &mut FnCallArgs<'a>) {
            if let Some(this_pointer) = old_this_ptr {
                mem::replace(args.get_mut(0).unwrap(), this_pointer);
            }
        }

        // First search in script-defined functions (can override built-in)
        if !native_only {
            if let Some(fn_def) = lib.get(&hashes.1) {
                normalize_first_arg(is_ref, &mut this_copy, &mut old_this_ptr, args);

                // Run scripted function
                let result =
                    self.call_script_fn(scope, state, lib, fn_name, fn_def, args, pos, level)?;

                // Restore the original reference
                restore_first_arg(old_this_ptr, args);

                return Ok((result, false));
            }
        }

        // Search built-in's and external functions
        if let Some(func) = self
            .global_module
            .get_fn(hashes.0)
            .or_else(|| self.packages.get_fn(hashes.0))
        {
            // Calling pure function in method-call?
            normalize_first_arg(
                func.is_pure() && is_ref,
                &mut this_copy,
                &mut old_this_ptr,
                args,
            );

            // Run external function
            let result = func.get_native_fn()(args);

            // Restore the original reference
            restore_first_arg(old_this_ptr, args);

            let result = result.map_err(|err| err.new_position(pos))?;

            // See if the function match print/debug (which requires special processing)
            return Ok(match fn_name {
                KEYWORD_PRINT => (
                    (self.print)(result.as_str().map_err(|type_name| {
                        Box::new(EvalAltResult::ErrorMismatchOutputType(
                            type_name.into(),
                            pos,
                        ))
                    })?)
                    .into(),
                    false,
                ),
                KEYWORD_DEBUG => (
                    (self.debug)(result.as_str().map_err(|type_name| {
                        Box::new(EvalAltResult::ErrorMismatchOutputType(
                            type_name.into(),
                            pos,
                        ))
                    })?)
                    .into(),
                    false,
                ),
                _ => (result, func.is_method()),
            });
        }

        // See if it is built in.
        if args.len() == 2 {
            match run_builtin_binary_op(fn_name, args[0], args[1])? {
                Some(v) => return Ok((v, false)),
                None => (),
            }
        }

        // Return default value (if any)
        if let Some(val) = def_val {
            return Ok((val.clone(), false));
        }

        // Getter function not found?
        if let Some(prop) = extract_prop_from_getter(fn_name) {
            return Err(Box::new(EvalAltResult::ErrorDotExpr(
                format!("- property '{}' unknown or write-only", prop),
                pos,
            )));
        }

        // Setter function not found?
        if let Some(prop) = extract_prop_from_setter(fn_name) {
            return Err(Box::new(EvalAltResult::ErrorDotExpr(
                format!("- property '{}' unknown or read-only", prop),
                pos,
            )));
        }

        let types_list: Vec<_> = args
            .iter()
            .map(|name| self.map_type_name(name.type_name()))
            .collect();

        // Getter function not found?
        if fn_name == FUNC_INDEXER {
            return Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
                format!("[]({})", types_list.join(", ")),
                pos,
            )));
        }

        // Raise error
        Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
            format!("{} ({})", fn_name, types_list.join(", ")),
            pos,
        )))
    }

    /// Call a script-defined function.
    ///
    /// ## WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_script_fn(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        fn_name: &str,
        fn_def: &FnDef,
        args: &mut FnCallArgs,
        pos: Position,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let orig_scope_level = state.scope_level;
        state.scope_level += 1;

        let scope_len = scope.len();

        // Put arguments into scope as variables
        // Actually consume the arguments instead of cloning them
        scope.extend(
            fn_def
                .params
                .iter()
                .zip(args.iter_mut().map(|v| mem::take(*v)))
                .map(|(name, value)| {
                    let var_name = unsafe_cast_var_name_to_lifetime(name.as_str(), state);
                    (var_name, ScopeEntryType::Normal, value)
                }),
        );

        // Evaluate the function at one higher level of call depth
        let result = self
            .eval_stmt(scope, state, lib, &fn_def.body, level + 1)
            .or_else(|err| match *err {
                // Convert return statement to return value
                EvalAltResult::Return(x, _) => Ok(x),
                EvalAltResult::ErrorInFunctionCall(name, err, _) => Err(Box::new(
                    EvalAltResult::ErrorInFunctionCall(format!("{} > {}", fn_name, name), err, pos),
                )),
                _ => Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                    fn_name.to_string(),
                    err,
                    pos,
                ))),
            });

        // Remove all local variables
        scope.rewind(scope_len);
        state.scope_level = orig_scope_level;

        result
    }

    // Has a system function an override?
    fn has_override(&self, lib: &FunctionsLib, hashes: (u64, u64)) -> bool {
        // First check registered functions
        self.global_module.contains_fn(hashes.0)
            // Then check packages
            || self.packages.contains_fn(hashes.0)
            // Then check script-defined functions
            || lib.contains_key(&hashes.1)
    }

    // Perform an actual function call, taking care of special functions
    ///
    /// ## WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    fn exec_fn_call(
        &self,
        state: &mut State,
        lib: &FunctionsLib,
        fn_name: &str,
        native_only: bool,
        hash_fn_def: u64,
        args: &mut FnCallArgs,
        is_ref: bool,
        def_val: Option<&Dynamic>,
        pos: Position,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
        let hash_fn = calc_fn_hash(
            empty(),
            fn_name,
            args.len(),
            args.iter().map(|a| a.type_id()),
        );
        let hashes = (hash_fn, if native_only { 0 } else { hash_fn_def });

        match fn_name {
            // type_of
            KEYWORD_TYPE_OF if args.len() == 1 && !self.has_override(lib, hashes) => Ok((
                self.map_type_name(args[0].type_name()).to_string().into(),
                false,
            )),

            // eval - reaching this point it must be a method-style call
            KEYWORD_EVAL if args.len() == 1 && !self.has_override(lib, hashes) => {
                Err(Box::new(EvalAltResult::ErrorRuntime(
                    "'eval' should not be called in method style. Try eval(...);".into(),
                    pos,
                )))
            }

            // Normal function call
            _ => {
                let mut scope = Scope::new();
                self.call_fn_raw(
                    &mut scope, state, lib, fn_name, hashes, args, is_ref, def_val, pos, level,
                )
            }
        }
    }

    /// Evaluate a text string as a script - used primarily for 'eval'.
    fn eval_script_expr(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        script: &Dynamic,
        pos: Position,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let script = script
            .as_str()
            .map_err(|type_name| EvalAltResult::ErrorMismatchOutputType(type_name.into(), pos))?;

        // Compile the script text
        // No optimizations because we only run it once
        let mut ast = self.compile_with_scope_and_optimization_level(
            &Scope::new(),
            &[script],
            OptimizationLevel::None,
        )?;

        // If new functions are defined within the eval string, it is an error
        if !ast.lib().is_empty() {
            return Err(Box::new(EvalAltResult::ErrorParsing(
                ParseErrorType::WrongFnDefinition.into_err(pos),
            )));
        }

        let statements = mem::take(ast.statements_mut());
        let ast = AST::new(statements, lib.clone());

        // Evaluate the AST
        let (result, operations) = self
            .eval_ast_with_scope_raw(scope, &ast)
            .map_err(|err| err.new_position(pos))?;

        state.operations += operations;
        self.inc_operations(state, pos)?;

        return Ok(result);
    }

    /// Chain-evaluate a dot/index chain.
    fn eval_dot_index_chain_helper(
        &self,
        state: &mut State,
        lib: &FunctionsLib,
        target: &mut Target,
        rhs: &Expr,
        idx_values: &mut StaticVec<Dynamic>,
        is_index: bool,
        op_pos: Position,
        level: usize,
        mut new_val: Option<Dynamic>,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        let is_ref = target.is_ref();

        // Get a reference to the mutation target Dynamic
        let obj = target.as_mut();

        // Pop the last index value
        let mut idx_val = idx_values.pop();

        if is_index {
            let pos = rhs.position();

            match rhs {
                // xxx[idx].expr... | xxx[idx][expr]...
                Expr::Dot(x) | Expr::Index(x) => {
                    let (idx, expr, pos) = x.as_ref();
                    let is_idx = matches!(rhs, Expr::Index(_));
                    let idx_pos = idx.position();
                    let this_ptr = &mut self.get_indexed_mut(
                        state, lib, obj, is_ref, idx_val, idx_pos, op_pos, false,
                    )?;

                    self.eval_dot_index_chain_helper(
                        state, lib, this_ptr, expr, idx_values, is_idx, *pos, level, new_val,
                    )
                }
                // xxx[rhs] = new_val
                _ if new_val.is_some() => {
                    let this_ptr = &mut self
                        .get_indexed_mut(state, lib, obj, is_ref, idx_val, pos, op_pos, true)?;

                    this_ptr.set_value(new_val.unwrap(), rhs.position())?;
                    Ok((Default::default(), true))
                }
                // xxx[rhs]
                _ => self
                    .get_indexed_mut(state, lib, obj, is_ref, idx_val, pos, op_pos, false)
                    .map(|v| (v.clone_into_dynamic(), false)),
            }
        } else {
            match rhs {
                // xxx.fn_name(arg_expr_list)
                Expr::FnCall(x) if x.1.is_none() => {
                    let ((name, native, pos), _, hash, _, def_val) = x.as_ref();
                    let def_val = def_val.as_ref();

                    let mut arg_values: StaticVec<_> = once(obj)
                        .chain(
                            idx_val
                                .downcast_mut::<StaticVec<Dynamic>>()
                                .unwrap()
                                .iter_mut(),
                        )
                        .collect();
                    let args = arg_values.as_mut();

                    self.exec_fn_call(
                        state, lib, name, *native, *hash, args, is_ref, def_val, *pos, 0,
                    )
                }
                // xxx.module::fn_name(...) - syntax error
                Expr::FnCall(_) => unreachable!(),
                // {xxx:map}.id = ???
                #[cfg(not(feature = "no_object"))]
                Expr::Property(x) if obj.is::<Map>() && new_val.is_some() => {
                    let ((prop, _, _), pos) = x.as_ref();
                    let index = prop.clone().into();
                    let mut val =
                        self.get_indexed_mut(state, lib, obj, is_ref, index, *pos, op_pos, true)?;

                    val.set_value(new_val.unwrap(), rhs.position())?;
                    Ok((Default::default(), true))
                }
                // {xxx:map}.id
                #[cfg(not(feature = "no_object"))]
                Expr::Property(x) if obj.is::<Map>() => {
                    let ((prop, _, _), pos) = x.as_ref();
                    let index = prop.clone().into();
                    let val =
                        self.get_indexed_mut(state, lib, obj, is_ref, index, *pos, op_pos, false)?;

                    Ok((val.clone_into_dynamic(), false))
                }
                // xxx.id = ???
                Expr::Property(x) if new_val.is_some() => {
                    let ((_, _, setter), pos) = x.as_ref();
                    let mut args = [obj, new_val.as_mut().unwrap()];
                    self.exec_fn_call(
                        state, lib, setter, true, 0, &mut args, is_ref, None, *pos, 0,
                    )
                    .map(|(v, _)| (v, true))
                }
                // xxx.id
                Expr::Property(x) => {
                    let ((_, getter, _), pos) = x.as_ref();
                    let mut args = [obj];
                    self.exec_fn_call(
                        state, lib, getter, true, 0, &mut args, is_ref, None, *pos, 0,
                    )
                    .map(|(v, _)| (v, false))
                }
                #[cfg(not(feature = "no_object"))]
                // {xxx:map}.prop[expr] | {xxx:map}.prop.expr
                Expr::Index(x) | Expr::Dot(x) if obj.is::<Map>() => {
                    let (prop, expr, pos) = x.as_ref();
                    let is_idx = matches!(rhs, Expr::Index(_));

                    let mut val = if let Expr::Property(p) = prop {
                        let ((prop, _, _), _) = p.as_ref();
                        let index = prop.clone().into();
                        self.get_indexed_mut(state, lib, obj, is_ref, index, *pos, op_pos, false)?
                    } else {
                        unreachable!();
                    };

                    self.eval_dot_index_chain_helper(
                        state, lib, &mut val, expr, idx_values, is_idx, *pos, level, new_val,
                    )
                }
                // xxx.prop[expr] | xxx.prop.expr
                Expr::Index(x) | Expr::Dot(x) => {
                    let (prop, expr, pos) = x.as_ref();
                    let is_idx = matches!(rhs, Expr::Index(_));
                    let args = &mut [obj, &mut Default::default()];

                    let (mut val, updated) = if let Expr::Property(p) = prop {
                        let ((_, getter, _), _) = p.as_ref();
                        let args = &mut args[..1];
                        self.exec_fn_call(state, lib, getter, true, 0, args, is_ref, None, *pos, 0)?
                    } else {
                        unreachable!();
                    };
                    let val = &mut val;
                    let target = &mut val.into();

                    let (result, may_be_changed) = self.eval_dot_index_chain_helper(
                        state, lib, target, expr, idx_values, is_idx, *pos, level, new_val,
                    )?;

                    // Feed the value back via a setter just in case it has been updated
                    if updated || may_be_changed {
                        if let Expr::Property(p) = prop {
                            let ((_, _, setter), _) = p.as_ref();
                            // Re-use args because the first &mut parameter will not be consumed
                            args[1] = val;
                            self.exec_fn_call(
                                state, lib, setter, true, 0, args, is_ref, None, *pos, 0,
                            )
                            .or_else(|err| match *err {
                                // If there is no setter, no need to feed it back because the property is read-only
                                EvalAltResult::ErrorDotExpr(_, _) => Ok(Default::default()),
                                err => Err(Box::new(err)),
                            })?;
                        }
                    }

                    Ok((result, may_be_changed))
                }
                // Syntax error
                _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                    "".into(),
                    rhs.position(),
                ))),
            }
        }
    }

    /// Evaluate a dot/index chain.
    fn eval_dot_index_chain(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        expr: &Expr,
        level: usize,
        new_val: Option<Dynamic>,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let ((dot_lhs, dot_rhs, op_pos), is_index) = match expr {
            Expr::Index(x) => (x.as_ref(), true),
            Expr::Dot(x) => (x.as_ref(), false),
            _ => unreachable!(),
        };

        let idx_values = &mut StaticVec::new();

        self.eval_indexed_chain(scope, state, lib, dot_rhs, idx_values, 0, level)?;

        match dot_lhs {
            // id.??? or id[???]
            Expr::Variable(_) => {
                let (target, name, typ, pos) = search_scope(scope, state, dot_lhs)?;
                self.inc_operations(state, pos)?;

                // Constants cannot be modified
                match typ {
                    ScopeEntryType::Module => unreachable!(),
                    ScopeEntryType::Constant if new_val.is_some() => {
                        return Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                            name.to_string(),
                            pos,
                        )));
                    }
                    ScopeEntryType::Constant | ScopeEntryType::Normal => (),
                }

                let this_ptr = &mut target.into();
                self.eval_dot_index_chain_helper(
                    state, lib, this_ptr, dot_rhs, idx_values, is_index, *op_pos, level, new_val,
                )
                .map(|(v, _)| v)
            }
            // {expr}.??? = ??? or {expr}[???] = ???
            expr if new_val.is_some() => {
                return Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(
                    expr.position(),
                )));
            }
            // {expr}.??? or {expr}[???]
            expr => {
                let val = self.eval_expr(scope, state, lib, expr, level)?;
                let this_ptr = &mut val.into();
                self.eval_dot_index_chain_helper(
                    state, lib, this_ptr, dot_rhs, idx_values, is_index, *op_pos, level, new_val,
                )
                .map(|(v, _)| v)
            }
        }
    }

    /// Evaluate a chain of indexes and store the results in a list.
    /// The first few results are stored in the array `list` which is of fixed length.
    /// Any spill-overs are stored in `more`, which is dynamic.
    /// The fixed length array is used to avoid an allocation in the overwhelming cases of just a few levels of indexing.
    /// The total number of values is returned.
    fn eval_indexed_chain(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        expr: &Expr,
        idx_values: &mut StaticVec<Dynamic>,
        size: usize,
        level: usize,
    ) -> Result<(), Box<EvalAltResult>> {
        self.inc_operations(state, expr.position())?;

        match expr {
            Expr::FnCall(x) if x.1.is_none() => {
                let arg_values =
                    x.3.iter()
                        .map(|arg_expr| self.eval_expr(scope, state, lib, arg_expr, level))
                        .collect::<Result<StaticVec<Dynamic>, _>>()?;

                idx_values.push(Dynamic::from(arg_values));
            }
            Expr::FnCall(_) => unreachable!(),
            Expr::Property(_) => idx_values.push(()), // Store a placeholder - no need to copy the property name
            Expr::Index(x) | Expr::Dot(x) => {
                let (lhs, rhs, _) = x.as_ref();

                // Evaluate in left-to-right order
                let lhs_val = match lhs {
                    Expr::Property(_) => Default::default(), // Store a placeholder in case of a property
                    _ => self.eval_expr(scope, state, lib, lhs, level)?,
                };

                // Push in reverse order
                self.eval_indexed_chain(scope, state, lib, rhs, idx_values, size, level)?;

                idx_values.push(lhs_val);
            }
            _ => idx_values.push(self.eval_expr(scope, state, lib, expr, level)?),
        }

        Ok(())
    }

    /// Get the value at the indexed position of a base type
    fn get_indexed_mut<'a>(
        &self,
        state: &mut State,
        lib: &FunctionsLib,
        val: &'a mut Dynamic,
        is_ref: bool,
        mut idx: Dynamic,
        idx_pos: Position,
        op_pos: Position,
        create: bool,
    ) -> Result<Target<'a>, Box<EvalAltResult>> {
        self.inc_operations(state, op_pos)?;

        match val {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(arr)) => {
                // val_array[idx]
                let index = idx
                    .as_int()
                    .map_err(|_| EvalAltResult::ErrorNumericIndexExpr(idx_pos))?;

                let arr_len = arr.len();

                if index >= 0 {
                    arr.get_mut(index as usize)
                        .map(Target::from)
                        .ok_or_else(|| {
                            Box::new(EvalAltResult::ErrorArrayBounds(arr_len, index, idx_pos))
                        })
                } else {
                    Err(Box::new(EvalAltResult::ErrorArrayBounds(
                        arr_len, index, idx_pos,
                    )))
                }
            }

            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(map)) => {
                // val_map[idx]
                Ok(if create {
                    let index = idx
                        .take_string()
                        .map_err(|_| EvalAltResult::ErrorStringIndexExpr(idx_pos))?;

                    map.entry(index).or_insert(Default::default()).into()
                } else {
                    let index = idx
                        .downcast_ref::<String>()
                        .ok_or_else(|| EvalAltResult::ErrorStringIndexExpr(idx_pos))?;

                    map.get_mut(index)
                        .map(Target::from)
                        .unwrap_or_else(|| Target::from(()))
                })
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Str(s)) => {
                // val_string[idx]
                let chars_len = s.chars().count();
                let index = idx
                    .as_int()
                    .map_err(|_| EvalAltResult::ErrorNumericIndexExpr(idx_pos))?;

                if index >= 0 {
                    let offset = index as usize;
                    let ch = s.chars().nth(offset).ok_or_else(|| {
                        Box::new(EvalAltResult::ErrorStringBounds(chars_len, index, idx_pos))
                    })?;
                    Ok(Target::StringChar(val, offset, ch.into()))
                } else {
                    Err(Box::new(EvalAltResult::ErrorStringBounds(
                        chars_len, index, idx_pos,
                    )))
                }
            }

            _ => {
                let fn_name = FUNC_INDEXER;
                let type_name = self.map_type_name(val.type_name());
                let args = &mut [val, &mut idx];
                self.exec_fn_call(state, lib, fn_name, true, 0, args, is_ref, None, op_pos, 0)
                    .map(|(v, _)| v.into())
                    .map_err(|_| {
                        Box::new(EvalAltResult::ErrorIndexingType(type_name.into(), op_pos))
                    })
            }
        }
    }

    // Evaluate an 'in' expression
    fn eval_in_expr(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        lhs: &Expr,
        rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, rhs.position())?;

        let lhs_value = self.eval_expr(scope, state, lib, lhs, level)?;
        let rhs_value = self.eval_expr(scope, state, lib, rhs, level)?;

        match rhs_value {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(mut rhs_value)) => {
                let op = "==";
                let def_value = false.into();
                let mut scope = Scope::new();

                // Call the `==` operator to compare each value
                for value in rhs_value.iter_mut() {
                    let args = &mut [&mut lhs_value.clone(), value];
                    let def_value = Some(&def_value);
                    let pos = rhs.position();

                    let hashes = (
                        // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
                        calc_fn_hash(empty(), op, args.len(), args.iter().map(|a| a.type_id())),
                        0,
                    );

                    let (r, _) = self.call_fn_raw(
                        &mut scope, state, lib, op, hashes, args, false, def_value, pos, level,
                    )?;
                    if r.as_bool().unwrap_or(false) {
                        return Ok(true.into());
                    }
                }

                Ok(false.into())
            }
            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(rhs_value)) => match lhs_value {
                // Only allows String or char
                Dynamic(Union::Str(s)) => Ok(rhs_value.contains_key(s.as_str()).into()),
                Dynamic(Union::Char(c)) => Ok(rhs_value.contains_key(&c.to_string()).into()),
                _ => Err(Box::new(EvalAltResult::ErrorInExpr(lhs.position()))),
            },
            Dynamic(Union::Str(rhs_value)) => match lhs_value {
                // Only allows String or char
                Dynamic(Union::Str(s)) => Ok(rhs_value.contains(s.as_str()).into()),
                Dynamic(Union::Char(c)) => Ok(rhs_value.contains(c).into()),
                _ => Err(Box::new(EvalAltResult::ErrorInExpr(lhs.position()))),
            },
            _ => Err(Box::new(EvalAltResult::ErrorInExpr(rhs.position()))),
        }
    }

    /// Evaluate an expression
    fn eval_expr(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        expr: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, expr.position())?;

        match expr {
            Expr::Expr(x) => self.eval_expr(scope, state, lib, x.as_ref(), level),

            Expr::IntegerConstant(x) => Ok(x.0.into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(x) => Ok(x.0.into()),
            Expr::StringConstant(x) => Ok(x.0.to_string().into()),
            Expr::CharConstant(x) => Ok(x.0.into()),
            Expr::Variable(_) => {
                let (val, _, _, _) = search_scope(scope, state, expr)?;
                Ok(val.clone())
            }
            Expr::Property(_) => unreachable!(),

            // Statement block
            Expr::Stmt(stmt) => self.eval_stmt(scope, state, lib, &stmt.0, level),

            // var op= rhs
            Expr::Assignment(x) if matches!(x.0, Expr::Variable(_)) => {
                let (lhs_expr, op, rhs_expr, op_pos) = x.as_ref();
                let mut rhs_val = self.eval_expr(scope, state, lib, rhs_expr, level)?;
                let (lhs_ptr, name, typ, pos) = search_scope(scope, state, lhs_expr)?;
                self.inc_operations(state, pos)?;

                match typ {
                    // Assignment to constant variable
                    ScopeEntryType::Constant => Err(Box::new(
                        EvalAltResult::ErrorAssignmentToConstant(name.to_string(), pos),
                    )),
                    // Normal assignment
                    ScopeEntryType::Normal if op.is_empty() => {
                        *lhs_ptr = rhs_val;
                        Ok(Default::default())
                    }
                    // Op-assignment - in order of precedence:
                    ScopeEntryType::Normal => {
                        // 1) Native registered overriding function
                        // 2) Built-in implementation
                        // 3) Map to `var = var op rhs`

                        // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
                        let arg_types = once(lhs_ptr.type_id()).chain(once(rhs_val.type_id()));
                        let hash_fn = calc_fn_hash(empty(), op, 2, arg_types);

                        if let Some(CallableFunction::Method(func)) = self
                            .global_module
                            .get_fn(hash_fn)
                            .or_else(|| self.packages.get_fn(hash_fn))
                        {
                            // Overriding exact implementation
                            func(&mut [lhs_ptr, &mut rhs_val])?;
                        } else if run_builtin_op_assignment(op, lhs_ptr, &rhs_val)?.is_none() {
                            // Not built in, map to `var = var op rhs`
                            let op = &op[..op.len() - 1]; // extract operator without =
                            let hash = calc_fn_hash(empty(), op, 2, empty());
                            let args = &mut [&mut lhs_ptr.clone(), &mut rhs_val];

                            // Set variable value
                            *lhs_ptr = self
                                .exec_fn_call(
                                    state, lib, op, true, hash, args, false, None, *op_pos, level,
                                )
                                .map(|(v, _)| v)?;
                        }
                        Ok(Default::default())
                    }
                    // A module cannot be assigned to
                    ScopeEntryType::Module => unreachable!(),
                }
            }

            // lhs op= rhs
            Expr::Assignment(x) => {
                let (lhs_expr, op, rhs_expr, op_pos) = x.as_ref();
                let mut rhs_val = self.eval_expr(scope, state, lib, rhs_expr, level)?;

                let new_val = Some(if op.is_empty() {
                    // Normal assignment
                    rhs_val
                } else {
                    // Op-assignment - always map to `lhs = lhs op rhs`
                    let op = &op[..op.len() - 1]; // extract operator without =
                    let hash = calc_fn_hash(empty(), op, 2, empty());
                    let args = &mut [
                        &mut self.eval_expr(scope, state, lib, lhs_expr, level)?,
                        &mut rhs_val,
                    ];
                    self.exec_fn_call(
                        state, lib, op, true, hash, args, false, None, *op_pos, level,
                    )
                    .map(|(v, _)| v)?
                });

                match lhs_expr {
                    // name op= rhs
                    Expr::Variable(_) => unreachable!(),
                    // idx_lhs[idx_expr] op= rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(_) => {
                        self.eval_dot_index_chain(scope, state, lib, lhs_expr, level, new_val)
                    }
                    // dot_lhs.dot_rhs op= rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(_) => {
                        self.eval_dot_index_chain(scope, state, lib, lhs_expr, level, new_val)
                    }
                    // Error assignment to constant
                    expr if expr.is_constant() => {
                        Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                            expr.get_constant_str(),
                            expr.position(),
                        )))
                    }
                    // Syntax error
                    expr => Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(
                        expr.position(),
                    ))),
                }
            }

            // lhs[idx_expr]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(_) => self.eval_dot_index_chain(scope, state, lib, expr, level, None),

            // lhs.dot_rhs
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(_) => self.eval_dot_index_chain(scope, state, lib, expr, level, None),

            #[cfg(not(feature = "no_index"))]
            Expr::Array(x) => Ok(Dynamic(Union::Array(Box::new(
                x.0.iter()
                    .map(|item| self.eval_expr(scope, state, lib, item, level))
                    .collect::<Result<Vec<_>, _>>()?,
            )))),

            #[cfg(not(feature = "no_object"))]
            Expr::Map(x) => Ok(Dynamic(Union::Map(Box::new(
                x.0.iter()
                    .map(|((key, _), expr)| {
                        self.eval_expr(scope, state, lib, expr, level)
                            .map(|val| (key.clone(), val))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?,
            )))),

            // Normal function call
            Expr::FnCall(x) if x.1.is_none() => {
                let ((name, native, pos), _, hash, args_expr, def_val) = x.as_ref();
                let def_val = def_val.as_ref();

                // Handle eval
                if name == KEYWORD_EVAL && args_expr.len() == 1 {
                    let hash_fn =
                        calc_fn_hash(empty(), name, 1, once(TypeId::of::<ImmutableString>()));

                    if !self.has_override(lib, (hash_fn, *hash)) {
                        // eval - only in function call style
                        let prev_len = scope.len();
                        let pos = args_expr.get(0).position();

                        // Evaluate the text string as a script
                        let script = self.eval_expr(scope, state, lib, args_expr.get(0), level)?;
                        let result = self.eval_script_expr(scope, state, lib, &script, pos);

                        if scope.len() != prev_len {
                            // IMPORTANT! If the eval defines new variables in the current scope,
                            //            all variable offsets from this point on will be mis-aligned.
                            state.always_search = true;
                        }

                        return result;
                    }
                }

                // Normal function call - except for eval (handled above)
                let mut arg_values: StaticVec<Dynamic>;
                let mut args: StaticVec<_>;
                let mut is_ref = false;

                if args_expr.is_empty() {
                    // No arguments
                    args = Default::default();
                } else {
                    // See if the first argument is a variable, if so, convert to method-call style
                    // in order to leverage potential &mut first argument and avoid cloning the value
                    match args_expr.get(0) {
                        // func(x, ...) -> x.func(...)
                        lhs @ Expr::Variable(_) => {
                            arg_values = args_expr
                                .iter()
                                .skip(1)
                                .map(|expr| self.eval_expr(scope, state, lib, expr, level))
                                .collect::<Result<_, _>>()?;

                            let (target, _, typ, pos) = search_scope(scope, state, lhs)?;
                            self.inc_operations(state, pos)?;

                            match typ {
                                ScopeEntryType::Module => unreachable!(),
                                ScopeEntryType::Constant | ScopeEntryType::Normal => (),
                            }

                            args = once(target).chain(arg_values.iter_mut()).collect();

                            is_ref = true;
                        }
                        // func(..., ...)
                        _ => {
                            arg_values = args_expr
                                .iter()
                                .map(|expr| self.eval_expr(scope, state, lib, expr, level))
                                .collect::<Result<_, _>>()?;

                            args = arg_values.iter_mut().collect();
                        }
                    }
                }

                let args = args.as_mut();
                self.exec_fn_call(
                    state, lib, name, *native, *hash, args, is_ref, def_val, *pos, level,
                )
                .map(|(v, _)| v)
            }

            // Module-qualified function call
            #[cfg(not(feature = "no_module"))]
            Expr::FnCall(x) if x.1.is_some() => {
                let ((name, _, pos), modules, hash_fn_def, args_expr, def_val) = x.as_ref();
                let modules = modules.as_ref().unwrap();

                let mut arg_values = args_expr
                    .iter()
                    .map(|expr| self.eval_expr(scope, state, lib, expr, level))
                    .collect::<Result<StaticVec<_>, _>>()?;

                let mut args: StaticVec<_> = arg_values.iter_mut().collect();

                let (id, root_pos) = modules.get(0); // First module

                let module = if let Some(index) = modules.index() {
                    scope
                        .get_mut(scope.len() - index.get())
                        .0
                        .downcast_mut::<Module>()
                        .unwrap()
                } else {
                    scope.find_module(id).ok_or_else(|| {
                        Box::new(EvalAltResult::ErrorModuleNotFound(id.into(), *root_pos))
                    })?
                };

                // First search in script-defined functions (can override built-in)
                let func = match module.get_qualified_fn(name, *hash_fn_def) {
                    Err(err) if matches!(*err, EvalAltResult::ErrorFunctionNotFound(_, _)) => {
                        // Then search in Rust functions
                        self.inc_operations(state, *pos)?;

                        // Rust functions are indexed in two steps:
                        // 1) Calculate a hash in a similar manner to script-defined functions,
                        //    i.e. qualifiers + function name + number of arguments.
                        // 2) Calculate a second hash with no qualifiers, empty function name,
                        //    zero number of arguments, and the actual list of argument `TypeId`'.s
                        let hash_fn_args =
                            calc_fn_hash(empty(), "", 0, args.iter().map(|a| a.type_id()));
                        // 3) The final hash is the XOR of the two hashes.
                        let hash_fn_native = *hash_fn_def ^ hash_fn_args;

                        module.get_qualified_fn(name, hash_fn_native)
                    }
                    r => r,
                };

                match func {
                    Ok(x) if x.is_script() => {
                        let args = args.as_mut();
                        let fn_def = x.get_fn_def();
                        let mut scope = Scope::new();
                        self.call_script_fn(&mut scope, state, lib, name, fn_def, args, *pos, level)
                    }
                    Ok(x) => x.get_native_fn()(args.as_mut()).map_err(|err| err.new_position(*pos)),
                    Err(err)
                        if def_val.is_some()
                            && matches!(*err, EvalAltResult::ErrorFunctionNotFound(_, _)) =>
                    {
                        Ok(def_val.clone().unwrap())
                    }
                    Err(err) => Err(err),
                }
            }

            Expr::In(x) => self.eval_in_expr(scope, state, lib, &x.0, &x.1, level),

            Expr::And(x) => {
                let (lhs, rhs, _) = x.as_ref();
                Ok((self
                    .eval_expr(scope, state, lib, lhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), lhs.position())
                    })?
                    && // Short-circuit using &&
                self
                    .eval_expr(scope, state, lib, rhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), rhs.position())
                    })?)
                .into())
            }

            Expr::Or(x) => {
                let (lhs, rhs, _) = x.as_ref();
                Ok((self
                    .eval_expr(scope, state, lib, lhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), lhs.position())
                    })?
                    || // Short-circuit using ||
                self
                    .eval_expr(scope, state, lib, rhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), rhs.position())
                    })?)
                .into())
            }

            Expr::True(_) => Ok(true.into()),
            Expr::False(_) => Ok(false.into()),
            Expr::Unit(_) => Ok(().into()),

            _ => unreachable!(),
        }
    }

    /// Evaluate a statement
    pub(crate) fn eval_stmt(
        &self,
        scope: &mut Scope,
        state: &mut State,
        lib: &FunctionsLib,
        stmt: &Stmt,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, stmt.position())?;

        match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Default::default()),

            // Expression as statement
            Stmt::Expr(expr) => {
                let result = self.eval_expr(scope, state, lib, expr, level)?;

                Ok(match expr.as_ref() {
                    // If it is a simple assignment, erase the result at the root
                    Expr::Assignment(_) => Default::default(),
                    _ => result,
                })
            }

            // Block scope
            Stmt::Block(x) => {
                let prev_len = scope.len();
                state.scope_level += 1;

                let result = x.0.iter().try_fold(Default::default(), |_, stmt| {
                    self.eval_stmt(scope, state, lib, stmt, level)
                });

                scope.rewind(prev_len);
                state.scope_level -= 1;

                // The impact of an eval statement goes away at the end of a block
                // because any new variables introduced will go out of scope
                state.always_search = false;

                result
            }

            // If-else statement
            Stmt::IfThenElse(x) => {
                let (expr, if_block, else_block) = x.as_ref();

                self.eval_expr(scope, state, lib, expr, level)?
                    .as_bool()
                    .map_err(|_| Box::new(EvalAltResult::ErrorLogicGuard(expr.position())))
                    .and_then(|guard_val| {
                        if guard_val {
                            self.eval_stmt(scope, state, lib, if_block, level)
                        } else if let Some(stmt) = else_block {
                            self.eval_stmt(scope, state, lib, stmt, level)
                        } else {
                            Ok(Default::default())
                        }
                    })
            }

            // While loop
            Stmt::While(x) => loop {
                let (expr, body) = x.as_ref();

                match self.eval_expr(scope, state, lib, expr, level)?.as_bool() {
                    Ok(true) => match self.eval_stmt(scope, state, lib, body, level) {
                        Ok(_) => (),
                        Err(err) => match *err {
                            EvalAltResult::ErrorLoopBreak(false, _) => (),
                            EvalAltResult::ErrorLoopBreak(true, _) => return Ok(Default::default()),
                            _ => return Err(err),
                        },
                    },
                    Ok(false) => return Ok(Default::default()),
                    Err(_) => {
                        return Err(Box::new(EvalAltResult::ErrorLogicGuard(expr.position())))
                    }
                }
            },

            // Loop statement
            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, state, lib, body, level) {
                    Ok(_) => (),
                    Err(err) => match *err {
                        EvalAltResult::ErrorLoopBreak(false, _) => (),
                        EvalAltResult::ErrorLoopBreak(true, _) => return Ok(Default::default()),
                        _ => return Err(err),
                    },
                }
            },

            // For loop
            Stmt::For(x) => {
                let (name, expr, stmt) = x.as_ref();
                let iter_type = self.eval_expr(scope, state, lib, expr, level)?;
                let tid = iter_type.type_id();

                if let Some(func) = self
                    .global_module
                    .get_iter(tid)
                    .or_else(|| self.packages.get_iter(tid))
                {
                    // Add the loop variable
                    let var_name = unsafe_cast_var_name_to_lifetime(name, &state);
                    scope.push(var_name, ());
                    let index = scope.len() - 1;
                    state.scope_level += 1;

                    for loop_var in func(iter_type) {
                        *scope.get_mut(index).0 = loop_var;
                        self.inc_operations(state, stmt.position())?;

                        match self.eval_stmt(scope, state, lib, stmt, level) {
                            Ok(_) => (),
                            Err(err) => match *err {
                                EvalAltResult::ErrorLoopBreak(false, _) => (),
                                EvalAltResult::ErrorLoopBreak(true, _) => break,
                                _ => return Err(err),
                            },
                        }
                    }

                    scope.rewind(scope.len() - 1);
                    state.scope_level -= 1;
                    Ok(Default::default())
                } else {
                    Err(Box::new(EvalAltResult::ErrorFor(x.1.position())))
                }
            }

            // Continue statement
            Stmt::Continue(pos) => Err(Box::new(EvalAltResult::ErrorLoopBreak(false, *pos))),

            // Break statement
            Stmt::Break(pos) => Err(Box::new(EvalAltResult::ErrorLoopBreak(true, *pos))),

            // Return value
            Stmt::ReturnWithVal(x) if x.1.is_some() && (x.0).0 == ReturnType::Return => {
                Err(Box::new(EvalAltResult::Return(
                    self.eval_expr(scope, state, lib, x.1.as_ref().unwrap(), level)?,
                    (x.0).1,
                )))
            }

            // Empty return
            Stmt::ReturnWithVal(x) if (x.0).0 == ReturnType::Return => {
                Err(Box::new(EvalAltResult::Return(Default::default(), (x.0).1)))
            }

            // Throw value
            Stmt::ReturnWithVal(x) if x.1.is_some() && (x.0).0 == ReturnType::Exception => {
                let val = self.eval_expr(scope, state, lib, x.1.as_ref().unwrap(), level)?;
                Err(Box::new(EvalAltResult::ErrorRuntime(
                    val.take_string().unwrap_or_else(|_| "".into()),
                    (x.0).1,
                )))
            }

            // Empty throw
            Stmt::ReturnWithVal(x) if (x.0).0 == ReturnType::Exception => {
                Err(Box::new(EvalAltResult::ErrorRuntime("".into(), (x.0).1)))
            }

            Stmt::ReturnWithVal(_) => unreachable!(),

            // Let statement
            Stmt::Let(x) if x.1.is_some() => {
                let ((var_name, _), expr) = x.as_ref();
                let val = self.eval_expr(scope, state, lib, expr.as_ref().unwrap(), level)?;
                let var_name = unsafe_cast_var_name_to_lifetime(var_name, &state);
                scope.push_dynamic_value(var_name, ScopeEntryType::Normal, val, false);
                Ok(Default::default())
            }

            Stmt::Let(x) => {
                let ((var_name, _), _) = x.as_ref();
                let var_name = unsafe_cast_var_name_to_lifetime(var_name, &state);
                scope.push(var_name, ());
                Ok(Default::default())
            }

            // Const statement
            Stmt::Const(x) if x.1.is_constant() => {
                let ((var_name, _), expr) = x.as_ref();
                let val = self.eval_expr(scope, state, lib, &expr, level)?;
                let var_name = unsafe_cast_var_name_to_lifetime(var_name, &state);
                scope.push_dynamic_value(var_name, ScopeEntryType::Constant, val, true);
                Ok(Default::default())
            }

            // Const expression not constant
            Stmt::Const(_) => unreachable!(),

            // Import statement
            Stmt::Import(x) => {
                #[cfg(feature = "no_module")]
                unreachable!();

                #[cfg(not(feature = "no_module"))]
                {
                    let (expr, (name, pos)) = x.as_ref();

                    // Guard against too many modules
                    if state.modules >= self.max_modules {
                        return Err(Box::new(EvalAltResult::ErrorTooManyModules(*pos)));
                    }

                    if let Some(path) = self
                        .eval_expr(scope, state, lib, &expr, level)?
                        .try_cast::<ImmutableString>()
                    {
                        if let Some(resolver) = &self.module_resolver {
                            // Use an empty scope to create a module
                            let module =
                                resolver.resolve(self, Scope::new(), &path, expr.position())?;

                            let mod_name = unsafe_cast_var_name_to_lifetime(name, &state);
                            scope.push_module(mod_name, module);

                            state.modules += 1;

                            Ok(Default::default())
                        } else {
                            Err(Box::new(EvalAltResult::ErrorModuleNotFound(
                                path.to_string(),
                                expr.position(),
                            )))
                        }
                    } else {
                        Err(Box::new(EvalAltResult::ErrorImportExpr(expr.position())))
                    }
                }
            }

            // Export statement
            Stmt::Export(list) => {
                for ((id, id_pos), rename) in list.iter() {
                    // Mark scope variables as public
                    if let Some(index) = scope
                        .get_index(id)
                        .map(|(i, _)| i)
                        .or_else(|| scope.get_module_index(id))
                    {
                        let alias = rename
                            .as_ref()
                            .map(|(n, _)| n.clone())
                            .unwrap_or_else(|| id.clone());

                        scope.set_entry_alias(index, alias);
                    } else {
                        return Err(Box::new(EvalAltResult::ErrorVariableNotFound(
                            id.into(),
                            *id_pos,
                        )));
                    }
                }
                Ok(Default::default())
            }
        }
    }

    /// Check if the number of operations stay within limit.
    fn inc_operations(&self, state: &mut State, pos: Position) -> Result<(), Box<EvalAltResult>> {
        state.operations += 1;

        #[cfg(not(feature = "unchecked"))]
        {
            // Guard against too many operations
            if state.operations > self.max_operations {
                return Err(Box::new(EvalAltResult::ErrorTooManyOperations(pos)));
            }
        }

        // Report progress - only in steps
        if let Some(progress) = &self.progress {
            if !progress(state.operations) {
                // Terminate script if progress returns false
                return Err(Box::new(EvalAltResult::ErrorTerminated(pos)));
            }
        }

        Ok(())
    }

    /// Map a type_name into a pretty-print name
    pub(crate) fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .get(name)
            .map(String::as_str)
            .unwrap_or(name)
    }
}

/// Build in common binary operator implementations to avoid the cost of calling a registered function.
fn run_builtin_binary_op(
    op: &str,
    x: &Dynamic,
    y: &Dynamic,
) -> Result<Option<Dynamic>, Box<EvalAltResult>> {
    use crate::packages::arithmetic::*;

    let args_type = x.type_id();

    if y.type_id() != args_type {
        return Ok(None);
    }

    if args_type == TypeId::of::<INT>() {
        let x = *x.downcast_ref::<INT>().unwrap();
        let y = *y.downcast_ref::<INT>().unwrap();

        #[cfg(not(feature = "unchecked"))]
        match op {
            "+" => return add(x, y).map(Into::<Dynamic>::into).map(Some),
            "-" => return sub(x, y).map(Into::<Dynamic>::into).map(Some),
            "*" => return mul(x, y).map(Into::<Dynamic>::into).map(Some),
            "/" => return div(x, y).map(Into::<Dynamic>::into).map(Some),
            "%" => return modulo(x, y).map(Into::<Dynamic>::into).map(Some),
            "~" => return pow_i_i(x, y).map(Into::<Dynamic>::into).map(Some),
            ">>" => return shr(x, y).map(Into::<Dynamic>::into).map(Some),
            "<<" => return shl(x, y).map(Into::<Dynamic>::into).map(Some),
            _ => (),
        }

        #[cfg(feature = "unchecked")]
        match op {
            "+" => return Ok(Some((x + y).into())),
            "-" => return Ok(Some((x - y).into())),
            "*" => return Ok(Some((x * y).into())),
            "/" => return Ok(Some((x / y).into())),
            "%" => return Ok(Some((x % y).into())),
            "~" => return pow_i_i_u(x, y).map(Into::<Dynamic>::into).map(Some),
            ">>" => return shr_u(x, y).map(Into::<Dynamic>::into).map(Some),
            "<<" => return shl_u(x, y).map(Into::<Dynamic>::into).map(Some),
            _ => (),
        }

        match op {
            "==" => return Ok(Some((x == y).into())),
            "!=" => return Ok(Some((x != y).into())),
            ">" => return Ok(Some((x > y).into())),
            ">=" => return Ok(Some((x >= y).into())),
            "<" => return Ok(Some((x < y).into())),
            "<=" => return Ok(Some((x <= y).into())),
            "&" => return Ok(Some((x & y).into())),
            "|" => return Ok(Some((x | y).into())),
            "^" => return Ok(Some((x ^ y).into())),
            _ => (),
        }
    } else if args_type == TypeId::of::<bool>() {
        let x = *x.downcast_ref::<bool>().unwrap();
        let y = *y.downcast_ref::<bool>().unwrap();

        match op {
            "&" => return Ok(Some((x && y).into())),
            "|" => return Ok(Some((x || y).into())),
            "==" => return Ok(Some((x == y).into())),
            "!=" => return Ok(Some((x != y).into())),
            _ => (),
        }
    } else if args_type == TypeId::of::<ImmutableString>() {
        let x = x.downcast_ref::<ImmutableString>().unwrap();
        let y = y.downcast_ref::<ImmutableString>().unwrap();

        match op {
            "+" => return Ok(Some((x + y).into())),
            "==" => return Ok(Some((x == y).into())),
            "!=" => return Ok(Some((x != y).into())),
            ">" => return Ok(Some((x > y).into())),
            ">=" => return Ok(Some((x >= y).into())),
            "<" => return Ok(Some((x < y).into())),
            "<=" => return Ok(Some((x <= y).into())),
            _ => (),
        }
    } else if args_type == TypeId::of::<char>() {
        let x = *x.downcast_ref::<char>().unwrap();
        let y = *y.downcast_ref::<char>().unwrap();

        match op {
            "==" => return Ok(Some((x == y).into())),
            "!=" => return Ok(Some((x != y).into())),
            ">" => return Ok(Some((x > y).into())),
            ">=" => return Ok(Some((x >= y).into())),
            "<" => return Ok(Some((x < y).into())),
            "<=" => return Ok(Some((x <= y).into())),
            _ => (),
        }
    } else if args_type == TypeId::of::<()>() {
        match op {
            "==" => return Ok(Some(true.into())),
            "!=" | ">" | ">=" | "<" | "<=" => return Ok(Some(false.into())),
            _ => (),
        }
    }

    #[cfg(not(feature = "no_float"))]
    {
        if args_type == TypeId::of::<FLOAT>() {
            let x = *x.downcast_ref::<FLOAT>().unwrap();
            let y = *y.downcast_ref::<FLOAT>().unwrap();

            match op {
                "+" => return Ok(Some((x + y).into())),
                "-" => return Ok(Some((x - y).into())),
                "*" => return Ok(Some((x * y).into())),
                "/" => return Ok(Some((x / y).into())),
                "%" => return Ok(Some((x % y).into())),
                "~" => return pow_f_f(x, y).map(Into::<Dynamic>::into).map(Some),
                "==" => return Ok(Some((x == y).into())),
                "!=" => return Ok(Some((x != y).into())),
                ">" => return Ok(Some((x > y).into())),
                ">=" => return Ok(Some((x >= y).into())),
                "<" => return Ok(Some((x < y).into())),
                "<=" => return Ok(Some((x <= y).into())),
                _ => (),
            }
        }
    }

    Ok(None)
}

/// Build in common operator assignment implementations to avoid the cost of calling a registered function.
fn run_builtin_op_assignment(
    op: &str,
    x: &mut Dynamic,
    y: &Dynamic,
) -> Result<Option<()>, Box<EvalAltResult>> {
    use crate::packages::arithmetic::*;

    let args_type = x.type_id();

    if y.type_id() != args_type {
        return Ok(None);
    }

    if args_type == TypeId::of::<INT>() {
        let x = x.downcast_mut::<INT>().unwrap();
        let y = *y.downcast_ref::<INT>().unwrap();

        #[cfg(not(feature = "unchecked"))]
        match op {
            "+=" => return Ok(Some(*x = add(*x, y)?)),
            "-=" => return Ok(Some(*x = sub(*x, y)?)),
            "*=" => return Ok(Some(*x = mul(*x, y)?)),
            "/=" => return Ok(Some(*x = div(*x, y)?)),
            "%=" => return Ok(Some(*x = modulo(*x, y)?)),
            "~=" => return Ok(Some(*x = pow_i_i(*x, y)?)),
            ">>=" => return Ok(Some(*x = shr(*x, y)?)),
            "<<=" => return Ok(Some(*x = shl(*x, y)?)),
            _ => (),
        }

        #[cfg(feature = "unchecked")]
        match op {
            "+=" => return Ok(Some(*x += y)),
            "-=" => return Ok(Some(*x -= y)),
            "*=" => return Ok(Some(*x *= y)),
            "/=" => return Ok(Some(*x /= y)),
            "%=" => return Ok(Some(*x %= y)),
            "~=" => return Ok(Some(*x = pow_i_i_u(*x, y)?)),
            ">>=" => return Ok(Some(*x = shr_u(*x, y)?)),
            "<<=" => return Ok(Some(*x = shl_u(*x, y)?)),
            _ => (),
        }

        match op {
            "&=" => return Ok(Some(*x &= y)),
            "|=" => return Ok(Some(*x |= y)),
            "^=" => return Ok(Some(*x ^= y)),
            _ => (),
        }
    } else if args_type == TypeId::of::<bool>() {
        let x = x.downcast_mut::<bool>().unwrap();
        let y = *y.downcast_ref::<bool>().unwrap();

        match op {
            "&=" => return Ok(Some(*x = *x && y)),
            "|=" => return Ok(Some(*x = *x || y)),
            _ => (),
        }
    } else if args_type == TypeId::of::<ImmutableString>() {
        let x = x.downcast_mut::<ImmutableString>().unwrap();
        let y = y.downcast_ref::<ImmutableString>().unwrap();

        match op {
            "+=" => return Ok(Some(*x += y)),
            _ => (),
        }
    }

    #[cfg(not(feature = "no_float"))]
    {
        if args_type == TypeId::of::<FLOAT>() {
            let x = x.downcast_mut::<FLOAT>().unwrap();
            let y = *y.downcast_ref::<FLOAT>().unwrap();

            match op {
                "+=" => return Ok(Some(*x += y)),
                "-=" => return Ok(Some(*x -= y)),
                "*=" => return Ok(Some(*x *= y)),
                "/=" => return Ok(Some(*x /= y)),
                "%=" => return Ok(Some(*x %= y)),
                "~=" => return Ok(Some(*x = pow_f_f(*x, y)?)),
                _ => (),
            }
        }
    }

    Ok(None)
}
