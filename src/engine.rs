//! Main module defining the script evaluation `Engine`.

use crate::any::{Dynamic, Union};
use crate::calc_fn_hash;
use crate::error::ParseErrorType;
use crate::fn_native::{FnCallArgs, NativeFunctionABI, PrintCallback, ProgressCallback};
use crate::module::Module;
use crate::optimize::OptimizationLevel;
use crate::packages::{CorePackage, Package, PackageLibrary, PackagesCollection, StandardPackage};
use crate::parser::{Expr, FnAccess, FnDef, ReturnType, SharedFnDef, Stmt, AST};
use crate::r#unsafe::unsafe_cast_var_name;
use crate::result::EvalAltResult;
use crate::scope::{EntryType as ScopeEntryType, Scope};
use crate::token::Position;
use crate::utils::{StaticVec, EMPTY_TYPE_ID};

#[cfg(not(feature = "no_module"))]
use crate::module::{resolvers, ModuleRef, ModuleResolver};

#[cfg(feature = "no_module")]
use crate::parser::ModuleRef;

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::HashMap,
    format,
    iter::{empty, once, repeat},
    mem,
    num::{NonZeroU64, NonZeroUsize},
    ops::{Deref, DerefMut},
    rc::Rc,
    string::{String, ToString},
    sync::Arc,
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
pub const MAX_CALL_STACK_DEPTH: usize = 28;

#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_CALL_STACK_DEPTH: usize = 256;

#[cfg(feature = "unchecked")]
pub const MAX_CALL_STACK_DEPTH: usize = usize::MAX;

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
            Target::StringChar(Dynamic(Union::Str(s)), index, _) => {
                // Replace the character at the specified index position
                let new_ch = new_val
                    .as_char()
                    .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;

                let mut chars: StaticVec<char> = s.chars().collect();
                let ch = *chars.get_ref(*index);

                // See if changed - if so, update the String
                if ch != new_ch {
                    *chars.get_mut(*index) = new_ch;
                    s.clear();
                    chars.iter().for_each(|&ch| s.push(ch));
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
#[derive(Debug, Clone, Copy)]
pub struct State<'a> {
    /// Global script-defined functions.
    pub fn_lib: &'a FunctionsLib,

    /// Normally, access to variables are parsed with a relative offset into the scope to avoid a lookup.
    /// In some situation, e.g. after running an `eval` statement, subsequent offsets may become mis-aligned.
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

impl<'a> State<'a> {
    /// Create a new `State`.
    pub fn new(fn_lib: &'a FunctionsLib) -> Self {
        Self {
            fn_lib,
            always_search: false,
            scope_level: 0,
            operations: 0,
            modules: 0,
        }
    }
    /// Does a certain script-defined function exist in the `State`?
    pub fn has_function(&self, hash: u64) -> bool {
        self.fn_lib.contains_key(&hash)
    }
    /// Get a script-defined function definition from the `State`.
    pub fn get_function(&self, hash: u64) -> Option<&FnDef> {
        self.fn_lib.get(&hash).map(|f| f.as_ref())
    }
}

/// A type that holds a library (`HashMap`) of script-defined functions.
///
/// Since script-defined functions have `Dynamic` parameters, functions with the same name
/// and number of parameters are considered equivalent.
///
/// The key of the `HashMap` is a `u64` hash calculated by the function `calc_fn_hash`
/// with dummy parameter types `EMPTY_TYPE_ID()` repeated the correct number of times.
#[derive(Debug, Clone, Default)]
pub struct FunctionsLib(HashMap<u64, SharedFnDef>);

impl FunctionsLib {
    /// Create a new `FunctionsLib` from a collection of `FnDef`.
    pub fn from_iter(vec: impl IntoIterator<Item = FnDef>) -> Self {
        FunctionsLib(
            vec.into_iter()
                .map(|fn_def| {
                    // Qualifiers (none) + function name + placeholders (one for each parameter).
                    let args_iter = repeat(EMPTY_TYPE_ID()).take(fn_def.params.len());
                    let hash = calc_fn_hash(empty(), &fn_def.name, args_iter);

                    #[cfg(feature = "sync")]
                    {
                        (hash, Arc::new(fn_def))
                    }
                    #[cfg(not(feature = "sync"))]
                    {
                        (hash, Rc::new(fn_def))
                    }
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
        // Qualifiers (none) + function name + placeholders (one for each parameter).
        let hash_fn_def = calc_fn_hash(empty(), name, repeat(EMPTY_TYPE_ID()).take(params));
        let fn_def = self.get_function(hash_fn_def);

        match fn_def.as_ref().map(|f| f.access) {
            None => None,
            Some(FnAccess::Private) if public_only => None,
            Some(FnAccess::Private) => fn_def,
            Some(FnAccess::Public) => fn_def,
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

impl From<Vec<(u64, SharedFnDef)>> for FunctionsLib {
    fn from(values: Vec<(u64, SharedFnDef)>) -> Self {
        FunctionsLib(values.into_iter().collect())
    }
}

impl Deref for FunctionsLib {
    #[cfg(feature = "sync")]
    type Target = HashMap<u64, Arc<FnDef>>;
    #[cfg(not(feature = "sync"))]
    type Target = HashMap<u64, Rc<FnDef>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FunctionsLib {
    #[cfg(feature = "sync")]
    fn deref_mut(&mut self) -> &mut HashMap<u64, Arc<FnDef>> {
        &mut self.0
    }
    #[cfg(not(feature = "sync"))]
    fn deref_mut(&mut self) -> &mut HashMap<u64, Rc<FnDef>> {
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
    pub(crate) print: Box<PrintCallback>,
    /// Closure for implementing the `debug` command.
    pub(crate) debug: Box<PrintCallback>,
    /// Closure for progress reporting.
    pub(crate) progress: Option<Box<ProgressCallback>>,

    /// Optimize the AST after compilation.
    pub(crate) optimization_level: OptimizationLevel,
    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Defaults to 28 for debug builds and 256 for non-debug builds.
    pub(crate) max_call_stack_depth: usize,
    /// Maximum number of operations allowed to run.
    pub(crate) max_operations: Option<NonZeroU64>,
    /// Maximum number of modules allowed to load.
    pub(crate) max_modules: Option<NonZeroU64>,
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

            type_names: Default::default(),

            // default print/debug implementations
            print: Box::new(default_print),
            debug: Box::new(default_print),

            // progress callback
            progress: None,

            // optimization level
            #[cfg(feature = "no_optimize")]
            optimization_level: OptimizationLevel::None,

            #[cfg(not(feature = "no_optimize"))]
            #[cfg(not(feature = "optimize_full"))]
            optimization_level: OptimizationLevel::Simple,

            #[cfg(not(feature = "no_optimize"))]
            #[cfg(feature = "optimize_full")]
            optimization_level: OptimizationLevel::Full,

            max_call_stack_depth: MAX_CALL_STACK_DEPTH,
            max_operations: None,
            max_modules: None,
        };

        #[cfg(feature = "no_stdlib")]
        engine.load_package(CorePackage::new().get());

        #[cfg(not(feature = "no_stdlib"))]
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
fn search_scope<'a>(
    scope: &'a mut Scope,
    name: &str,
    #[cfg(not(feature = "no_module"))] modules: Option<(&Box<ModuleRef>, u64)>,
    #[cfg(feature = "no_module")] _: Option<(&ModuleRef, u64)>,
    index: Option<NonZeroUsize>,
    pos: Position,
) -> Result<(&'a mut Dynamic, ScopeEntryType), Box<EvalAltResult>> {
    #[cfg(not(feature = "no_module"))]
    {
        if let Some((modules, hash_var)) = modules {
            let module = if let Some(index) = modules.index() {
                scope
                    .get_mut(scope.len() - index.get())
                    .0
                    .downcast_mut::<Module>()
                    .unwrap()
            } else {
                let (id, root_pos) = modules.get_ref(0);

                scope.find_module(id).ok_or_else(|| {
                    Box::new(EvalAltResult::ErrorModuleNotFound(id.into(), *root_pos))
                })?
            };

            return Ok((
                module.get_qualified_var_mut(name, hash_var, pos)?,
                // Module variables are constant
                ScopeEntryType::Constant,
            ));
        }
    }

    let index = if let Some(index) = index {
        scope.len() - index.get()
    } else {
        scope
            .get_index(name)
            .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(name.into(), pos)))?
            .0
    };

    Ok(scope.get_mut(index))
}

impl Engine {
    /// Create a new `Engine`
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new `Engine` with _no_ built-in functions.
    /// Use the `load_package` method to load packages of functions.
    pub fn new_raw() -> Self {
        Self {
            packages: Default::default(),
            global_module: Default::default(),

            #[cfg(not(feature = "no_module"))]
            module_resolver: None,

            type_names: Default::default(),
            print: Box::new(|_| {}),
            debug: Box::new(|_| {}),
            progress: None,

            #[cfg(feature = "no_optimize")]
            optimization_level: OptimizationLevel::None,

            #[cfg(not(feature = "no_optimize"))]
            #[cfg(not(feature = "optimize_full"))]
            optimization_level: OptimizationLevel::Simple,

            #[cfg(not(feature = "no_optimize"))]
            #[cfg(feature = "optimize_full")]
            optimization_level: OptimizationLevel::Full,

            max_call_stack_depth: MAX_CALL_STACK_DEPTH,
            max_operations: None,
            max_modules: None,
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
        self.max_operations = NonZeroU64::new(operations);
    }

    /// Set the maximum number of imported modules allowed for a script (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_modules(&mut self, modules: u64) {
        self.max_modules = NonZeroU64::new(modules);
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
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_fn_raw(
        &self,
        scope: Option<&mut Scope>,
        state: &mut State,
        fn_name: &str,
        hashes: (u64, u64),
        args: &mut FnCallArgs,
        is_ref: bool,
        def_val: Option<&Dynamic>,
        pos: Position,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        self.inc_operations(state, pos)?;

        // Check for stack overflow
        if level > self.max_call_stack_depth {
            return Err(Box::new(EvalAltResult::ErrorStackOverflow(pos)));
        }

        // First search in script-defined functions (can override built-in)
        if hashes.1 > 0 {
            if let Some(fn_def) = state.get_function(hashes.1) {
                let (result, state2) =
                    self.call_script_fn(scope, *state, fn_name, fn_def, args, pos, level)?;
                *state = state2;
                return Ok((result, false));
            }
        }

        // Search built-in's and external functions
        if let Some(func) = self
            .global_module
            .get_fn(hashes.0)
            .or_else(|| self.packages.get_fn(hashes.0))
        {
            let mut backup: Dynamic = Default::default();

            let (updated, restore) = match func.abi() {
                // Calling pure function in method-call
                NativeFunctionABI::Pure if is_ref && args.len() > 0 => {
                    // Backup the original value.  It'll be consumed because the function
                    // is pure and doesn't know that the first value is a reference (i.e. `is_ref`)
                    backup = args[0].clone();
                    (false, true)
                }
                NativeFunctionABI::Pure => (false, false),
                NativeFunctionABI::Method => (true, false),
            };

            // Run external function
            let result = match func.call(args) {
                Ok(r) => {
                    // Restore the backup value for the first argument since it has been consumed!
                    if restore {
                        *args[0] = backup;
                    }
                    r
                }
                Err(err) => {
                    return Err(err.new_position(pos));
                }
            };

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
                _ => (result, updated),
            });
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
    pub(crate) fn call_script_fn<'s>(
        &self,
        scope: Option<&mut Scope>,
        mut state: State<'s>,
        fn_name: &str,
        fn_def: &FnDef,
        args: &mut FnCallArgs,
        pos: Position,
        level: usize,
    ) -> Result<(Dynamic, State<'s>), Box<EvalAltResult>> {
        self.inc_operations(&mut state, pos)?;

        let orig_scope_level = state.scope_level;
        state.scope_level += 1;

        match scope {
            // Extern scope passed in which is not empty
            Some(scope) if scope.len() > 0 => {
                let scope_len = scope.len();

                // Put arguments into scope as variables
                scope.extend(
                    fn_def
                        .params
                        .iter()
                        .zip(
                            // Actually consume the arguments instead of cloning them
                            args.into_iter().map(|v| mem::take(*v)),
                        )
                        .map(|(name, value)| {
                            let var_name = unsafe_cast_var_name(name.as_str(), &mut state);
                            (var_name, ScopeEntryType::Normal, value)
                        }),
                );

                // Evaluate the function at one higher level of call depth
                let result = self
                    .eval_stmt(scope, &mut state, &fn_def.body, level + 1)
                    .or_else(|err| match *err {
                        // Convert return statement to return value
                        EvalAltResult::Return(x, _) => Ok(x),
                        EvalAltResult::ErrorInFunctionCall(name, err, _) => {
                            Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                                format!("{} > {}", fn_name, name),
                                err,
                                pos,
                            )))
                        }
                        _ => Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                            fn_name.to_string(),
                            err,
                            pos,
                        ))),
                    });

                // Remove all local variables
                scope.rewind(scope_len);
                state.scope_level = orig_scope_level;

                return result.map(|v| (v, state));
            }
            // No new scope - create internal scope
            _ => {
                let mut scope = Scope::new();

                // Put arguments into scope as variables
                scope.extend(
                    fn_def
                        .params
                        .iter()
                        .zip(
                            // Actually consume the arguments instead of cloning them
                            args.into_iter().map(|v| mem::take(*v)),
                        )
                        .map(|(name, value)| (name, ScopeEntryType::Normal, value)),
                );

                // Evaluate the function at one higher level of call depth
                let result = self
                    .eval_stmt(&mut scope, &mut state, &fn_def.body, level + 1)
                    .or_else(|err| match *err {
                        // Convert return statement to return value
                        EvalAltResult::Return(x, _) => Ok(x),
                        EvalAltResult::ErrorInFunctionCall(name, err, _) => {
                            Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                                format!("{} > {}", fn_name, name),
                                err,
                                pos,
                            )))
                        }
                        _ => Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                            fn_name.to_string(),
                            err,
                            pos,
                        ))),
                    });

                state.scope_level = orig_scope_level;
                return result.map(|v| (v, state));
            }
        }
    }

    // Has a system function an override?
    fn has_override(&self, state: &State, hashes: (u64, u64)) -> bool {
        // First check registered functions
        self.global_module.contains_fn(hashes.0)
            // Then check packages
            || self.packages.contains_fn(hashes.0)
            // Then check script-defined functions
            || state.has_function(hashes.1)
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
        fn_name: &str,
        hash_fn_def: u64,
        args: &mut FnCallArgs,
        is_ref: bool,
        def_val: Option<&Dynamic>,
        pos: Position,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        // Qualifiers (none) + function name + argument `TypeId`'s.
        let hash_fn = calc_fn_hash(empty(), fn_name, args.iter().map(|a| a.type_id()));
        let hashes = (hash_fn, hash_fn_def);

        match fn_name {
            // type_of
            KEYWORD_TYPE_OF if args.len() == 1 && !self.has_override(state, hashes) => Ok((
                self.map_type_name(args[0].type_name()).to_string().into(),
                false,
            )),

            // eval - reaching this point it must be a method-style call
            KEYWORD_EVAL if args.len() == 1 && !self.has_override(state, hashes) => {
                Err(Box::new(EvalAltResult::ErrorRuntime(
                    "'eval' should not be called in method style. Try eval(...);".into(),
                    pos,
                )))
            }

            // Normal function call
            _ => self.call_fn_raw(
                None, state, fn_name, hashes, args, is_ref, def_val, pos, level,
            ),
        }
    }

    /// Evaluate a text string as a script - used primarily for 'eval'.
    fn eval_script_expr(
        &self,
        scope: &mut Scope,
        state: &mut State,
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
        if ast.fn_lib().len() > 0 {
            return Err(Box::new(EvalAltResult::ErrorParsing(
                ParseErrorType::WrongFnDefinition.into_err(pos),
            )));
        }

        let statements = mem::take(ast.statements_mut());
        let ast = AST::new(statements, state.fn_lib.clone());

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
            match rhs {
                // xxx[idx].dot_rhs... | xxx[idx][dot_rhs]...
                Expr::Dot(x) | Expr::Index(x) => {
                    let is_idx = matches!(rhs, Expr::Index(_));
                    let pos = x.0.position();
                    let this_ptr = &mut self
                        .get_indexed_mut(state, obj, is_ref, idx_val, pos, op_pos, false)?;

                    self.eval_dot_index_chain_helper(
                        state, this_ptr, &x.1, idx_values, is_idx, x.2, level, new_val,
                    )
                }
                // xxx[rhs] = new_val
                _ if new_val.is_some() => {
                    let pos = rhs.position();
                    let this_ptr = &mut self
                        .get_indexed_mut(state, obj, is_ref, idx_val, pos, op_pos, true)?;

                    this_ptr.set_value(new_val.unwrap(), rhs.position())?;
                    Ok((Default::default(), true))
                }
                // xxx[rhs]
                _ => self
                    .get_indexed_mut(state, obj, is_ref, idx_val, rhs.position(), op_pos, false)
                    .map(|v| (v.clone_into_dynamic(), false)),
            }
        } else {
            match rhs {
                // xxx.fn_name(arg_expr_list)
                Expr::FnCall(x) if x.1.is_none() => {
                    let ((name, pos), _, hash_fn_def, _, def_val) = x.as_ref();
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

                    self.exec_fn_call(state, name, *hash_fn_def, args, is_ref, def_val, *pos, 0)
                }
                // xxx.module::fn_name(...) - syntax error
                Expr::FnCall(_) => unreachable!(),
                // {xxx:map}.id = ???
                #[cfg(not(feature = "no_object"))]
                Expr::Property(x) if obj.is::<Map>() && new_val.is_some() => {
                    let ((prop, _, _), pos) = x.as_ref();
                    let index = prop.clone().into();
                    let mut val =
                        self.get_indexed_mut(state, obj, is_ref, index, *pos, op_pos, true)?;

                    val.set_value(new_val.unwrap(), rhs.position())?;
                    Ok((Default::default(), true))
                }
                // {xxx:map}.id
                #[cfg(not(feature = "no_object"))]
                Expr::Property(x) if obj.is::<Map>() => {
                    let ((prop, _, _), pos) = x.as_ref();
                    let index = prop.clone().into();
                    let val =
                        self.get_indexed_mut(state, obj, is_ref, index, *pos, op_pos, false)?;

                    Ok((val.clone_into_dynamic(), false))
                }
                // xxx.id = ???
                Expr::Property(x) if new_val.is_some() => {
                    let ((_, _, setter), pos) = x.as_ref();
                    let mut args = [obj, new_val.as_mut().unwrap()];
                    self.exec_fn_call(state, setter, 0, &mut args, is_ref, None, *pos, 0)
                        .map(|(v, _)| (v, true))
                }
                // xxx.id
                Expr::Property(x) => {
                    let ((_, getter, _), pos) = x.as_ref();
                    let mut args = [obj];
                    self.exec_fn_call(state, getter, 0, &mut args, is_ref, None, *pos, 0)
                        .map(|(v, _)| (v, false))
                }
                #[cfg(not(feature = "no_object"))]
                // {xxx:map}.idx_lhs[idx_expr] | {xxx:map}.dot_lhs.rhs
                Expr::Index(x) | Expr::Dot(x) if obj.is::<Map>() => {
                    let is_idx = matches!(rhs, Expr::Index(_));

                    let mut val = if let Expr::Property(p) = &x.0 {
                        let ((prop, _, _), _) = p.as_ref();
                        let index = prop.clone().into();
                        self.get_indexed_mut(state, obj, is_ref, index, x.2, op_pos, false)?
                    } else {
                        // Syntax error
                        return Err(Box::new(EvalAltResult::ErrorDotExpr(
                            "".into(),
                            rhs.position(),
                        )));
                    };

                    self.eval_dot_index_chain_helper(
                        state, &mut val, &x.1, idx_values, is_idx, x.2, level, new_val,
                    )
                }
                // xxx.idx_lhs[idx_expr] | xxx.dot_lhs.rhs
                Expr::Index(x) | Expr::Dot(x) => {
                    let is_idx = matches!(rhs, Expr::Index(_));
                    let args = &mut [obj, &mut Default::default()];

                    let (mut val, updated) = if let Expr::Property(p) = &x.0 {
                        let ((_, getter, _), _) = p.as_ref();
                        self.exec_fn_call(state, getter, 0, &mut args[..1], is_ref, None, x.2, 0)?
                    } else {
                        // Syntax error
                        return Err(Box::new(EvalAltResult::ErrorDotExpr(
                            "".into(),
                            rhs.position(),
                        )));
                    };
                    let val = &mut val;
                    let target = &mut val.into();

                    let (result, may_be_changed) = self.eval_dot_index_chain_helper(
                        state, target, &x.1, idx_values, is_idx, x.2, level, new_val,
                    )?;

                    // Feed the value back via a setter just in case it has been updated
                    if updated || may_be_changed {
                        if let Expr::Property(p) = &x.0 {
                            let ((_, _, setter), _) = p.as_ref();
                            // Re-use args because the first &mut parameter will not be consumed
                            args[1] = val;
                            self.exec_fn_call(state, setter, 0, args, is_ref, None, x.2, 0)
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
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        is_index: bool,
        op_pos: Position,
        level: usize,
        new_val: Option<Dynamic>,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let idx_values = &mut StaticVec::new();

        self.eval_indexed_chain(scope, state, dot_rhs, idx_values, 0, level)?;

        match dot_lhs {
            // id.??? or id[???]
            Expr::Variable(x) => {
                let ((name, pos), modules, hash_var, index) = x.as_ref();
                let index = if state.always_search { None } else { *index };
                let mod_and_hash = modules.as_ref().map(|m| (m, *hash_var));
                let (target, typ) = search_scope(scope, &name, mod_and_hash, index, *pos)?;

                // Constants cannot be modified
                match typ {
                    ScopeEntryType::Module => unreachable!(),
                    ScopeEntryType::Constant if new_val.is_some() => {
                        return Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                            name.clone(),
                            *pos,
                        )));
                    }
                    ScopeEntryType::Constant | ScopeEntryType::Normal => (),
                }

                let this_ptr = &mut target.into();
                self.eval_dot_index_chain_helper(
                    state, this_ptr, dot_rhs, idx_values, is_index, op_pos, level, new_val,
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
                let val = self.eval_expr(scope, state, expr, level)?;
                let this_ptr = &mut val.into();
                self.eval_dot_index_chain_helper(
                    state, this_ptr, dot_rhs, idx_values, is_index, op_pos, level, new_val,
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
        expr: &Expr,
        idx_values: &mut StaticVec<Dynamic>,
        size: usize,
        level: usize,
    ) -> Result<(), Box<EvalAltResult>> {
        match expr {
            Expr::FnCall(x) if x.1.is_none() => {
                let mut arg_values = StaticVec::<Dynamic>::new();

                for arg_expr in x.3.iter() {
                    arg_values.push(self.eval_expr(scope, state, arg_expr, level)?);
                }

                idx_values.push(Dynamic::from(arg_values));
            }
            Expr::FnCall(_) => unreachable!(),
            Expr::Property(_) => idx_values.push(()), // Store a placeholder - no need to copy the property name
            Expr::Index(x) | Expr::Dot(x) => {
                // Evaluate in left-to-right order
                let lhs_val = match x.0 {
                    Expr::Property(_) => Default::default(), // Store a placeholder in case of a property
                    _ => self.eval_expr(scope, state, &x.0, level)?,
                };

                // Push in reverse order
                self.eval_indexed_chain(scope, state, &x.1, idx_values, size, level)?;

                idx_values.push(lhs_val);
            }
            _ => idx_values.push(self.eval_expr(scope, state, expr, level)?),
        }

        Ok(())
    }

    /// Get the value at the indexed position of a base type
    fn get_indexed_mut<'a>(
        &self,
        state: &mut State,
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
                let index = idx
                    .take_string()
                    .map_err(|_| EvalAltResult::ErrorStringIndexExpr(idx_pos))?;

                Ok(if create {
                    map.entry(index).or_insert(Default::default()).into()
                } else {
                    map.get_mut(&index)
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
                let type_name = self.map_type_name(val.type_name());
                let args = &mut [val, &mut idx];
                self.exec_fn_call(state, FUNC_INDEXER, 0, args, is_ref, None, op_pos, 0)
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
        lhs: &Expr,
        rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, rhs.position())?;

        let mut lhs_value = self.eval_expr(scope, state, lhs, level)?;
        let rhs_value = self.eval_expr(scope, state, rhs, level)?;

        match rhs_value {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(mut rhs_value)) => {
                let op = "==";
                let def_value = false.into();
                let hash_fn_def = calc_fn_hash(empty(), op, repeat(EMPTY_TYPE_ID()).take(2));

                // Call the `==` operator to compare each value
                for value in rhs_value.iter_mut() {
                    let args = &mut [&mut lhs_value, value];
                    let def_value = Some(&def_value);
                    let pos = rhs.position();

                    // Qualifiers (none) + function name + argument `TypeId`'s.
                    let hash_fn = calc_fn_hash(empty(), op, args.iter().map(|a| a.type_id()));
                    let hashes = (hash_fn, hash_fn_def);

                    let (r, _) = self
                        .call_fn_raw(None, state, op, hashes, args, true, def_value, pos, level)?;
                    if r.as_bool().unwrap_or(false) {
                        return Ok(true.into());
                    }
                }

                Ok(false.into())
            }
            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(rhs_value)) => match lhs_value {
                // Only allows String or char
                Dynamic(Union::Str(s)) => Ok(rhs_value.contains_key(s.as_ref()).into()),
                Dynamic(Union::Char(c)) => Ok(rhs_value.contains_key(&c.to_string()).into()),
                _ => Err(Box::new(EvalAltResult::ErrorInExpr(lhs.position()))),
            },
            Dynamic(Union::Str(rhs_value)) => match lhs_value {
                // Only allows String or char
                Dynamic(Union::Str(s)) => Ok(rhs_value.contains(s.as_ref()).into()),
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
        expr: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, expr.position())?;

        match expr {
            Expr::IntegerConstant(x) => Ok(x.0.into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(x) => Ok(x.0.into()),
            Expr::StringConstant(x) => Ok(x.0.to_string().into()),
            Expr::CharConstant(x) => Ok(x.0.into()),
            Expr::Variable(x) => {
                let ((name, pos), modules, hash_var, index) = x.as_ref();
                let index = if state.always_search { None } else { *index };
                let mod_and_hash = modules.as_ref().map(|m| (m, *hash_var));
                let (val, _) = search_scope(scope, name, mod_and_hash, index, *pos)?;
                Ok(val.clone())
            }
            Expr::Property(_) => unreachable!(),

            // Statement block
            Expr::Stmt(stmt) => self.eval_stmt(scope, state, &stmt.0, level),

            // lhs = rhs
            Expr::Assignment(x) => {
                let op_pos = x.2;
                let rhs_val = self.eval_expr(scope, state, &x.1, level)?;

                match &x.0 {
                    // name = rhs
                    Expr::Variable(x) => {
                        let ((name, pos), modules, hash_var, index) = x.as_ref();
                        let index = if state.always_search { None } else { *index };
                        let mod_and_hash = modules.as_ref().map(|m| (m, *hash_var));
                        let (lhs_ptr, typ) = search_scope(scope, name, mod_and_hash, index, *pos)?;
                        match typ {
                            ScopeEntryType::Constant => Err(Box::new(
                                EvalAltResult::ErrorAssignmentToConstant(name.clone(), *pos),
                            )),
                            ScopeEntryType::Normal => {
                                *lhs_ptr = rhs_val;
                                Ok(Default::default())
                            }
                            // End variable cannot be a module
                            ScopeEntryType::Module => unreachable!(),
                        }
                    }
                    // idx_lhs[idx_expr] = rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(x) => {
                        let new_val = Some(rhs_val);
                        self.eval_dot_index_chain(
                            scope, state, &x.0, &x.1, true, x.2, level, new_val,
                        )
                    }
                    // dot_lhs.dot_rhs = rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(x) => {
                        let new_val = Some(rhs_val);
                        self.eval_dot_index_chain(
                            scope, state, &x.0, &x.1, false, op_pos, level, new_val,
                        )
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
            Expr::Index(x) => {
                self.eval_dot_index_chain(scope, state, &x.0, &x.1, true, x.2, level, None)
            }

            // lhs.dot_rhs
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(x) => {
                self.eval_dot_index_chain(scope, state, &x.0, &x.1, false, x.2, level, None)
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Array(x) => Ok(Dynamic(Union::Array(Box::new(
                x.0.iter()
                    .map(|item| self.eval_expr(scope, state, item, level))
                    .collect::<Result<Vec<_>, _>>()?,
            )))),

            #[cfg(not(feature = "no_object"))]
            Expr::Map(x) => Ok(Dynamic(Union::Map(Box::new(
                x.0.iter()
                    .map(|((key, _), expr)| {
                        self.eval_expr(scope, state, expr, level)
                            .map(|val| (key.clone(), val))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?,
            )))),

            // Normal function call
            Expr::FnCall(x) if x.1.is_none() => {
                let ((name, pos), _, hash_fn_def, args_expr, def_val) = x.as_ref();
                let def_val = def_val.as_ref();

                let mut arg_values = args_expr
                    .iter()
                    .map(|expr| self.eval_expr(scope, state, expr, level))
                    .collect::<Result<StaticVec<_>, _>>()?;

                let mut args: StaticVec<_> = arg_values.iter_mut().collect();

                if name == KEYWORD_EVAL && args.len() == 1 && args.get_ref(0).is::<String>() {
                    let hash_fn = calc_fn_hash(empty(), name, once(TypeId::of::<String>()));

                    if !self.has_override(state, (hash_fn, *hash_fn_def)) {
                        // eval - only in function call style
                        let prev_len = scope.len();

                        // Evaluate the text string as a script
                        let result = self.eval_script_expr(
                            scope,
                            state,
                            args.pop(),
                            args_expr[0].position(),
                        );

                        if scope.len() != prev_len {
                            // IMPORTANT! If the eval defines new variables in the current scope,
                            //            all variable offsets from this point on will be mis-aligned.
                            state.always_search = true;
                        }

                        return result;
                    }
                }

                // Normal function call - except for eval (handled above)
                let args = args.as_mut();
                self.exec_fn_call(state, name, *hash_fn_def, args, false, def_val, *pos, level)
                    .map(|(v, _)| v)
            }

            // Module-qualified function call
            #[cfg(not(feature = "no_module"))]
            Expr::FnCall(x) if x.1.is_some() => {
                let ((name, pos), modules, hash_fn_def, args_expr, def_val) = x.as_ref();
                let modules = modules.as_ref().unwrap();

                let mut arg_values = args_expr
                    .iter()
                    .map(|expr| self.eval_expr(scope, state, expr, level))
                    .collect::<Result<StaticVec<_>, _>>()?;

                let mut args: StaticVec<_> = arg_values.iter_mut().collect();

                let (id, root_pos) = modules.get_ref(0); // First module

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
                if let Some(fn_def) = module.get_qualified_scripted_fn(*hash_fn_def) {
                    let args = args.as_mut();
                    let (result, state2) =
                        self.call_script_fn(None, *state, name, fn_def, args, *pos, level)?;
                    *state = state2;
                    Ok(result)
                } else {
                    // Then search in Rust functions
                    self.inc_operations(state, *pos)?;

                    // Rust functions are indexed in two steps:
                    // 1) Calculate a hash in a similar manner to script-defined functions,
                    //    i.e. qualifiers + function name + dummy parameter types (one for each parameter).
                    // 2) Calculate a second hash with no qualifiers, empty function name, and
                    //    the actual list of parameter `TypeId`'.s
                    let hash_fn_args = calc_fn_hash(empty(), "", args.iter().map(|a| a.type_id()));
                    // 3) The final hash is the XOR of the two hashes.
                    let hash_fn_native = *hash_fn_def ^ hash_fn_args;

                    match module.get_qualified_fn(name, hash_fn_native) {
                        Ok(func) => func
                            .call(args.as_mut())
                            .map_err(|err| err.new_position(*pos)),
                        Err(_) if def_val.is_some() => Ok(def_val.clone().unwrap()),
                        Err(err) => Err(err),
                    }
                }
            }

            Expr::In(x) => self.eval_in_expr(scope, state, &x.0, &x.1, level),

            Expr::And(x) => {
                let (lhs, rhs, _) = x.as_ref();
                Ok((self
                    .eval_expr(scope, state, lhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), lhs.position())
                    })?
                    && // Short-circuit using &&
                self
                    .eval_expr(scope, state, rhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), rhs.position())
                    })?)
                .into())
            }

            Expr::Or(x) => {
                let (lhs, rhs, _) = x.as_ref();
                Ok((self
                    .eval_expr(scope, state, lhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), lhs.position())
                    })?
                    || // Short-circuit using ||
                self
                    .eval_expr(scope, state, rhs, level)?
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
    pub(crate) fn eval_stmt<'s>(
        &self,
        scope: &mut Scope<'s>,
        state: &mut State,
        stmt: &Stmt,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, stmt.position())?;

        match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Default::default()),

            // Expression as statement
            Stmt::Expr(expr) => {
                let result = self.eval_expr(scope, state, expr, level)?;

                Ok(match expr.as_ref() {
                    // If it is an assignment, erase the result at the root
                    Expr::Assignment(_) => Default::default(),
                    _ => result,
                })
            }

            // Block scope
            Stmt::Block(x) => {
                let prev_len = scope.len();
                state.scope_level += 1;

                let result = x.0.iter().try_fold(Default::default(), |_, stmt| {
                    self.eval_stmt(scope, state, stmt, level)
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

                self.eval_expr(scope, state, expr, level)?
                    .as_bool()
                    .map_err(|_| Box::new(EvalAltResult::ErrorLogicGuard(expr.position())))
                    .and_then(|guard_val| {
                        if guard_val {
                            self.eval_stmt(scope, state, if_block, level)
                        } else if let Some(stmt) = else_block {
                            self.eval_stmt(scope, state, stmt, level)
                        } else {
                            Ok(Default::default())
                        }
                    })
            }

            // While loop
            Stmt::While(x) => loop {
                let (expr, body) = x.as_ref();

                match self.eval_expr(scope, state, expr, level)?.as_bool() {
                    Ok(true) => match self.eval_stmt(scope, state, body, level) {
                        Ok(_) => {
                            self.inc_operations(state, body.position())?;
                        }
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
                match self.eval_stmt(scope, state, body, level) {
                    Ok(_) => {
                        self.inc_operations(state, body.position())?;
                    }
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
                let iter_type = self.eval_expr(scope, state, expr, level)?;
                let tid = iter_type.type_id();

                if let Some(iter_fn) = self
                    .global_module
                    .get_iter(tid)
                    .or_else(|| self.packages.get_iter(tid))
                {
                    // Add the loop variable
                    let var_name = unsafe_cast_var_name(name, &state);
                    scope.push(var_name, ());
                    let index = scope.len() - 1;
                    state.scope_level += 1;

                    for loop_var in iter_fn(iter_type) {
                        *scope.get_mut(index).0 = loop_var;
                        self.inc_operations(state, stmt.position())?;

                        match self.eval_stmt(scope, state, stmt, level) {
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
                    self.eval_expr(scope, state, x.1.as_ref().unwrap(), level)?,
                    (x.0).1,
                )))
            }

            // Empty return
            Stmt::ReturnWithVal(x) if (x.0).0 == ReturnType::Return => {
                Err(Box::new(EvalAltResult::Return(Default::default(), (x.0).1)))
            }

            // Throw value
            Stmt::ReturnWithVal(x) if x.1.is_some() && (x.0).0 == ReturnType::Exception => {
                let val = self.eval_expr(scope, state, x.1.as_ref().unwrap(), level)?;
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
                let ((var_name, pos), expr) = x.as_ref();
                let val = self.eval_expr(scope, state, expr.as_ref().unwrap(), level)?;
                let var_name = unsafe_cast_var_name(var_name, &state);
                scope.push_dynamic_value(var_name, ScopeEntryType::Normal, val, false);
                self.inc_operations(state, *pos)?;
                Ok(Default::default())
            }

            Stmt::Let(x) => {
                let ((var_name, pos), _) = x.as_ref();
                let var_name = unsafe_cast_var_name(var_name, &state);
                scope.push(var_name, ());
                self.inc_operations(state, *pos)?;
                Ok(Default::default())
            }

            // Const statement
            Stmt::Const(x) if x.1.is_constant() => {
                let ((var_name, pos), expr) = x.as_ref();
                let val = self.eval_expr(scope, state, &expr, level)?;
                let var_name = unsafe_cast_var_name(var_name, &state);
                scope.push_dynamic_value(var_name, ScopeEntryType::Constant, val, true);
                self.inc_operations(state, *pos)?;
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
                    if let Some(max) = self.max_modules {
                        if state.modules >= max.get() {
                            return Err(Box::new(EvalAltResult::ErrorTooManyModules(*pos)));
                        }
                    }

                    if let Some(path) = self
                        .eval_expr(scope, state, &expr, level)?
                        .try_cast::<String>()
                    {
                        if let Some(resolver) = self.module_resolver.as_ref() {
                            // Use an empty scope to create a module
                            let module =
                                resolver.resolve(self, Scope::new(), &path, expr.position())?;

                            let mod_name = unsafe_cast_var_name(name, &state);
                            scope.push_module(mod_name, module);

                            state.modules += 1;
                            self.inc_operations(state, *pos)?;

                            Ok(Default::default())
                        } else {
                            Err(Box::new(EvalAltResult::ErrorModuleNotFound(
                                path,
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
                for ((id, id_pos), rename) in list.as_ref() {
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
            if let Some(max) = self.max_operations {
                if state.operations > max.get() {
                    return Err(Box::new(EvalAltResult::ErrorTooManyOperations(pos)));
                }
            }
        }

        // Report progress - only in steps
        if let Some(progress) = self.progress.as_ref() {
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
