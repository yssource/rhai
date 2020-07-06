//! Main module defining the script evaluation `Engine`.

use crate::any::{map_std_type_name, Dynamic, Union, Variant};
use crate::calc_fn_hash;
use crate::error::ParseErrorType;
use crate::fn_native::{CallableFunction, Callback, FnCallArgs, FnPtr};
use crate::module::{resolvers, Module, ModuleRef, ModuleResolver};
use crate::optimize::OptimizationLevel;
use crate::packages::{Package, PackagesCollection, StandardPackage};
use crate::parser::{Expr, FnAccess, ImmutableString, ReturnType, ScriptFnDef, Stmt, AST, INT};
use crate::r#unsafe::unsafe_cast_var_name_to_lifetime;
use crate::result::EvalAltResult;
use crate::scope::{EntryType as ScopeEntryType, Scope};
use crate::token::Position;
use crate::utils::StaticVec;

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

use crate::stdlib::{
    any::{type_name, TypeId},
    borrow::Cow,
    boxed::Box,
    collections::{HashMap, HashSet},
    convert::TryFrom,
    format,
    iter::{empty, once},
    mem,
    string::{String, ToString},
    vec::Vec,
};

/// Variable-sized array of `Dynamic` values.
///
/// Not available under the `no_index` feature.
#[cfg(not(feature = "no_index"))]
pub type Array = Vec<Dynamic>;

/// Hash map of `Dynamic` values with `ImmutableString` keys.
///
/// Not available under the `no_object` feature.
#[cfg(not(feature = "no_object"))]
pub type Map = HashMap<ImmutableString, Dynamic>;

/// A stack of imported modules.
pub type Imports<'a> = Vec<(Cow<'a, str>, Module)>;

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
pub const MAX_EXPR_DEPTH: usize = 0;
#[cfg(feature = "unchecked")]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = 0;

pub const KEYWORD_PRINT: &str = "print";
pub const KEYWORD_DEBUG: &str = "debug";
pub const KEYWORD_TYPE_OF: &str = "type_of";
pub const KEYWORD_EVAL: &str = "eval";
pub const KEYWORD_FN_PTR: &str = "Fn";
pub const KEYWORD_FN_PTR_CALL: &str = "call";
pub const KEYWORD_THIS: &str = "this";
pub const FN_TO_STRING: &str = "to_string";
pub const FN_GET: &str = "get$";
pub const FN_SET: &str = "set$";
pub const FN_IDX_GET: &str = "$index$get$";
pub const FN_IDX_SET: &str = "$index$set$";

/// A type specifying the method of chaining.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum ChainType {
    None,
    Index,
    Dot,
}

/// A type that encapsulates a mutation target for an expression with side effects.
#[derive(Debug)]
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
            Self::Ref(_) => true,
            Self::Value(_) | Self::StringChar(_, _, _) => false,
        }
    }
    /// Is the `Target` an owned value?
    pub fn is_value(&self) -> bool {
        match self {
            Self::Ref(_) => false,
            Self::Value(_) => true,
            Self::StringChar(_, _, _) => false,
        }
    }
    /// Is the `Target` a specific type?
    pub fn is<T: Variant + Clone>(&self) -> bool {
        match self {
            Target::Ref(r) => r.is::<T>(),
            Target::Value(r) => r.is::<T>(),
            Target::StringChar(_, _, _) => TypeId::of::<T>() == TypeId::of::<char>(),
        }
    }
    /// Get the value of the `Target` as a `Dynamic`, cloning a referenced value if necessary.
    pub fn clone_into_dynamic(self) -> Dynamic {
        match self {
            Self::Ref(r) => r.clone(),        // Referenced value is cloned
            Self::Value(v) => v,              // Owned value is simply taken
            Self::StringChar(_, _, ch) => ch, // Character is taken
        }
    }
    /// Get a mutable reference from the `Target`.
    pub fn as_mut(&mut self) -> &mut Dynamic {
        match self {
            Self::Ref(r) => *r,
            Self::Value(ref mut r) => r,
            Self::StringChar(_, _, ref mut r) => r,
        }
    }
    /// Update the value of the `Target`.
    /// Position in `EvalAltResult` is None and must be set afterwards.
    pub fn set_value(&mut self, new_val: Dynamic) -> Result<(), Box<EvalAltResult>> {
        match self {
            Self::Ref(r) => **r = new_val,
            Self::Value(_) => {
                return Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(
                    Position::none(),
                )))
            }
            Self::StringChar(Dynamic(Union::Str(ref mut s)), index, _) => {
                // Replace the character at the specified index position
                let new_ch = new_val
                    .as_char()
                    .map_err(|_| EvalAltResult::ErrorCharMismatch(Position::none()))?;

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
#[derive(Debug, Clone, Eq, PartialEq, Hash, Default)]
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
    pub modules: usize,
}

impl State {
    /// Create a new `State`.
    pub fn new() -> Self {
        Default::default()
    }
}

/// Get a script-defined function definition from a module.
#[cfg(not(feature = "no_function"))]
pub fn get_script_function_by_signature<'a>(
    module: &'a Module,
    name: &str,
    params: usize,
    public_only: bool,
) -> Option<&'a ScriptFnDef> {
    // Qualifiers (none) + function name + number of arguments.
    let hash_script = calc_fn_hash(empty(), name, params, empty());
    let func = module.get_fn(hash_script)?;
    if !func.is_script() {
        return None;
    }
    let fn_def = func.get_fn_def();

    match fn_def.access {
        FnAccess::Private if public_only => None,
        FnAccess::Private | FnAccess::Public => Some(&fn_def),
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
/// Currently, `Engine` is neither `Send` nor `Sync`. Use the `sync` feature to make it `Send + Sync`.
pub struct Engine {
    /// A unique ID identifying this scripting `Engine`.
    pub id: Option<String>,

    /// A module containing all functions directly loaded into the Engine.
    pub(crate) global_module: Module,
    /// A collection of all library packages loaded into the Engine.
    pub(crate) packages: PackagesCollection,

    /// A module resolution service.
    pub(crate) module_resolver: Option<Box<dyn ModuleResolver>>,

    /// A hashmap mapping type names to pretty-print names.
    pub(crate) type_names: Option<HashMap<String, String>>,

    /// A hashset containing symbols to disable.
    pub(crate) disabled_symbols: Option<HashSet<String>>,
    /// A hashset containing custom keywords and precedence to recognize.
    pub(crate) custom_keywords: Option<HashMap<String, u8>>,

    /// Callback closure for implementing the `print` command.
    pub(crate) print: Callback<str, ()>,
    /// Callback closure for implementing the `debug` command.
    pub(crate) debug: Callback<str, ()>,
    /// Callback closure for progress reporting.
    pub(crate) progress: Option<Callback<u64, bool>>,

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
    pub(crate) max_modules: usize,
    /// Maximum length of a string.
    pub(crate) max_string_size: usize,
    /// Maximum length of an array.
    pub(crate) max_array_size: usize,
    /// Maximum number of properties in a map.
    pub(crate) max_map_size: usize,
}

impl Default for Engine {
    fn default() -> Self {
        // Create the new scripting Engine
        let mut engine = Self {
            id: None,

            packages: Default::default(),
            global_module: Default::default(),

            #[cfg(not(feature = "no_module"))]
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(target_arch = "wasm32"))]
            module_resolver: Some(Box::new(resolvers::FileModuleResolver::new())),
            #[cfg(any(feature = "no_module", feature = "no_std", target_arch = "wasm32",))]
            module_resolver: None,

            type_names: None,
            disabled_symbols: None,
            custom_keywords: None,

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
            max_operations: 0,
            max_modules: usize::MAX,
            max_string_size: 0,
            max_array_size: 0,
            max_map_size: 0,
        };

        engine.load_package(StandardPackage::new().get());

        engine
    }
}

/// Make getter function
pub fn make_getter(id: &str) -> String {
    format!("{}{}", FN_GET, id)
}

/// Extract the property name from a getter function name.
fn extract_prop_from_getter(fn_name: &str) -> Option<&str> {
    #[cfg(not(feature = "no_object"))]
    if fn_name.starts_with(FN_GET) {
        Some(&fn_name[FN_GET.len()..])
    } else {
        None
    }

    #[cfg(feature = "no_object")]
    None
}

/// Make setter function
pub fn make_setter(id: &str) -> String {
    format!("{}{}", FN_SET, id)
}

/// Extract the property name from a setter function name.
fn extract_prop_from_setter(fn_name: &str) -> Option<&str> {
    #[cfg(not(feature = "no_object"))]
    if fn_name.starts_with(FN_SET) {
        Some(&fn_name[FN_SET.len()..])
    } else {
        None
    }

    #[cfg(feature = "no_object")]
    None
}

/// Print/debug to stdout
fn default_print(s: &str) {
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(target_arch = "wasm32"))]
    println!("{}", s);
}

/// Search for a module within an imports stack.
/// Position in `EvalAltResult` is None and must be set afterwards.
fn search_imports<'s>(
    mods: &'s mut Imports,
    state: &mut State,
    modules: &Box<ModuleRef>,
) -> Result<&'s mut Module, Box<EvalAltResult>> {
    let (root, root_pos) = modules.get(0);

    // Qualified - check if the root module is directly indexed
    let index = if state.always_search {
        None
    } else {
        modules.index()
    };

    Ok(if let Some(index) = index {
        let offset = mods.len() - index.get();
        &mut mods.get_mut(offset).unwrap().1
    } else {
        mods.iter_mut()
            .rev()
            .find(|(n, _)| n == root)
            .map(|(_, m)| m)
            .ok_or_else(|| {
                Box::new(EvalAltResult::ErrorModuleNotFound(
                    root.to_string(),
                    *root_pos,
                ))
            })?
    })
}

/// Search for a variable within the scope
fn search_scope<'s, 'a>(
    scope: &'s mut Scope,
    mods: &'s mut Imports,
    state: &mut State,
    this_ptr: &'s mut Option<&mut Dynamic>,
    expr: &'a Expr,
) -> Result<(&'s mut Dynamic, &'a str, ScopeEntryType, Position), Box<EvalAltResult>> {
    let ((name, pos), modules, hash_var, index) = match expr {
        Expr::Variable(v) => v.as_ref(),
        _ => unreachable!(),
    };

    // Check if the variable is `this`
    if name == KEYWORD_THIS {
        if let Some(val) = this_ptr {
            return Ok(((*val).into(), KEYWORD_THIS, ScopeEntryType::Normal, *pos));
        } else {
            return Err(Box::new(EvalAltResult::ErrorUnboundedThis(*pos)));
        }
    }

    // Check if it is qualified
    if let Some(modules) = modules {
        let module = search_imports(mods, state, modules)?;
        let target = module
            .get_qualified_var_mut(*hash_var)
            .map_err(|err| match *err {
                EvalAltResult::ErrorVariableNotFound(_, _) => Box::new(
                    EvalAltResult::ErrorVariableNotFound(format!("{}{}", modules, name), *pos),
                ),
                _ => err.new_position(*pos),
            })?;

        // Module variables are constant
        Ok((target, name, ScopeEntryType::Constant, *pos))
    } else {
        // Unqualified - check if it is directly indexed
        let index = if state.always_search { None } else { *index };

        let index = if let Some(index) = index {
            scope.len() - index.get()
        } else {
            // Find the variable in the scope
            scope
                .get_index(name)
                .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(name.into(), *pos)))?
                .0
        };

        let (val, typ) = scope.get_mut(index);
        Ok((val, name, typ, *pos))
    }
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
            id: None,

            packages: Default::default(),
            global_module: Default::default(),
            module_resolver: None,

            type_names: None,
            disabled_symbols: None,
            custom_keywords: None,

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
            max_operations: 0,
            max_modules: usize::MAX,
            max_string_size: 0,
            max_array_size: 0,
            max_map_size: 0,
        }
    }

    /// Universal method for calling functions either registered with the `Engine` or written in Rhai.
    /// Position in `EvalAltResult` is None and must be set afterwards.
    ///
    /// ## WARNING
    ///
    /// Function call arguments be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_fn_raw(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        fn_name: &str,
        (hash_fn, hash_script): (u64, u64),
        args: &mut FnCallArgs,
        is_ref: bool,
        is_method: bool,
        def_val: Option<&Dynamic>,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        self.inc_operations(state)?;

        let native_only = hash_script == 0;

        // Check for stack overflow
        #[cfg(not(feature = "no_function"))]
        #[cfg(not(feature = "unchecked"))]
        if level > self.max_call_stack_depth {
            return Err(Box::new(
                EvalAltResult::ErrorStackOverflow(Position::none()),
            ));
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
            //
            // # Safety
            //
            // Blindly casting a a reference to another lifetime saves on allocations and string cloning,
            // but must be used with the utmost care.
            //
            // We can do this here because, at the end of this scope, we'd restore the original reference
            // with `restore_first_arg_of_method_call`. Therefore this shorter lifetime does not get "out".
            let this_pointer = mem::replace(args.get_mut(0).unwrap(), unsafe {
                mem::transmute(this_copy)
            });

            *old_this_ptr = Some(this_pointer);
        }

        /// This function restores the first argument that was replaced by `normalize_first_arg_of_method_call`.
        fn restore_first_arg<'a>(old_this_ptr: Option<&'a mut Dynamic>, args: &mut FnCallArgs<'a>) {
            if let Some(this_pointer) = old_this_ptr {
                mem::replace(args.get_mut(0).unwrap(), this_pointer);
            }
        }

        // Search for the function
        // First search in script-defined functions (can override built-in)
        // Then search registered native functions (can override packages)
        // Then search packages
        // NOTE: We skip script functions for global_module and packages, and native functions for lib
        let func = if !native_only {
            lib.get_fn(hash_script) //.or_else(|| lib.get_fn(hash_fn))
        } else {
            None
        }
        //.or_else(|| self.global_module.get_fn(hash_script))
        .or_else(|| self.global_module.get_fn(hash_fn))
        //.or_else(|| self.packages.get_fn(hash_script))
        .or_else(|| self.packages.get_fn(hash_fn));

        if let Some(func) = func {
            #[cfg(not(feature = "no_function"))]
            let need_normalize = is_ref && (func.is_pure() || (func.is_script() && !is_method));
            #[cfg(feature = "no_function")]
            let need_normalize = is_ref && func.is_pure();

            // Calling pure function but the first argument is a reference?
            normalize_first_arg(need_normalize, &mut this_copy, &mut old_this_ptr, args);

            #[cfg(not(feature = "no_function"))]
            if func.is_script() {
                // Run scripted function
                let fn_def = func.get_fn_def();

                // Method call of script function - map first argument to `this`
                return if is_method {
                    let (first, rest) = args.split_at_mut(1);
                    Ok((
                        self.call_script_fn(
                            scope,
                            mods,
                            state,
                            lib,
                            &mut Some(first[0]),
                            fn_name,
                            fn_def,
                            rest,
                            level,
                        )?,
                        false,
                    ))
                } else {
                    let result = self.call_script_fn(
                        scope, mods, state, lib, &mut None, fn_name, fn_def, args, level,
                    )?;

                    // Restore the original reference
                    restore_first_arg(old_this_ptr, args);

                    Ok((result, false))
                };
            }

            // Run external function
            let result = func.get_native_fn()(self, lib, args)?;

            // Restore the original reference
            restore_first_arg(old_this_ptr, args);

            // See if the function match print/debug (which requires special processing)
            return Ok(match fn_name {
                KEYWORD_PRINT => (
                    (self.print)(result.as_str().map_err(|typ| {
                        Box::new(EvalAltResult::ErrorMismatchOutputType(
                            self.map_type_name(type_name::<ImmutableString>()).into(),
                            typ.into(),
                            Position::none(),
                        ))
                    })?)
                    .into(),
                    false,
                ),
                KEYWORD_DEBUG => (
                    (self.debug)(result.as_str().map_err(|typ| {
                        Box::new(EvalAltResult::ErrorMismatchOutputType(
                            self.map_type_name(type_name::<ImmutableString>()).into(),
                            typ.into(),
                            Position::none(),
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
                Position::none(),
            )));
        }

        // Setter function not found?
        if let Some(prop) = extract_prop_from_setter(fn_name) {
            return Err(Box::new(EvalAltResult::ErrorDotExpr(
                format!("- property '{}' unknown or read-only", prop),
                Position::none(),
            )));
        }

        // index getter function not found?
        if fn_name == FN_IDX_GET && args.len() == 2 {
            return Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
                format!(
                    "{} [{}]",
                    self.map_type_name(args[0].type_name()),
                    self.map_type_name(args[1].type_name()),
                ),
                Position::none(),
            )));
        }

        // index setter function not found?
        if fn_name == FN_IDX_SET {
            return Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
                format!(
                    "{} [{}]=",
                    self.map_type_name(args[0].type_name()),
                    self.map_type_name(args[1].type_name()),
                ),
                Position::none(),
            )));
        }

        // Raise error
        Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
            format!(
                "{} ({})",
                fn_name,
                args.iter()
                    .map(|name| if name.is::<ImmutableString>() {
                        "&str | ImmutableString"
                    } else {
                        self.map_type_name((*name).type_name())
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Position::none(),
        )))
    }

    /// Call a script-defined function.
    /// Position in `EvalAltResult` is None and must be set afterwards.
    ///
    /// ## WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_script_fn(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        fn_name: &str,
        fn_def: &ScriptFnDef,
        args: &mut FnCallArgs,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let orig_scope_level = state.scope_level;
        state.scope_level += 1;

        let prev_scope_len = scope.len();
        let prev_mods_len = mods.len();

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
            .eval_stmt(scope, mods, state, lib, this_ptr, &fn_def.body, level + 1)
            .or_else(|err| match *err {
                // Convert return statement to return value
                EvalAltResult::Return(x, _) => Ok(x),
                EvalAltResult::ErrorInFunctionCall(name, err, _) => {
                    Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                        format!("{} > {}", fn_name, name),
                        err,
                        Position::none(),
                    )))
                }
                _ => Err(Box::new(EvalAltResult::ErrorInFunctionCall(
                    fn_name.to_string(),
                    err,
                    Position::none(),
                ))),
            });

        // Remove all local variables
        scope.rewind(prev_scope_len);
        mods.truncate(prev_mods_len);
        state.scope_level = orig_scope_level;

        result
    }

    // Has a system function an override?
    fn has_override(&self, lib: &Module, (hash_fn, hash_script): (u64, u64)) -> bool {
        // NOTE: We skip script functions for global_module and packages, and native functions for lib

        // First check script-defined functions
        lib.contains_fn(hash_script)
        //|| lib.contains_fn(hash_fn)
        // Then check registered functions
        //|| self.global_module.contains_fn(hash_script)
        || self.global_module.contains_fn(hash_fn)
        // Then check packages
        //|| self.packages.contains_fn(hash_script)
        || self.packages.contains_fn(hash_fn)
    }

    /// Perform an actual function call, taking care of special functions
    /// Position in `EvalAltResult` is None and must be set afterwards.
    ///
    /// ## WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    fn exec_fn_call(
        &self,
        state: &mut State,
        lib: &Module,
        fn_name: &str,
        native_only: bool,
        hash_script: u64,
        args: &mut FnCallArgs,
        is_ref: bool,
        is_method: bool,
        def_val: Option<&Dynamic>,
        level: usize,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
        let arg_types = args.iter().map(|a| a.type_id());
        let hash_fn = calc_fn_hash(empty(), fn_name, args.len(), arg_types);
        let hashes = (hash_fn, if native_only { 0 } else { hash_script });

        match fn_name {
            // type_of
            KEYWORD_TYPE_OF if args.len() == 1 && !self.has_override(lib, hashes) => Ok((
                self.map_type_name(args[0].type_name()).to_string().into(),
                false,
            )),

            // Fn
            KEYWORD_FN_PTR if args.len() == 1 && !self.has_override(lib, hashes) => {
                Err(Box::new(EvalAltResult::ErrorRuntime(
                    "'Fn' should not be called in method style. Try Fn(...);".into(),
                    Position::none(),
                )))
            }

            // eval - reaching this point it must be a method-style call
            KEYWORD_EVAL if args.len() == 1 && !self.has_override(lib, hashes) => {
                Err(Box::new(EvalAltResult::ErrorRuntime(
                    "'eval' should not be called in method style. Try eval(...);".into(),
                    Position::none(),
                )))
            }

            // Normal function call
            _ => {
                let mut scope = Scope::new();
                let mut mods = Imports::new();
                self.call_fn_raw(
                    &mut scope, &mut mods, state, lib, fn_name, hashes, args, is_ref, is_method,
                    def_val, level,
                )
            }
        }
    }

    /// Evaluate a text string as a script - used primarily for 'eval'.
    /// Position in `EvalAltResult` is None and must be set afterwards.
    fn eval_script_expr(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        script: &Dynamic,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let script = script.as_str().map_err(|typ| {
            EvalAltResult::ErrorMismatchOutputType(
                self.map_type_name(type_name::<ImmutableString>()).into(),
                typ.into(),
                Position::none(),
            )
        })?;

        // Compile the script text
        // No optimizations because we only run it once
        let mut ast = self.compile_with_scope_and_optimization_level(
            &Scope::new(),
            &[script],
            OptimizationLevel::None,
        )?;

        // If new functions are defined within the eval string, it is an error
        if ast.lib().num_fn() != 0 {
            return Err(ParseErrorType::WrongFnDefinition.into());
        }

        let statements = mem::take(ast.statements_mut());
        let ast = AST::new(statements, lib.clone());

        // Evaluate the AST
        let (result, operations) = self.eval_ast_with_scope_raw(scope, mods, &ast)?;

        state.operations += operations;
        self.inc_operations(state)?;

        return Ok(result);
    }

    /// Chain-evaluate a dot/index chain.
    /// Position in `EvalAltResult` is None and must be set afterwards.
    fn eval_dot_index_chain_helper(
        &self,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        target: &mut Target,
        rhs: &Expr,
        idx_values: &mut StaticVec<Dynamic>,
        chain_type: ChainType,
        level: usize,
        mut new_val: Option<Dynamic>,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        if chain_type == ChainType::None {
            panic!();
        }

        let is_ref = target.is_ref();
        let is_value = target.is_value();

        let next_chain = match rhs {
            Expr::Index(_) => ChainType::Index,
            Expr::Dot(_) => ChainType::Dot,
            _ => ChainType::None,
        };

        // Pop the last index value
        let mut idx_val = idx_values.pop();

        match chain_type {
            #[cfg(not(feature = "no_index"))]
            ChainType::Index => {
                let pos = rhs.position();

                match rhs {
                    // xxx[idx].expr... | xxx[idx][expr]...
                    Expr::Dot(x) | Expr::Index(x) => {
                        let (idx, expr, pos) = x.as_ref();
                        let idx_pos = idx.position();
                        let obj_ptr = &mut self
                            .get_indexed_mut(state, lib, target, idx_val, idx_pos, false, level)?;

                        self.eval_dot_index_chain_helper(
                            state, lib, this_ptr, obj_ptr, expr, idx_values, next_chain, level,
                            new_val,
                        )
                        .map_err(|err| err.new_position(*pos))
                    }
                    // xxx[rhs] = new_val
                    _ if new_val.is_some() => {
                        let mut idx_val2 = idx_val.clone();

                        match self.get_indexed_mut(state, lib, target, idx_val, pos, true, level) {
                            // Indexed value is an owned value - the only possibility is an indexer
                            // Try to call an index setter
                            Ok(obj_ptr) if obj_ptr.is_value() => {
                                let args =
                                    &mut [target.as_mut(), &mut idx_val2, &mut new_val.unwrap()];

                                self.exec_fn_call(
                                    state, lib, FN_IDX_SET, true, 0, args, is_ref, true, None,
                                    level,
                                )
                                .or_else(|err| match *err {
                                    // If there is no index setter, no need to set it back because the indexer is read-only
                                    EvalAltResult::ErrorFunctionNotFound(s, _)
                                        if s == FN_IDX_SET =>
                                    {
                                        Ok(Default::default())
                                    }
                                    _ => Err(err),
                                })?;
                            }
                            // Indexed value is a reference - update directly
                            Ok(ref mut obj_ptr) => {
                                obj_ptr
                                    .set_value(new_val.unwrap())
                                    .map_err(|err| err.new_position(rhs.position()))?;
                            }
                            Err(err) => match *err {
                                // No index getter - try to call an index setter
                                EvalAltResult::ErrorIndexingType(_, _) => {
                                    let args = &mut [
                                        target.as_mut(),
                                        &mut idx_val2,
                                        &mut new_val.unwrap(),
                                    ];

                                    self.exec_fn_call(
                                        state, lib, FN_IDX_SET, true, 0, args, is_ref, true, None,
                                        level,
                                    )?;
                                }
                                // Error
                                err => return Err(Box::new(err)),
                            },
                        }
                        Ok(Default::default())
                    }
                    // xxx[rhs]
                    _ => self
                        .get_indexed_mut(state, lib, target, idx_val, pos, false, level)
                        .map(|v| (v.clone_into_dynamic(), false)),
                }
            }

            #[cfg(not(feature = "no_object"))]
            ChainType::Dot => {
                match rhs {
                    // xxx.fn_name(arg_expr_list)
                    Expr::FnCall(x) if x.1.is_none() => {
                        let ((name, native, pos), _, hash, _, def_val) = x.as_ref();
                        let def_val = def_val.as_ref();

                        // Get a reference to the mutation target Dynamic
                        let (result, updated) = {
                            let obj = target.as_mut();
                            let idx = idx_val.downcast_mut::<StaticVec<Dynamic>>().unwrap();
                            let mut fn_name = name.as_ref();

                            // Check if it is a FnPtr call
                            if fn_name == KEYWORD_FN_PTR_CALL && obj.is::<FnPtr>() {
                                // Redirect function name
                                fn_name = obj.as_str().unwrap();
                                // Recalculate hash
                                let hash = calc_fn_hash(empty(), fn_name, idx.len(), empty());
                                // Arguments are passed as-is
                                let mut arg_values = idx.iter_mut().collect::<StaticVec<_>>();
                                let args = arg_values.as_mut();

                                // Map it to name(args) in function-call style
                                self.exec_fn_call(
                                    state, lib, fn_name, *native, hash, args, false, false,
                                    def_val, level,
                                )
                            } else {
                                let redirected: Option<ImmutableString>;
                                let mut hash = *hash;

                                // Check if it is a map method call in OOP style
                                if let Some(map) = obj.downcast_ref::<Map>() {
                                    if let Some(val) = map.get(fn_name) {
                                        if let Some(f) = val.downcast_ref::<FnPtr>() {
                                            // Remap the function name
                                            redirected = Some(f.get_fn_name().clone());
                                            fn_name = redirected.as_ref().unwrap();

                                            // Recalculate the hash based on the new function name
                                            hash =
                                                calc_fn_hash(empty(), fn_name, idx.len(), empty());
                                        }
                                    }
                                };

                                // Attached object pointer in front of the arguments
                                let mut arg_values =
                                    once(obj).chain(idx.iter_mut()).collect::<StaticVec<_>>();
                                let args = arg_values.as_mut();

                                self.exec_fn_call(
                                    state, lib, fn_name, *native, hash, args, is_ref, true,
                                    def_val, level,
                                )
                            }
                            .map_err(|err| err.new_position(*pos))?
                        };

                        // Feed the changed temp value back
                        if updated && !is_ref && !is_value {
                            let new_val = target.as_mut().clone();
                            target.set_value(new_val)?;
                        }

                        Ok((result, updated))
                    }
                    // xxx.module::fn_name(...) - syntax error
                    Expr::FnCall(_) => unreachable!(),
                    // {xxx:map}.id = ???
                    Expr::Property(x) if target.is::<Map>() && new_val.is_some() => {
                        let ((prop, _, _), pos) = x.as_ref();
                        let index = prop.clone().into();
                        let mut val =
                            self.get_indexed_mut(state, lib, target, index, *pos, true, level)?;

                        val.set_value(new_val.unwrap())
                            .map_err(|err| err.new_position(rhs.position()))?;
                        Ok((Default::default(), true))
                    }
                    // {xxx:map}.id
                    Expr::Property(x) if target.is::<Map>() => {
                        let ((prop, _, _), pos) = x.as_ref();
                        let index = prop.clone().into();
                        let val =
                            self.get_indexed_mut(state, lib, target, index, *pos, false, level)?;

                        Ok((val.clone_into_dynamic(), false))
                    }
                    // xxx.id = ???
                    Expr::Property(x) if new_val.is_some() => {
                        let ((_, _, setter), pos) = x.as_ref();
                        let mut args = [target.as_mut(), new_val.as_mut().unwrap()];
                        self.exec_fn_call(
                            state, lib, setter, true, 0, &mut args, is_ref, true, None, level,
                        )
                        .map(|(v, _)| (v, true))
                        .map_err(|err| err.new_position(*pos))
                    }
                    // xxx.id
                    Expr::Property(x) => {
                        let ((_, getter, _), pos) = x.as_ref();
                        let mut args = [target.as_mut()];
                        self.exec_fn_call(
                            state, lib, getter, true, 0, &mut args, is_ref, true, None, level,
                        )
                        .map(|(v, _)| (v, false))
                        .map_err(|err| err.new_position(*pos))
                    }
                    // {xxx:map}.prop[expr] | {xxx:map}.prop.expr
                    Expr::Index(x) | Expr::Dot(x) if target.is::<Map>() => {
                        let (prop, expr, pos) = x.as_ref();

                        let mut val = if let Expr::Property(p) = prop {
                            let ((prop, _, _), _) = p.as_ref();
                            let index = prop.clone().into();
                            self.get_indexed_mut(state, lib, target, index, *pos, false, level)?
                        } else {
                            unreachable!();
                        };

                        self.eval_dot_index_chain_helper(
                            state, lib, this_ptr, &mut val, expr, idx_values, next_chain, level,
                            new_val,
                        )
                        .map_err(|err| err.new_position(*pos))
                    }
                    // xxx.prop[expr] | xxx.prop.expr
                    Expr::Index(x) | Expr::Dot(x) => {
                        let (prop, expr, pos) = x.as_ref();
                        let args = &mut [target.as_mut(), &mut Default::default()];

                        let (mut val, updated) = if let Expr::Property(p) = prop {
                            let ((_, getter, _), _) = p.as_ref();
                            let args = &mut args[..1];
                            self.exec_fn_call(
                                state, lib, getter, true, 0, args, is_ref, true, None, level,
                            )
                            .map_err(|err| err.new_position(*pos))?
                        } else {
                            unreachable!();
                        };
                        let val = &mut val;
                        let target = &mut val.into();

                        let (result, may_be_changed) = self
                            .eval_dot_index_chain_helper(
                                state, lib, this_ptr, target, expr, idx_values, next_chain, level,
                                new_val,
                            )
                            .map_err(|err| err.new_position(*pos))?;

                        // Feed the value back via a setter just in case it has been updated
                        if updated || may_be_changed {
                            if let Expr::Property(p) = prop {
                                let ((_, _, setter), _) = p.as_ref();
                                // Re-use args because the first &mut parameter will not be consumed
                                args[1] = val;
                                self.exec_fn_call(
                                    state, lib, setter, true, 0, args, is_ref, true, None, level,
                                )
                                .or_else(|err| match *err {
                                    // If there is no setter, no need to feed it back because the property is read-only
                                    EvalAltResult::ErrorDotExpr(_, _) => Ok(Default::default()),
                                    _ => Err(err.new_position(*pos)),
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

            _ => unreachable!(),
        }
    }

    /// Evaluate a dot/index chain.
    fn eval_dot_index_chain(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
        new_val: Option<Dynamic>,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let ((dot_lhs, dot_rhs, op_pos), chain_type) = match expr {
            Expr::Index(x) => (x.as_ref(), ChainType::Index),
            Expr::Dot(x) => (x.as_ref(), ChainType::Dot),
            _ => unreachable!(),
        };

        let idx_values = &mut StaticVec::new();

        self.eval_indexed_chain(
            scope, mods, state, lib, this_ptr, dot_rhs, idx_values, 0, level,
        )?;

        match dot_lhs {
            // id.??? or id[???]
            Expr::Variable(x) => {
                let (var_name, var_pos) = &x.0;

                self.inc_operations(state)
                    .map_err(|err| err.new_position(*var_pos))?;

                let (target, _, typ, pos) = search_scope(scope, mods, state, this_ptr, dot_lhs)?;

                // Constants cannot be modified
                match typ {
                    ScopeEntryType::Constant if new_val.is_some() => {
                        return Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                            var_name.to_string(),
                            pos,
                        )));
                    }
                    ScopeEntryType::Constant | ScopeEntryType::Normal => (),
                }

                let obj_ptr = &mut target.into();
                self.eval_dot_index_chain_helper(
                    state, lib, &mut None, obj_ptr, dot_rhs, idx_values, chain_type, level, new_val,
                )
                .map(|(v, _)| v)
                .map_err(|err| err.new_position(*op_pos))
            }
            // {expr}.??? = ??? or {expr}[???] = ???
            expr if new_val.is_some() => {
                return Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(
                    expr.position(),
                )));
            }
            // {expr}.??? or {expr}[???]
            expr => {
                let val = self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
                let obj_ptr = &mut val.into();
                self.eval_dot_index_chain_helper(
                    state, lib, this_ptr, obj_ptr, dot_rhs, idx_values, chain_type, level, new_val,
                )
                .map(|(v, _)| v)
                .map_err(|err| err.new_position(*op_pos))
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
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        idx_values: &mut StaticVec<Dynamic>,
        size: usize,
        level: usize,
    ) -> Result<(), Box<EvalAltResult>> {
        self.inc_operations(state)
            .map_err(|err| err.new_position(expr.position()))?;

        match expr {
            Expr::FnCall(x) if x.1.is_none() => {
                let arg_values =
                    x.3.iter()
                        .map(|arg_expr| {
                            self.eval_expr(scope, mods, state, lib, this_ptr, arg_expr, level)
                        })
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
                    _ => self.eval_expr(scope, mods, state, lib, this_ptr, lhs, level)?,
                };

                // Push in reverse order
                self.eval_indexed_chain(
                    scope, mods, state, lib, this_ptr, rhs, idx_values, size, level,
                )?;

                idx_values.push(lhs_val);
            }
            _ => idx_values.push(self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?),
        }

        Ok(())
    }

    /// Get the value at the indexed position of a base type
    /// Position in `EvalAltResult` may be None and should be set afterwards.
    fn get_indexed_mut<'a>(
        &self,
        state: &mut State,
        lib: &Module,
        target: &'a mut Target,
        mut idx: Dynamic,
        idx_pos: Position,
        create: bool,
        level: usize,
    ) -> Result<Target<'a>, Box<EvalAltResult>> {
        self.inc_operations(state)?;

        let is_ref = target.is_ref();
        let val = target.as_mut();

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
                        .take_immutable_string()
                        .map_err(|_| EvalAltResult::ErrorStringIndexExpr(idx_pos))?;

                    map.entry(index).or_insert(Default::default()).into()
                } else {
                    let index = idx
                        .downcast_ref::<String>()
                        .ok_or_else(|| EvalAltResult::ErrorStringIndexExpr(idx_pos))?;

                    map.get_mut(index.as_str())
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

            #[cfg(not(feature = "no_index"))]
            _ => {
                let type_name = self.map_type_name(val.type_name());
                let args = &mut [val, &mut idx];
                self.exec_fn_call(
                    state, lib, FN_IDX_GET, true, 0, args, is_ref, true, None, level,
                )
                .map(|(v, _)| v.into())
                .map_err(|_| {
                    Box::new(EvalAltResult::ErrorIndexingType(
                        type_name.into(),
                        Position::none(),
                    ))
                })
            }

            #[cfg(feature = "no_index")]
            _ => Err(Box::new(EvalAltResult::ErrorIndexingType(
                self.map_type_name(val.type_name()).into(),
                Position::none(),
            ))),
        }
    }

    // Evaluate an 'in' expression
    fn eval_in_expr(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        lhs: &Expr,
        rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state)
            .map_err(|err| err.new_position(rhs.position()))?;

        let lhs_value = self.eval_expr(scope, mods, state, lib, this_ptr, lhs, level)?;
        let rhs_value = self.eval_expr(scope, mods, state, lib, this_ptr, rhs, level)?;

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

                    let hashes = (
                        // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
                        calc_fn_hash(empty(), op, args.len(), args.iter().map(|a| a.type_id())),
                        0,
                    );

                    let (r, _) = self
                        .call_fn_raw(
                            &mut scope, mods, state, lib, op, hashes, args, false, false,
                            def_value, level,
                        )
                        .map_err(|err| err.new_position(rhs.position()))?;
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
                Dynamic(Union::Char(c)) => {
                    Ok(rhs_value.contains_key(c.to_string().as_str()).into())
                }
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
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state)
            .map_err(|err| err.new_position(expr.position()))?;

        let result = match expr {
            Expr::Expr(x) => self.eval_expr(scope, mods, state, lib, this_ptr, x.as_ref(), level),

            Expr::IntegerConstant(x) => Ok(x.0.into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(x) => Ok(x.0.into()),
            Expr::StringConstant(x) => Ok(x.0.to_string().into()),
            Expr::CharConstant(x) => Ok(x.0.into()),
            Expr::Variable(x) if (x.0).0 == KEYWORD_THIS => {
                if let Some(ref val) = this_ptr {
                    Ok((*val).clone())
                } else {
                    Err(Box::new(EvalAltResult::ErrorUnboundedThis((x.0).1)))
                }
            }
            Expr::Variable(_) => {
                let (val, _, _, _) = search_scope(scope, mods, state, this_ptr, expr)?;
                Ok(val.clone())
            }
            Expr::Property(_) => unreachable!(),

            // Statement block
            Expr::Stmt(x) => self.eval_stmt(scope, mods, state, lib, this_ptr, &x.0, level),

            // var op= rhs
            Expr::Assignment(x) if matches!(x.0, Expr::Variable(_)) => {
                let (lhs_expr, op, rhs_expr, op_pos) = x.as_ref();
                let mut rhs_val =
                    self.eval_expr(scope, mods, state, lib, this_ptr, rhs_expr, level)?;
                let (lhs_ptr, name, typ, pos) =
                    search_scope(scope, mods, state, this_ptr, lhs_expr)?;
                self.inc_operations(state)
                    .map_err(|err| err.new_position(pos))?;

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
                            func(self, lib, &mut [lhs_ptr, &mut rhs_val])?;
                        } else if run_builtin_op_assignment(op, lhs_ptr, &rhs_val)?.is_none() {
                            // Not built in, map to `var = var op rhs`
                            let op = &op[..op.len() - 1]; // extract operator without =
                            let hash = calc_fn_hash(empty(), op, 2, empty());
                            let args = &mut [&mut lhs_ptr.clone(), &mut rhs_val];

                            // Set variable value
                            *lhs_ptr = self
                                .exec_fn_call(
                                    state, lib, op, true, hash, args, false, false, None, level,
                                )
                                .map(|(v, _)| v)
                                .map_err(|err| err.new_position(*op_pos))?;
                        }
                        Ok(Default::default())
                    }
                }
            }

            // lhs op= rhs
            Expr::Assignment(x) => {
                let (lhs_expr, op, rhs_expr, op_pos) = x.as_ref();
                let mut rhs_val =
                    self.eval_expr(scope, mods, state, lib, this_ptr, rhs_expr, level)?;

                let new_val = Some(if op.is_empty() {
                    // Normal assignment
                    rhs_val
                } else {
                    // Op-assignment - always map to `lhs = lhs op rhs`
                    let op = &op[..op.len() - 1]; // extract operator without =
                    let hash = calc_fn_hash(empty(), op, 2, empty());
                    let args = &mut [
                        &mut self.eval_expr(scope, mods, state, lib, this_ptr, lhs_expr, level)?,
                        &mut rhs_val,
                    ];
                    self.exec_fn_call(state, lib, op, true, hash, args, false, false, None, level)
                        .map(|(v, _)| v)
                        .map_err(|err| err.new_position(*op_pos))?
                });

                match lhs_expr {
                    // name op= rhs
                    Expr::Variable(_) => unreachable!(),
                    // idx_lhs[idx_expr] op= rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(_) => {
                        self.eval_dot_index_chain(
                            scope, mods, state, lib, this_ptr, lhs_expr, level, new_val,
                        )?;
                        Ok(Default::default())
                    }
                    // dot_lhs.dot_rhs op= rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(_) => {
                        self.eval_dot_index_chain(
                            scope, mods, state, lib, this_ptr, lhs_expr, level, new_val,
                        )?;
                        Ok(Default::default())
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
            Expr::Index(_) => {
                self.eval_dot_index_chain(scope, mods, state, lib, this_ptr, expr, level, None)
            }

            // lhs.dot_rhs
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(_) => {
                self.eval_dot_index_chain(scope, mods, state, lib, this_ptr, expr, level, None)
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Array(x) => Ok(Dynamic(Union::Array(Box::new(
                x.0.iter()
                    .map(|item| self.eval_expr(scope, mods, state, lib, this_ptr, item, level))
                    .collect::<Result<Vec<_>, _>>()?,
            )))),

            #[cfg(not(feature = "no_object"))]
            Expr::Map(x) => Ok(Dynamic(Union::Map(Box::new(
                x.0.iter()
                    .map(|((key, _), expr)| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                            .map(|val| (key.clone(), val))
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?,
            )))),

            // Normal function call
            Expr::FnCall(x) if x.1.is_none() => {
                let ((name, native, pos), _, hash, args_expr, def_val) = x.as_ref();
                let def_val = def_val.as_ref();

                // Handle Fn()
                if name == KEYWORD_FN_PTR && args_expr.len() == 1 {
                    let hash_fn =
                        calc_fn_hash(empty(), name, 1, once(TypeId::of::<ImmutableString>()));

                    if !self.has_override(lib, (hash_fn, *hash)) {
                        // Fn - only in function call style
                        let expr = args_expr.get(0);
                        let arg_value =
                            self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;

                        return arg_value
                            .take_immutable_string()
                            .map_err(|typ| {
                                Box::new(EvalAltResult::ErrorMismatchOutputType(
                                    self.map_type_name(type_name::<ImmutableString>()).into(),
                                    typ.into(),
                                    expr.position(),
                                ))
                            })
                            .and_then(|s| FnPtr::try_from(s))
                            .map(Into::<Dynamic>::into)
                            .map_err(|err| err.new_position(*pos));
                    }
                }

                // Handle eval()
                if name == KEYWORD_EVAL && args_expr.len() == 1 {
                    let hash_fn =
                        calc_fn_hash(empty(), name, 1, once(TypeId::of::<ImmutableString>()));

                    if !self.has_override(lib, (hash_fn, *hash)) {
                        // eval - only in function call style
                        let prev_len = scope.len();
                        let expr = args_expr.get(0);
                        let script =
                            self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
                        let result = self
                            .eval_script_expr(scope, mods, state, lib, &script)
                            .map_err(|err| err.new_position(expr.position()));

                        if scope.len() != prev_len {
                            // IMPORTANT! If the eval defines new variables in the current scope,
                            //            all variable offsets from this point on will be mis-aligned.
                            state.always_search = true;
                        }

                        return result;
                    }
                }

                // Normal function call - except for Fn and eval (handled above)
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
                                .map(|expr| {
                                    self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                                })
                                .collect::<Result<_, _>>()?;

                            let (target, _, _, pos) =
                                search_scope(scope, mods, state, this_ptr, lhs)?;

                            self.inc_operations(state)
                                .map_err(|err| err.new_position(pos))?;

                            args = once(target).chain(arg_values.iter_mut()).collect();

                            is_ref = true;
                        }
                        // func(..., ...)
                        _ => {
                            arg_values = args_expr
                                .iter()
                                .map(|expr| {
                                    self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                                })
                                .collect::<Result<_, _>>()?;

                            args = arg_values.iter_mut().collect();
                        }
                    }
                }

                let args = args.as_mut();
                self.exec_fn_call(
                    state, lib, name, *native, *hash, args, is_ref, false, def_val, level,
                )
                .map(|(v, _)| v)
                .map_err(|err| err.new_position(*pos))
            }

            // Module-qualified function call
            Expr::FnCall(x) if x.1.is_some() => {
                let ((name, _, pos), modules, hash_script, args_expr, def_val) = x.as_ref();
                let modules = modules.as_ref().unwrap();

                let mut arg_values = args_expr
                    .iter()
                    .map(|expr| self.eval_expr(scope, mods, state, lib, this_ptr, expr, level))
                    .collect::<Result<StaticVec<_>, _>>()?;

                let mut args: StaticVec<_> = arg_values.iter_mut().collect();

                let module = search_imports(mods, state, modules)?;

                // First search in script-defined functions (can override built-in)
                let func = match module.get_qualified_fn(*hash_script) {
                    Err(err) if matches!(*err, EvalAltResult::ErrorFunctionNotFound(_, _)) => {
                        // Then search in Rust functions
                        self.inc_operations(state)
                            .map_err(|err| err.new_position(*pos))?;

                        // Qualified Rust functions are indexed in two steps:
                        // 1) Calculate a hash in a similar manner to script-defined functions,
                        //    i.e. qualifiers + function name + number of arguments.
                        // 2) Calculate a second hash with no qualifiers, empty function name,
                        //    zero number of arguments, and the actual list of argument `TypeId`'.s
                        let hash_fn_args =
                            calc_fn_hash(empty(), "", 0, args.iter().map(|a| a.type_id()));
                        // 3) The final hash is the XOR of the two hashes.
                        let hash_qualified_fn = *hash_script ^ hash_fn_args;

                        module.get_qualified_fn(hash_qualified_fn)
                    }
                    r => r,
                };

                match func {
                    #[cfg(not(feature = "no_function"))]
                    Ok(f) if f.is_script() => {
                        let args = args.as_mut();
                        let fn_def = f.get_fn_def();
                        let mut scope = Scope::new();
                        let mut mods = Imports::new();
                        self.call_script_fn(
                            &mut scope, &mut mods, state, lib, &mut None, name, fn_def, args, level,
                        )
                        .map_err(|err| err.new_position(*pos))
                    }
                    Ok(f) => f.get_native_fn()(self, lib, args.as_mut())
                        .map_err(|err| err.new_position(*pos)),
                    Err(err) => match *err {
                        EvalAltResult::ErrorFunctionNotFound(_, _) if def_val.is_some() => {
                            Ok(def_val.clone().unwrap())
                        }
                        EvalAltResult::ErrorFunctionNotFound(_, _) => {
                            Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
                                format!("{}{}", modules, name),
                                *pos,
                            )))
                        }
                        _ => Err(err.new_position(*pos)),
                    },
                }
            }

            Expr::In(x) => self.eval_in_expr(scope, mods, state, lib, this_ptr, &x.0, &x.1, level),

            Expr::And(x) => {
                let (lhs, rhs, _) = x.as_ref();
                Ok((self
                    .eval_expr(scope, mods, state, lib, this_ptr, lhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), lhs.position())
                    })?
                    && // Short-circuit using &&
                self
                    .eval_expr(scope, mods, state, lib, this_ptr, rhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), rhs.position())
                    })?)
                .into())
            }

            Expr::Or(x) => {
                let (lhs, rhs, _) = x.as_ref();
                Ok((self
                    .eval_expr(scope, mods, state, lib, this_ptr, lhs, level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), lhs.position())
                    })?
                    || // Short-circuit using ||
                self
                    .eval_expr(scope, mods, state, lib, this_ptr, rhs, level)?
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
        };

        self.check_data_size(result)
            .map_err(|err| err.new_position(expr.position()))
    }

    /// Evaluate a statement
    pub(crate) fn eval_stmt(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &Module,
        this_ptr: &mut Option<&mut Dynamic>,
        stmt: &Stmt,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state)
            .map_err(|err| err.new_position(stmt.position()))?;

        let result = match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Default::default()),

            // Expression as statement
            Stmt::Expr(expr) => self.eval_expr(scope, mods, state, lib, this_ptr, expr, level),

            // Block scope
            Stmt::Block(x) => {
                let prev_scope_len = scope.len();
                let prev_mods_len = mods.len();
                state.scope_level += 1;

                let result = x.0.iter().try_fold(Default::default(), |_, stmt| {
                    self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level)
                });

                scope.rewind(prev_scope_len);
                mods.truncate(prev_mods_len);
                state.scope_level -= 1;

                // The impact of an eval statement goes away at the end of a block
                // because any new variables introduced will go out of scope
                state.always_search = false;

                result
            }

            // If-else statement
            Stmt::IfThenElse(x) => {
                let (expr, if_block, else_block) = x.as_ref();

                self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .as_bool()
                    .map_err(|_| Box::new(EvalAltResult::ErrorLogicGuard(expr.position())))
                    .and_then(|guard_val| {
                        if guard_val {
                            self.eval_stmt(scope, mods, state, lib, this_ptr, if_block, level)
                        } else if let Some(stmt) = else_block {
                            self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level)
                        } else {
                            Ok(Default::default())
                        }
                    })
            }

            // While loop
            Stmt::While(x) => loop {
                let (expr, body) = x.as_ref();

                match self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .as_bool()
                {
                    Ok(true) => {
                        match self.eval_stmt(scope, mods, state, lib, this_ptr, body, level) {
                            Ok(_) => (),
                            Err(err) => match *err {
                                EvalAltResult::ErrorLoopBreak(false, _) => (),
                                EvalAltResult::ErrorLoopBreak(true, _) => {
                                    return Ok(Default::default())
                                }
                                _ => return Err(err),
                            },
                        }
                    }
                    Ok(false) => return Ok(Default::default()),
                    Err(_) => {
                        return Err(Box::new(EvalAltResult::ErrorLogicGuard(expr.position())))
                    }
                }
            },

            // Loop statement
            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, mods, state, lib, this_ptr, body, level) {
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
                let iter_type = self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
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
                        self.inc_operations(state)
                            .map_err(|err| err.new_position(stmt.position()))?;

                        match self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level) {
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
                    self.eval_expr(
                        scope,
                        mods,
                        state,
                        lib,
                        this_ptr,
                        x.1.as_ref().unwrap(),
                        level,
                    )?,
                    (x.0).1,
                )))
            }

            // Empty return
            Stmt::ReturnWithVal(x) if (x.0).0 == ReturnType::Return => {
                Err(Box::new(EvalAltResult::Return(Default::default(), (x.0).1)))
            }

            // Throw value
            Stmt::ReturnWithVal(x) if x.1.is_some() && (x.0).0 == ReturnType::Exception => {
                let val = self.eval_expr(
                    scope,
                    mods,
                    state,
                    lib,
                    this_ptr,
                    x.1.as_ref().unwrap(),
                    level,
                )?;
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
                let val = self.eval_expr(
                    scope,
                    mods,
                    state,
                    lib,
                    this_ptr,
                    expr.as_ref().unwrap(),
                    level,
                )?;
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
                let val = self.eval_expr(scope, mods, state, lib, this_ptr, &expr, level)?;
                let var_name = unsafe_cast_var_name_to_lifetime(var_name, &state);
                scope.push_dynamic_value(var_name, ScopeEntryType::Constant, val, true);
                Ok(Default::default())
            }

            // Const expression not constant
            Stmt::Const(_) => unreachable!(),

            // Import statement
            Stmt::Import(x) => {
                let (expr, (name, pos)) = x.as_ref();

                // Guard against too many modules
                if state.modules >= self.max_modules {
                    return Err(Box::new(EvalAltResult::ErrorTooManyModules(*pos)));
                }

                if let Some(path) = self
                    .eval_expr(scope, mods, state, lib, this_ptr, &expr, level)?
                    .try_cast::<ImmutableString>()
                {
                    #[cfg(not(feature = "no_module"))]
                    if let Some(resolver) = &self.module_resolver {
                        let mut module = resolver.resolve(self, &path, expr.position())?;
                        module.index_all_sub_modules();
                        mods.push((name.clone().into(), module));

                        state.modules += 1;

                        Ok(Default::default())
                    } else {
                        Err(Box::new(EvalAltResult::ErrorModuleNotFound(
                            path.to_string(),
                            expr.position(),
                        )))
                    }

                    #[cfg(feature = "no_module")]
                    Ok(Default::default())
                } else {
                    Err(Box::new(EvalAltResult::ErrorImportExpr(expr.position())))
                }
            }

            // Export statement
            Stmt::Export(list) => {
                for ((id, id_pos), rename) in list.iter() {
                    // Mark scope variables as public
                    if let Some(index) = scope.get_index(id).map(|(i, _)| i) {
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
        };

        self.check_data_size(result)
            .map_err(|err| err.new_position(stmt.position()))
    }

    /// Check a result to ensure that the data size is within allowable limit.
    /// Position in `EvalAltResult` may be None and should be set afterwards.
    fn check_data_size(
        &self,
        result: Result<Dynamic, Box<EvalAltResult>>,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        #[cfg(feature = "unchecked")]
        return result;

        // If no data size limits, just return
        if self.max_string_size + self.max_array_size + self.max_map_size == 0 {
            return result;
        }

        // Recursively calculate the size of a value (especially `Array` and `Map`)
        fn calc_size(value: &Dynamic) -> (usize, usize, usize) {
            match value {
                #[cfg(not(feature = "no_index"))]
                Dynamic(Union::Array(arr)) => {
                    let mut arrays = 0;
                    let mut maps = 0;

                    arr.iter().for_each(|value| match value {
                        Dynamic(Union::Array(_)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        #[cfg(not(feature = "no_object"))]
                        Dynamic(Union::Map(_)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        _ => arrays += 1,
                    });

                    (arrays, maps, 0)
                }
                #[cfg(not(feature = "no_object"))]
                Dynamic(Union::Map(map)) => {
                    let mut arrays = 0;
                    let mut maps = 0;

                    map.values().for_each(|value| match value {
                        #[cfg(not(feature = "no_index"))]
                        Dynamic(Union::Array(_)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        Dynamic(Union::Map(_)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        _ => maps += 1,
                    });

                    (arrays, maps, 0)
                }
                Dynamic(Union::Str(s)) => (0, 0, s.len()),
                _ => (0, 0, 0),
            }
        }

        match result {
            // Simply return all errors
            Err(_) => return result,
            // String with limit
            Ok(Dynamic(Union::Str(_))) if self.max_string_size > 0 => (),
            // Array with limit
            #[cfg(not(feature = "no_index"))]
            Ok(Dynamic(Union::Array(_))) if self.max_array_size > 0 => (),
            // Map with limit
            #[cfg(not(feature = "no_object"))]
            Ok(Dynamic(Union::Map(_))) if self.max_map_size > 0 => (),
            // Everything else is simply returned
            Ok(_) => return result,
        };

        let (arr, map, s) = calc_size(result.as_ref().unwrap());

        if s > self.max_string_size {
            Err(Box::new(EvalAltResult::ErrorDataTooLarge(
                "Length of string".to_string(),
                self.max_string_size,
                s,
                Position::none(),
            )))
        } else if arr > self.max_array_size {
            Err(Box::new(EvalAltResult::ErrorDataTooLarge(
                "Size of array".to_string(),
                self.max_array_size,
                arr,
                Position::none(),
            )))
        } else if map > self.max_map_size {
            Err(Box::new(EvalAltResult::ErrorDataTooLarge(
                "Number of properties in object map".to_string(),
                self.max_map_size,
                map,
                Position::none(),
            )))
        } else {
            result
        }
    }

    /// Check if the number of operations stay within limit.
    /// Position in `EvalAltResult` is None and must be set afterwards.
    fn inc_operations(&self, state: &mut State) -> Result<(), Box<EvalAltResult>> {
        state.operations += 1;

        #[cfg(not(feature = "unchecked"))]
        // Guard against too many operations
        if self.max_operations > 0 && state.operations > self.max_operations {
            return Err(Box::new(EvalAltResult::ErrorTooManyOperations(
                Position::none(),
            )));
        }

        // Report progress - only in steps
        if let Some(progress) = &self.progress {
            if !progress(&state.operations) {
                // Terminate script if progress returns false
                return Err(Box::new(EvalAltResult::ErrorTerminated(Position::none())));
            }
        }

        Ok(())
    }

    /// Map a type_name into a pretty-print name
    pub(crate) fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .as_ref()
            .and_then(|t| t.get(name).map(String::as_str))
            .unwrap_or(map_std_type_name(name))
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
            "+" => return add(x, y).map(Into::into).map(Some),
            "-" => return sub(x, y).map(Into::into).map(Some),
            "*" => return mul(x, y).map(Into::into).map(Some),
            "/" => return div(x, y).map(Into::into).map(Some),
            "%" => return modulo(x, y).map(Into::into).map(Some),
            "~" => return pow_i_i(x, y).map(Into::into).map(Some),
            ">>" => return shr(x, y).map(Into::into).map(Some),
            "<<" => return shl(x, y).map(Into::into).map(Some),
            _ => (),
        }

        #[cfg(feature = "unchecked")]
        match op {
            "+" => return Ok(Some((x + y).into())),
            "-" => return Ok(Some((x - y).into())),
            "*" => return Ok(Some((x * y).into())),
            "/" => return Ok(Some((x / y).into())),
            "%" => return Ok(Some((x % y).into())),
            "~" => return pow_i_i_u(x, y).map(Into::into).map(Some),
            ">>" => return shr_u(x, y).map(Into::into).map(Some),
            "<<" => return shl_u(x, y).map(Into::into).map(Some),
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
            "^" => return Ok(Some((x ^ y).into())),
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
    if args_type == TypeId::of::<FLOAT>() {
        let x = *x.downcast_ref::<FLOAT>().unwrap();
        let y = *y.downcast_ref::<FLOAT>().unwrap();

        match op {
            "+" => return Ok(Some((x + y).into())),
            "-" => return Ok(Some((x - y).into())),
            "*" => return Ok(Some((x * y).into())),
            "/" => return Ok(Some((x / y).into())),
            "%" => return Ok(Some((x % y).into())),
            "~" => return pow_f_f(x, y).map(Into::into).map(Some),
            "==" => return Ok(Some((x == y).into())),
            "!=" => return Ok(Some((x != y).into())),
            ">" => return Ok(Some((x > y).into())),
            ">=" => return Ok(Some((x >= y).into())),
            "<" => return Ok(Some((x < y).into())),
            "<=" => return Ok(Some((x <= y).into())),
            _ => (),
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

    Ok(None)
}
