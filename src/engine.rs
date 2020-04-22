//! Main module defining the script evaluation `Engine`.

use crate::any::{Dynamic, Union};
use crate::calc_fn_hash;
use crate::error::ParseErrorType;
use crate::optimize::OptimizationLevel;
use crate::packages::{CorePackage, Package, PackageLibrary, StandardPackage};
use crate::parser::{Expr, FnDef, ReturnType, Stmt, INT};
use crate::result::EvalAltResult;
use crate::scope::{EntryRef as ScopeSource, EntryType as ScopeEntryType, Scope};
use crate::token::Position;

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    collections::{hash_map::DefaultHasher, HashMap},
    format,
    hash::{Hash, Hasher},
    iter::once,
    ops::{Deref, DerefMut},
    rc::Rc,
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};

/// An dynamic array of `Dynamic` values.
///
/// Not available under the `no_index` feature.
pub type Array = Vec<Dynamic>;

/// An dynamic hash map of `Dynamic` values with `String` keys.
///
/// Not available under the `no_object` feature.
pub type Map = HashMap<String, Dynamic>;

pub type FnCallArgs<'a> = [&'a mut Dynamic];

#[cfg(feature = "sync")]
pub type FnAny =
    dyn Fn(&mut FnCallArgs, Position) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&mut FnCallArgs, Position) -> Result<Dynamic, Box<EvalAltResult>>;

#[cfg(feature = "sync")]
pub type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

#[cfg(debug_assertions)]
pub const MAX_CALL_STACK_DEPTH: usize = 28;

#[cfg(not(debug_assertions))]
pub const MAX_CALL_STACK_DEPTH: usize = 256;

pub const KEYWORD_PRINT: &str = "print";
pub const KEYWORD_DEBUG: &str = "debug";
pub const KEYWORD_TYPE_OF: &str = "type_of";
pub const KEYWORD_EVAL: &str = "eval";
pub const FUNC_TO_STRING: &str = "to_string";
pub const FUNC_GETTER: &str = "get$";
pub const FUNC_SETTER: &str = "set$";

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
const FUNCTIONS_COUNT: usize = 512;

#[cfg(any(feature = "only_i32", feature = "only_i64"))]
const FUNCTIONS_COUNT: usize = 256;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
enum IndexValue {
    Num(usize),
    Str(String),
}

impl IndexValue {
    fn from_num(idx: INT) -> Self {
        Self::Num(idx as usize)
    }
    fn from_str(name: String) -> Self {
        Self::Str(name)
    }
    fn as_num(self) -> usize {
        match self {
            Self::Num(n) => n,
            _ => panic!("index value is numeric"),
        }
    }
    fn as_str(self) -> String {
        match self {
            Self::Str(s) => s,
            _ => panic!("index value is string"),
        }
    }
}

#[derive(Debug)]
enum Target<'a> {
    Scope(ScopeSource<'a>),
    Value(&'a mut Dynamic),
}

impl<'a> Target<'a> {
    fn get_mut(self, scope: &'a mut Scope) -> &'a mut Dynamic {
        match self {
            Self::Value(t) => t,
            Self::Scope(src) => scope.get_mut(src),
        }
    }
}

impl<'a> From<ScopeSource<'a>> for Target<'a> {
    fn from(src: ScopeSource<'a>) -> Self {
        Self::Scope(src)
    }
}

impl<'a> From<&'a mut Dynamic> for Target<'a> {
    fn from(value: &'a mut Dynamic) -> Self {
        Self::Value(value)
    }
}

/// A type that holds a library (`HashMap`) of script-defined functions.
///
/// Since script-defined functions have `Dynamic` parameters, functions with the same name
/// and number of parameters are considered equivalent.
///
/// The key of the `HashMap` is a `u64` hash calculated by the function `calc_fn_def`.
#[derive(Debug, Clone)]
pub struct FunctionsLib(
    #[cfg(feature = "sync")] HashMap<u64, Arc<FnDef>>,
    #[cfg(not(feature = "sync"))] HashMap<u64, Rc<FnDef>>,
);

impl FunctionsLib {
    /// Create a new `FunctionsLib`.
    pub fn new() -> Self {
        FunctionsLib(HashMap::new())
    }
    /// Create a new `FunctionsLib` from a collection of `FnDef`.
    pub fn from_vec(vec: Vec<FnDef>) -> Self {
        FunctionsLib(
            vec.into_iter()
                .map(|f| {
                    let hash = calc_fn_def(&f.name, f.params.len());

                    #[cfg(feature = "sync")]
                    {
                        (hash, Arc::new(f))
                    }
                    #[cfg(not(feature = "sync"))]
                    {
                        (hash, Rc::new(f))
                    }
                })
                .collect(),
        )
    }
    /// Does a certain function exist in the `FunctionsLib`?
    pub fn has_function(&self, name: &str, params: usize) -> bool {
        self.contains_key(&calc_fn_def(name, params))
    }
    /// Get a function definition from the `FunctionsLib`.
    pub fn get_function(&self, name: &str, params: usize) -> Option<&FnDef> {
        self.get(&calc_fn_def(name, params)).map(|f| f.as_ref())
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
    /// A collection of all library packages loaded into the engine.
    pub(crate) packages: Vec<PackageLibrary>,
    /// A `HashMap` containing all compiled functions known to the engine.
    ///
    /// The key of the `HashMap` is a `u64` hash calculated by the function `crate::calc_fn_hash`.
    pub(crate) functions: HashMap<u64, Box<FnAny>>,

    /// A hashmap containing all iterators known to the engine.
    pub(crate) type_iterators: HashMap<TypeId, Box<IteratorFn>>,
    /// A hashmap mapping type names to pretty-print names.
    pub(crate) type_names: Option<HashMap<String, String>>,

    /// Closure for implementing the `print` command.
    #[cfg(feature = "sync")]
    pub(crate) on_print: Option<Box<dyn Fn(&str) + Send + Sync + 'static>>,
    /// Closure for implementing the `print` command.
    #[cfg(not(feature = "sync"))]
    pub(crate) on_print: Option<Box<dyn Fn(&str) + 'static>>,

    /// Closure for implementing the `debug` command.
    #[cfg(feature = "sync")]
    pub(crate) on_debug: Option<Box<dyn Fn(&str) + Send + Sync + 'static>>,
    /// Closure for implementing the `debug` command.
    #[cfg(not(feature = "sync"))]
    pub(crate) on_debug: Option<Box<dyn Fn(&str) + 'static>>,

    /// Optimize the AST after compilation.
    pub(crate) optimization_level: OptimizationLevel,

    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Defaults to 28 for debug builds and 256 for non-debug builds.
    pub(crate) max_call_stack_depth: usize,
}

impl Default for Engine {
    fn default() -> Self {
        // Create the new scripting Engine
        let mut engine = Self {
            packages: Vec::new(),
            functions: HashMap::with_capacity(FUNCTIONS_COUNT),
            type_iterators: HashMap::new(),
            type_names: None,

            // default print/debug implementations
            on_print: Some(Box::new(default_print)),
            on_debug: Some(Box::new(default_print)),

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

/// Calculate a `u64` hash key from a function name and parameter types.
///
/// Parameter types are passed in via `TypeId` values from an iterator
/// which can come from any source.
pub fn calc_fn_spec(fn_name: &str, params: impl Iterator<Item = TypeId>) -> u64 {
    let mut s = DefaultHasher::new();
    fn_name.hash(&mut s);
    params.for_each(|t| t.hash(&mut s));
    s.finish()
}

/// Calculate a `u64` hash key from a function name and number of parameters (without regard to types).
pub(crate) fn calc_fn_def(fn_name: &str, params: usize) -> u64 {
    let mut s = DefaultHasher::new();
    fn_name.hash(&mut s);
    params.hash(&mut s);
    s.finish()
}

/// Print/debug to stdout
fn default_print(s: &str) {
    #[cfg(not(feature = "no_std"))]
    println!("{}", s);
}

/// Search for a variable within the scope, returning its value and index inside the Scope
fn search_scope<'a>(
    scope: &'a Scope,
    id: &str,
    begin: Position,
) -> Result<(ScopeSource<'a>, Dynamic), Box<EvalAltResult>> {
    scope
        .get(id)
        .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(id.into(), begin)))
}

/// Replace a character at an index position in a mutable string
fn str_replace_char(s: &mut String, idx: usize, new_ch: char) {
    let mut chars: Vec<char> = s.chars().collect();
    let ch = *chars.get(idx).expect("string index out of bounds");

    // See if changed - if so, update the String
    if ch != new_ch {
        chars[idx] = new_ch;
        s.clear();
        chars.iter().for_each(|&ch| s.push(ch));
    }
}

/// Update the value at an index position
fn update_indexed_val(
    mut target: Dynamic,
    idx: IndexValue,
    new_val: Dynamic,
    pos: Position,
) -> Result<Dynamic, Box<EvalAltResult>> {
    match target.get_mut() {
        Union::Array(arr) => {
            arr[idx.as_num()] = new_val;
        }
        Union::Map(map) => {
            map.insert(idx.as_str(), new_val);
        }
        Union::Str(s) => {
            // Value must be a character
            let ch = new_val
                .as_char()
                .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;
            str_replace_char(s, idx.as_num(), ch);
        }
        // All other variable types should be an error
        _ => panic!("invalid type for indexing: {}", target.type_name()),
    }

    Ok(target)
}

/// Update the value at an index position in a variable inside the scope
fn update_indexed_scope_var(
    scope: &mut Scope,
    src: ScopeSource,
    idx: IndexValue,
    new_val: Dynamic,
    pos: Position,
) -> Result<Dynamic, Box<EvalAltResult>> {
    let target = scope.get_mut(src);

    match target.get_mut() {
        // array_id[idx] = val
        Union::Array(arr) => {
            arr[idx.as_num()] = new_val;
        }
        // map_id[idx] = val
        Union::Map(map) => {
            map.insert(idx.as_str(), new_val);
        }
        // string_id[idx] = val
        Union::Str(s) => {
            // Value must be a character
            let ch = new_val
                .as_char()
                .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;
            str_replace_char(s, idx.as_num(), ch);
        }
        // All other variable types should be an error
        _ => panic!("invalid type for indexing: {}", target.type_name()),
    }

    Ok(().into())
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
            packages: Vec::new(),
            functions: HashMap::with_capacity(FUNCTIONS_COUNT / 2),
            type_iterators: HashMap::new(),
            type_names: None,
            on_print: None,
            on_debug: None,

            #[cfg(feature = "no_optimize")]
            optimization_level: OptimizationLevel::None,

            #[cfg(not(feature = "no_optimize"))]
            #[cfg(not(feature = "optimize_full"))]
            optimization_level: OptimizationLevel::Simple,

            #[cfg(not(feature = "no_optimize"))]
            #[cfg(feature = "optimize_full")]
            optimization_level: OptimizationLevel::Full,

            max_call_stack_depth: MAX_CALL_STACK_DEPTH,
        }
    }

    /// Load a new package into the `Engine`.
    ///
    /// When searching for functions, packages loaded later are preferred.
    /// In other words, loaded packages are searched in reverse order.
    pub fn load_package(&mut self, package: PackageLibrary) {
        // Push the package to the top - packages are searched in reverse order
        self.packages.insert(0, package);
    }

    /// Control whether and how the `Engine` will optimize an AST after compilation
    ///
    /// Not available under the `no_optimize` feature.
    #[cfg(not(feature = "no_optimize"))]
    pub fn set_optimization_level(&mut self, optimization_level: OptimizationLevel) {
        self.optimization_level = optimization_level
    }

    /// Set the maximum levels of function calls allowed for a script in order to avoid
    /// infinite recursion and stack overflows.
    pub fn set_max_call_levels(&mut self, levels: usize) {
        self.max_call_stack_depth = levels
    }

    /// Universal method for calling functions either registered with the `Engine` or written in Rhai
    pub(crate) fn call_fn_raw(
        &self,
        scope: Option<&mut Scope>,
        fn_lib: Option<&FunctionsLib>,
        fn_name: &str,
        args: &mut FnCallArgs,
        def_val: Option<&Dynamic>,
        pos: Position,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        // Check for stack overflow
        if level > self.max_call_stack_depth {
            return Err(Box::new(EvalAltResult::ErrorStackOverflow(pos)));
        }

        #[cfg(feature = "no_function")]
        const fn_lib: Option<&FunctionsLib> = None;

        // First search in script-defined functions (can override built-in)
        if let Some(fn_def) = fn_lib.and_then(|lib| lib.get_function(fn_name, args.len())) {
            match scope {
                // Extern scope passed in which is not empty
                Some(scope) if scope.len() > 0 => {
                    let scope_len = scope.len();

                    scope.extend(
                        // Put arguments into scope as variables - variable name is copied
                        // TODO - avoid copying variable name
                        fn_def
                            .params
                            .iter()
                            .zip(args.into_iter().map(|v| v.clone()))
                            .map(|(name, value)| (name.clone(), ScopeEntryType::Normal, value)),
                    );

                    // Evaluate the function at one higher level of call depth
                    let result = self
                        .eval_stmt(scope, fn_lib, &fn_def.body, level + 1)
                        .or_else(|err| match *err {
                            // Convert return statement to return value
                            EvalAltResult::Return(x, _) => Ok(x),
                            err => Err(Box::new(err.set_position(pos))),
                        });

                    scope.rewind(scope_len);

                    return result;
                }
                // No new scope - create internal scope
                _ => {
                    let mut scope = Scope::new();

                    scope.extend(
                        // Put arguments into scope as variables
                        fn_def
                            .params
                            .iter()
                            .zip(args.into_iter().map(|v| v.clone()))
                            .map(|(name, value)| (name, ScopeEntryType::Normal, value)),
                    );

                    // Evaluate the function at one higher level of call depth
                    return self
                        .eval_stmt(&mut scope, fn_lib, &fn_def.body, level + 1)
                        .or_else(|err| match *err {
                            // Convert return statement to return value
                            EvalAltResult::Return(x, _) => Ok(x),
                            err => Err(Box::new(err.set_position(pos))),
                        });
                }
            }
        }

        // Argument must be a string
        fn cast_to_string(r: &Dynamic, pos: Position) -> Result<&str, Box<EvalAltResult>> {
            r.as_str().map_err(|type_name| {
                Box::new(EvalAltResult::ErrorMismatchOutputType(
                    type_name.into(),
                    pos,
                ))
            })
        }

        // Search built-in's and external functions
        let fn_spec = calc_fn_hash(fn_name, args.iter().map(|a| a.type_id()));

        if let Some(func) = self.functions.get(&fn_spec).or_else(|| {
            self.packages
                .iter()
                .find(|pkg| pkg.functions.contains_key(&fn_spec))
                .and_then(|pkg| pkg.functions.get(&fn_spec))
        }) {
            // Run external function
            let result = func(args, pos)?;

            // See if the function match print/debug (which requires special processing)
            return match fn_name {
                KEYWORD_PRINT if self.on_print.is_some() => {
                    self.on_print.as_ref().unwrap()(cast_to_string(&result, pos)?);
                    Ok(().into())
                }
                KEYWORD_DEBUG if self.on_debug.is_some() => {
                    self.on_debug.as_ref().unwrap()(cast_to_string(&result, pos)?);
                    Ok(().into())
                }
                KEYWORD_PRINT | KEYWORD_DEBUG => Ok(().into()),
                _ => Ok(result),
            };
        }

        if let Some(prop) = extract_prop_from_getter(fn_name) {
            return match args[0] {
                // Map property access
                Dynamic(Union::Map(map)) => Ok(map.get(prop).cloned().unwrap_or_else(|| ().into())),

                // Getter function not found
                _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                    format!("- property '{}' unknown or write-only", prop),
                    pos,
                ))),
            };
        }

        if let Some(prop) = extract_prop_from_setter(fn_name) {
            let (arg, value) = args.split_at_mut(1);

            return match arg[0] {
                // Map property update
                Dynamic(Union::Map(map)) => {
                    map.insert(prop.to_string(), value[0].clone());
                    Ok(().into())
                }

                // Setter function not found
                _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                    format!("- property '{}' unknown or read-only", prop),
                    pos,
                ))),
            };
        }

        if let Some(val) = def_val {
            // Return default value
            return Ok(val.clone());
        }

        // Raise error
        let types_list: Vec<_> = args
            .iter()
            .map(|x| x.type_name())
            .map(|name| self.map_type_name(name))
            .collect();

        Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
            format!("{} ({})", fn_name, types_list.join(", ")),
            pos,
        )))
    }

    /// Chain-evaluate a dot setter.
    fn dot_get_helper(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        target: Target,
        dot_rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        match dot_rhs {
            // xxx.fn_name(arg_expr_list)
            Expr::FunctionCall(fn_name, arg_expr_list, def_val, pos) => {
                let mut values = arg_expr_list
                    .iter()
                    .map(|arg_expr| self.eval_expr(scope, fn_lib, arg_expr, level))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut args: Vec<_> = once(target.get_mut(scope))
                    .chain(values.iter_mut())
                    .collect();
                let def_val = def_val.as_ref();
                self.call_fn_raw(None, fn_lib, fn_name, &mut args, def_val, *pos, 0)
            }

            // xxx.id
            Expr::Property(id, pos) => {
                let mut args = [target.get_mut(scope)];
                self.call_fn_raw(None, fn_lib, &make_getter(id), &mut args, None, *pos, 0)
            }

            // xxx.idx_lhs[idx_expr]
            Expr::Index(idx_lhs, idx_expr, op_pos) => {
                let lhs_value = match idx_lhs.as_ref() {
                    // xxx.id[idx_expr]
                    Expr::Property(id, pos) => {
                        let mut args = [target.get_mut(scope)];
                        self.call_fn_raw(None, fn_lib, &make_getter(id), &mut args, None, *pos, 0)?
                    }
                    // xxx.???[???][idx_expr]
                    Expr::Index(_, _, _) => {
                        // Chain the indexing
                        self.dot_get_helper(scope, fn_lib, target, idx_lhs, level)?
                    }
                    // Syntax error
                    _ => {
                        return Err(Box::new(EvalAltResult::ErrorDotExpr(
                            "".to_string(),
                            dot_rhs.position(),
                        )))
                    }
                };

                self.get_indexed_val(scope, fn_lib, &lhs_value, idx_expr, *op_pos, level, false)
                    .map(|(val, _)| val)
            }

            // xxx.dot_lhs.rhs
            Expr::Dot(dot_lhs, rhs, _) => match dot_lhs.as_ref() {
                // xxx.id.rhs
                Expr::Property(id, pos) => {
                    let mut args = [target.get_mut(scope)];
                    self.call_fn_raw(None, fn_lib, &make_getter(id), &mut args, None, *pos, 0)
                        .and_then(|mut val| {
                            self.dot_get_helper(scope, fn_lib, (&mut val).into(), rhs, level)
                        })
                }
                // xxx.idx_lhs[idx_expr].rhs
                Expr::Index(idx_lhs, idx_expr, op_pos) => {
                    let val = match idx_lhs.as_ref() {
                        // xxx.id[idx_expr].rhs
                        Expr::Property(id, pos) => {
                            let fn_name = make_getter(id);
                            let mut args = [target.get_mut(scope)];
                            self.call_fn_raw(None, fn_lib, &fn_name, &mut args, None, *pos, 0)?
                        }
                        // xxx.???[???][idx_expr].rhs
                        Expr::Index(_, _, _) => {
                            self.dot_get_helper(scope, fn_lib, target, idx_lhs, level)?
                        }
                        // Syntax error
                        _ => {
                            return Err(Box::new(EvalAltResult::ErrorDotExpr(
                                "".to_string(),
                                dot_rhs.position(),
                            )))
                        }
                    };

                    self.get_indexed_val(scope, fn_lib, &val, idx_expr, *op_pos, level, false)
                        .and_then(|(mut val, _)| {
                            self.dot_get_helper(scope, fn_lib, (&mut val).into(), rhs, level)
                        })
                }
                // Syntax error
                _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                    "".to_string(),
                    dot_lhs.position(),
                ))),
            },

            // Syntax error
            _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                "".to_string(),
                dot_rhs.position(),
            ))),
        }
    }

    /// Evaluate a dot chain getter
    fn dot_get(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        match dot_lhs {
            // id.???
            Expr::Variable(id, pos) => {
                let (entry, _) = search_scope(scope, id, *pos)?;

                // Avoid referencing scope which is used below as mut
                let entry = ScopeSource { name: id, ..entry };

                // This is a variable property access (potential function call).
                // Use a direct index into `scope` to directly mutate the variable value.
                self.dot_get_helper(scope, fn_lib, entry.into(), dot_rhs, level)
            }

            // idx_lhs[idx_expr].???
            Expr::Index(idx_lhs, idx_expr, op_pos) => {
                let (src, index, mut val) =
                    self.eval_index_expr(scope, fn_lib, idx_lhs, idx_expr, *op_pos, level)?;
                let value = self.dot_get_helper(scope, fn_lib, (&mut val).into(), dot_rhs, level);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                if let Some(src) = src {
                    match src.typ {
                        ScopeEntryType::Constant => {
                            return Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                                src.name.to_string(),
                                idx_lhs.position(),
                            )));
                        }

                        ScopeEntryType::Normal => {
                            update_indexed_scope_var(scope, src, index, val, dot_rhs.position())?;
                        }
                    }
                }

                value
            }

            // {expr}.???
            expr => {
                let mut val = self.eval_expr(scope, fn_lib, expr, level)?;
                self.dot_get_helper(scope, fn_lib, (&mut val).into(), dot_rhs, level)
            }
        }
    }

    /// Get the value at the indexed position of a base type
    fn get_indexed_val(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        val: &Dynamic,
        idx_expr: &Expr,
        op_pos: Position,
        level: usize,
        only_index: bool,
    ) -> Result<(Dynamic, IndexValue), Box<EvalAltResult>> {
        let idx_pos = idx_expr.position();
        let type_name = self.map_type_name(val.type_name());

        match val.get_ref() {
            Union::Array(arr) => {
                // val_array[idx]
                let index = self
                    .eval_expr(scope, fn_lib, idx_expr, level)?
                    .as_int()
                    .map_err(|_| EvalAltResult::ErrorNumericIndexExpr(idx_expr.position()))?;

                let arr_len = arr.len();

                if index >= 0 {
                    arr.get(index as usize)
                        .map(|v| {
                            (
                                if only_index { ().into() } else { v.clone() },
                                IndexValue::from_num(index),
                            )
                        })
                        .ok_or_else(|| {
                            Box::new(EvalAltResult::ErrorArrayBounds(arr_len, index, idx_pos))
                        })
                } else {
                    Err(Box::new(EvalAltResult::ErrorArrayBounds(
                        arr_len, index, idx_pos,
                    )))
                }
            }

            Union::Map(map) => {
                // val_map[idx]
                let index = self
                    .eval_expr(scope, fn_lib, idx_expr, level)?
                    .take_string()
                    .map_err(|_| EvalAltResult::ErrorStringIndexExpr(idx_expr.position()))?;

                Ok((
                    map.get(&index)
                        .map(|v| if only_index { ().into() } else { v.clone() })
                        .unwrap_or_else(|| ().into()),
                    IndexValue::from_str(index),
                ))
            }

            Union::Str(s) => {
                // val_string[idx]
                let index = self
                    .eval_expr(scope, fn_lib, idx_expr, level)?
                    .as_int()
                    .map_err(|_| EvalAltResult::ErrorNumericIndexExpr(idx_expr.position()))?;

                let num_chars = s.chars().count();

                if index >= 0 {
                    s.chars()
                        .nth(index as usize)
                        .map(|ch| (ch.into(), IndexValue::from_num(index)))
                        .ok_or_else(|| {
                            Box::new(EvalAltResult::ErrorStringBounds(num_chars, index, idx_pos))
                        })
                } else {
                    Err(Box::new(EvalAltResult::ErrorStringBounds(
                        num_chars, index, idx_pos,
                    )))
                }
            }

            // Error - cannot be indexed
            _ => Err(Box::new(EvalAltResult::ErrorIndexingType(
                type_name.to_string(),
                op_pos,
            ))),
        }
    }

    /// Evaluate an index expression
    fn eval_index_expr<'a>(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        lhs: &'a Expr,
        idx_expr: &Expr,
        op_pos: Position,
        level: usize,
    ) -> Result<(Option<ScopeSource<'a>>, IndexValue, Dynamic), Box<EvalAltResult>> {
        match lhs {
            // id[idx_expr]
            Expr::Variable(name, _) => {
                let (ScopeSource { typ, index, .. }, val) =
                    search_scope(scope, &name, lhs.position())?;
                let (val, idx) =
                    self.get_indexed_val(scope, fn_lib, &val, idx_expr, op_pos, level, false)?;

                Ok((Some(ScopeSource { name, typ, index }), idx, val))
            }

            // (expr)[idx_expr]
            expr => {
                let val = self.eval_expr(scope, fn_lib, expr, level)?;
                self.get_indexed_val(scope, fn_lib, &val, idx_expr, op_pos, level, false)
                    .map(|(val, index)| (None, index, val))
            }
        }
    }

    /// Chain-evaluate a dot setter
    fn dot_set_helper(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        this_ptr: &mut Dynamic,
        dot_rhs: &Expr,
        new_val: &mut Dynamic,
        val_pos: Position,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        match dot_rhs {
            // xxx.id
            Expr::Property(id, pos) => {
                let mut args = [this_ptr, new_val];
                self.call_fn_raw(None, fn_lib, &make_setter(id), &mut args, None, *pos, 0)
            }

            // xxx.lhs[idx_expr]
            // TODO - Allow chaining of indexing!
            Expr::Index(lhs, idx_expr, op_pos) => match lhs.as_ref() {
                // xxx.id[idx_expr]
                Expr::Property(id, pos) => {
                    let fn_name = make_getter(id);
                    self.call_fn_raw(None, fn_lib, &fn_name, &mut [this_ptr], None, *pos, 0)
                        .and_then(|val| {
                            let (_, index) = self.get_indexed_val(
                                scope, fn_lib, &val, idx_expr, *op_pos, level, true,
                            )?;

                            update_indexed_val(val, index, new_val.clone(), val_pos)
                        })
                        .and_then(|mut val| {
                            let fn_name = make_setter(id);
                            let mut args = [this_ptr, &mut val];
                            self.call_fn_raw(None, fn_lib, &fn_name, &mut args, None, *pos, 0)
                        })
                }

                // All others - syntax error for setters chain
                _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                    "for assignment".to_string(),
                    *op_pos,
                ))),
            },

            // xxx.lhs.{...}
            Expr::Dot(lhs, rhs, _) => match lhs.as_ref() {
                // xxx.id.rhs
                Expr::Property(id, pos) => {
                    let fn_name = make_getter(id);
                    self.call_fn_raw(None, fn_lib, &fn_name, &mut [this_ptr], None, *pos, 0)
                        .and_then(|mut val| {
                            self.dot_set_helper(
                                scope, fn_lib, &mut val, rhs, new_val, val_pos, level,
                            )?;
                            Ok(val)
                        })
                        .and_then(|mut val| {
                            let fn_name = make_setter(id);
                            let mut args = [this_ptr, &mut val];
                            self.call_fn_raw(None, fn_lib, &fn_name, &mut args, None, *pos, 0)
                        })
                }

                // xxx.lhs[idx_expr].rhs
                // TODO - Allow chaining of indexing!
                Expr::Index(lhs, idx_expr, op_pos) => match lhs.as_ref() {
                    // xxx.id[idx_expr].rhs
                    Expr::Property(id, pos) => {
                        let fn_name = make_getter(id);
                        self.call_fn_raw(None, fn_lib, &fn_name, &mut [this_ptr], None, *pos, 0)
                            .and_then(|v| {
                                let (mut value, index) = self.get_indexed_val(
                                    scope, fn_lib, &v, idx_expr, *op_pos, level, false,
                                )?;

                                self.dot_set_helper(
                                    scope, fn_lib, &mut value, rhs, new_val, val_pos, level,
                                )?;

                                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                                update_indexed_val(v, index, value, val_pos)
                            })
                            .and_then(|mut v| {
                                let fn_name = make_setter(id);
                                let mut args = [this_ptr, &mut v];
                                self.call_fn_raw(None, fn_lib, &fn_name, &mut args, None, *pos, 0)
                            })
                    }

                    // All others - syntax error for setters chain
                    _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                        "for assignment".to_string(),
                        *op_pos,
                    ))),
                },

                // All others - syntax error for setters chain
                _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                    "for assignment".to_string(),
                    lhs.position(),
                ))),
            },

            // Syntax error
            _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                "for assignment".to_string(),
                dot_rhs.position(),
            ))),
        }
    }

    // Evaluate a dot chain setter
    fn dot_set(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        new_val: &mut Dynamic,
        val_pos: Position,
        op_pos: Position,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        match dot_lhs {
            // id.???
            Expr::Variable(id, pos) => {
                let (src, mut target) = search_scope(scope, id, *pos)?;

                match src.typ {
                    ScopeEntryType::Constant => Err(Box::new(
                        EvalAltResult::ErrorAssignmentToConstant(id.to_string(), op_pos),
                    )),
                    _ => {
                        // Avoid referencing scope which is used below as mut
                        let entry = ScopeSource { name: id, ..src };
                        let this_ptr = &mut target;
                        let value = self.dot_set_helper(
                            scope, fn_lib, this_ptr, dot_rhs, new_val, val_pos, level,
                        );

                        // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                        *scope.get_mut(entry) = target;

                        value
                    }
                }
            }

            // lhs[idx_expr].???
            // TODO - Allow chaining of indexing!
            Expr::Index(lhs, idx_expr, op_pos) => {
                let (src, index, mut target) =
                    self.eval_index_expr(scope, fn_lib, lhs, idx_expr, *op_pos, level)?;
                let this_ptr = &mut target;
                let value =
                    self.dot_set_helper(scope, fn_lib, this_ptr, dot_rhs, new_val, val_pos, level);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                if let Some(src) = src {
                    match src.typ {
                        ScopeEntryType::Constant => {
                            return Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                                src.name.to_string(),
                                lhs.position(),
                            )));
                        }
                        ScopeEntryType::Normal => {
                            update_indexed_scope_var(scope, src, index, target, val_pos)?;
                        }
                    }
                }

                value
            }

            // Syntax error
            _ => Err(Box::new(EvalAltResult::ErrorDotExpr(
                "for assignment".to_string(),
                dot_lhs.position(),
            ))),
        }
    }

    // Evaluate an 'in' expression
    fn eval_in_expr(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        lhs: &Expr,
        rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let mut lhs_value = self.eval_expr(scope, fn_lib, lhs, level)?;
        let rhs_value = self.eval_expr(scope, fn_lib, rhs, level)?;

        match rhs_value {
            Dynamic(Union::Array(mut rhs_value)) => {
                let def_value = false.into();
                let mut result = false;

                // Call the '==' operator to compare each value
                for value in rhs_value.iter_mut() {
                    let args = &mut [&mut lhs_value, value];
                    let def_value = Some(&def_value);
                    if self
                        .call_fn_raw(None, fn_lib, "==", args, def_value, rhs.position(), level)?
                        .as_bool()
                        .unwrap_or(false)
                    {
                        result = true;
                        break;
                    }
                }

                Ok(result.into())
            }
            Dynamic(Union::Map(rhs_value)) => {
                // Only allows String or char
                match lhs_value {
                    Dynamic(Union::Str(s)) => Ok(rhs_value.contains_key(s.as_ref()).into()),
                    Dynamic(Union::Char(c)) => Ok(rhs_value.contains_key(&c.to_string()).into()),
                    _ => Err(Box::new(EvalAltResult::ErrorInExpr(lhs.position()))),
                }
            }
            Dynamic(Union::Str(rhs_value)) => {
                // Only allows String or char
                match lhs_value {
                    Dynamic(Union::Str(s)) => Ok(rhs_value.contains(s.as_ref()).into()),
                    Dynamic(Union::Char(c)) => Ok(rhs_value.contains(c).into()),
                    _ => Err(Box::new(EvalAltResult::ErrorInExpr(lhs.position()))),
                }
            }
            _ => Err(Box::new(EvalAltResult::ErrorInExpr(rhs.position()))),
        }
    }

    /// Evaluate an expression
    fn eval_expr(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        expr: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        match expr {
            Expr::IntegerConstant(i, _) => Ok((*i).into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(f, _) => Ok((*f).into()),
            Expr::StringConstant(s, _) => Ok(s.to_string().into()),
            Expr::CharConstant(c, _) => Ok((*c).into()),
            Expr::Variable(id, pos) => search_scope(scope, id, *pos).map(|(_, val)| val),
            Expr::Property(_, _) => panic!("unexpected property."),

            // Statement block
            Expr::Stmt(stmt, _) => self.eval_stmt(scope, fn_lib, stmt, level),

            // lhs = rhs
            Expr::Assignment(lhs, rhs, op_pos) => {
                let mut rhs_val = self.eval_expr(scope, fn_lib, rhs, level)?;

                match lhs.as_ref() {
                    // name = rhs
                    Expr::Variable(name, pos) => match scope.get(name) {
                        None => {
                            return Err(Box::new(EvalAltResult::ErrorVariableNotFound(
                                name.to_string(),
                                *pos,
                            )))
                        }

                        Some((
                            entry
                            @
                            ScopeSource {
                                typ: ScopeEntryType::Normal,
                                ..
                            },
                            _,
                        )) => {
                            // Avoid referencing scope which is used below as mut
                            let entry = ScopeSource { name, ..entry };
                            *scope.get_mut(entry) = rhs_val.clone();
                            Ok(rhs_val)
                        }

                        Some((
                            ScopeSource {
                                typ: ScopeEntryType::Constant,
                                ..
                            },
                            _,
                        )) => Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                            name.to_string(),
                            *op_pos,
                        ))),
                    },

                    // idx_lhs[idx_expr] = rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(idx_lhs, idx_expr, op_pos) => {
                        let (src, index, _) =
                            self.eval_index_expr(scope, fn_lib, idx_lhs, idx_expr, *op_pos, level)?;

                        if let Some(src) = src {
                            match src.typ {
                                ScopeEntryType::Constant => {
                                    Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                                        src.name.to_string(),
                                        idx_lhs.position(),
                                    )))
                                }
                                ScopeEntryType::Normal => {
                                    let pos = rhs.position();
                                    Ok(update_indexed_scope_var(scope, src, index, rhs_val, pos)?)
                                }
                            }
                        } else {
                            Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(
                                idx_lhs.position(),
                            )))
                        }
                    }

                    // dot_lhs.dot_rhs = rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(dot_lhs, dot_rhs, _) => {
                        let new_val = &mut rhs_val;
                        let val_pos = rhs.position();
                        self.dot_set(
                            scope, fn_lib, dot_lhs, dot_rhs, new_val, val_pos, *op_pos, level,
                        )
                    }

                    // Error assignment to constant
                    expr if expr.is_constant() => {
                        Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                            expr.get_constant_str(),
                            lhs.position(),
                        )))
                    }

                    // Syntax error
                    _ => Err(Box::new(EvalAltResult::ErrorAssignmentToUnknownLHS(
                        lhs.position(),
                    ))),
                }
            }

            // lhs[idx_expr]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(lhs, idx_expr, op_pos) => self
                .eval_index_expr(scope, fn_lib, lhs, idx_expr, *op_pos, level)
                .map(|(_, _, x)| x),

            #[cfg(not(feature = "no_object"))]
            Expr::Dot(lhs, rhs, _) => self.dot_get(scope, fn_lib, lhs, rhs, level),

            #[cfg(not(feature = "no_index"))]
            Expr::Array(contents, _) => {
                let mut arr = Array::new();

                contents.into_iter().try_for_each(|item| {
                    self.eval_expr(scope, fn_lib, item, level)
                        .map(|val| arr.push(val))
                })?;

                Ok(Dynamic(Union::Array(Box::new(arr))))
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Map(contents, _) => {
                let mut map = Map::new();

                contents.into_iter().try_for_each(|(key, expr, _)| {
                    self.eval_expr(scope, fn_lib, &expr, level).map(|val| {
                        map.insert(key.clone(), val);
                    })
                })?;

                Ok(Dynamic(Union::Map(Box::new(map))))
            }

            Expr::FunctionCall(fn_name, args_expr_list, def_val, pos) => {
                // Has a system function an override?
                fn has_override(
                    engine: &Engine,
                    fn_lib: Option<&FunctionsLib>,
                    name: &str,
                ) -> bool {
                    engine
                        .functions
                        .contains_key(&calc_fn_hash(name, once(TypeId::of::<String>())))
                        || fn_lib.map_or(false, |lib| lib.has_function(name, 1))
                }

                match fn_name.as_ref() {
                    // type_of
                    KEYWORD_TYPE_OF
                        if args_expr_list.len() == 1
                            && !has_override(self, fn_lib, KEYWORD_TYPE_OF) =>
                    {
                        let result = self.eval_expr(scope, fn_lib, &args_expr_list[0], level)?;
                        Ok(self.map_type_name(result.type_name()).to_string().into())
                    }

                    // eval
                    KEYWORD_EVAL
                        if args_expr_list.len() == 1
                            && !has_override(self, fn_lib, KEYWORD_EVAL) =>
                    {
                        let pos = args_expr_list[0].position();
                        let result = self.eval_expr(scope, fn_lib, &args_expr_list[0], level)?;

                        // Get the script text by evaluating the expression
                        let script = result.as_str().map_err(|type_name| {
                            EvalAltResult::ErrorMismatchOutputType(type_name.into(), pos)
                        })?;

                        // Compile the script text
                        // No optimizations because we only run it once
                        let mut ast = self.compile_with_scope_and_optimization_level(
                            &Scope::new(),
                            script,
                            OptimizationLevel::None,
                        )?;

                        // If new functions are defined within the eval string, it is an error
                        if ast.1.len() > 0 {
                            return Err(Box::new(EvalAltResult::ErrorParsing(
                                ParseErrorType::WrongFnDefinition.into_err(pos),
                            )));
                        }

                        if let Some(lib) = fn_lib {
                            #[cfg(feature = "sync")]
                            {
                                ast.1 = Arc::new(lib.clone());
                            }
                            #[cfg(not(feature = "sync"))]
                            {
                                ast.1 = Rc::new(lib.clone());
                            }
                        }

                        // Evaluate the AST
                        self.eval_ast_with_scope_raw(scope, &ast)
                            .map_err(|err| Box::new(err.set_position(pos)))
                    }

                    // Normal function call
                    _ => {
                        let mut arg_values = args_expr_list
                            .iter()
                            .map(|expr| self.eval_expr(scope, fn_lib, expr, level))
                            .collect::<Result<Vec<_>, _>>()?;

                        let mut args: Vec<_> = arg_values.iter_mut().collect();
                        let def_val = def_val.as_ref();
                        self.call_fn_raw(None, fn_lib, fn_name, &mut args, def_val, *pos, level)
                    }
                }
            }

            Expr::In(lhs, rhs, _) => {
                self.eval_in_expr(scope, fn_lib, lhs.as_ref(), rhs.as_ref(), level)
            }

            Expr::And(lhs, rhs, _) => Ok((self
                    .eval_expr(scope, fn_lib,lhs.as_ref(), level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), lhs.position())
                    })?
                    && // Short-circuit using &&
                self
                    .eval_expr(scope, fn_lib,rhs.as_ref(), level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), rhs.position())
                    })?)
            .into()),

            Expr::Or(lhs, rhs, _) => Ok((self
                    .eval_expr(scope,fn_lib, lhs.as_ref(), level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), lhs.position())
                    })?
                    || // Short-circuit using ||
                self
                    .eval_expr(scope,fn_lib, rhs.as_ref(), level)?
                    .as_bool()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), rhs.position())
                    })?)
            .into()),

            Expr::True(_) => Ok(true.into()),
            Expr::False(_) => Ok(false.into()),
            Expr::Unit(_) => Ok(().into()),

            _ => panic!("should not appear: {:?}", expr),
        }
    }

    /// Evaluate a statement
    pub(crate) fn eval_stmt(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        stmt: &Stmt,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        match stmt {
            // No-op
            Stmt::Noop(_) => Ok(().into()),

            // Expression as statement
            Stmt::Expr(expr) => {
                let result = self.eval_expr(scope, fn_lib, expr, level)?;

                Ok(if !matches!(expr.as_ref(), Expr::Assignment(_, _, _)) {
                    result
                } else {
                    // If it is an assignment, erase the result at the root
                    ().into()
                })
            }

            // Block scope
            Stmt::Block(block, _) => {
                let prev_len = scope.len();

                let result = block.iter().try_fold(().into(), |_, stmt| {
                    self.eval_stmt(scope, fn_lib, stmt, level)
                });

                scope.rewind(prev_len);

                result
            }

            // If-else statement
            Stmt::IfThenElse(guard, if_body, else_body) => self
                .eval_expr(scope, fn_lib, guard, level)?
                .as_bool()
                .map_err(|_| Box::new(EvalAltResult::ErrorLogicGuard(guard.position())))
                .and_then(|guard_val| {
                    if guard_val {
                        self.eval_stmt(scope, fn_lib, if_body, level)
                    } else if let Some(stmt) = else_body {
                        self.eval_stmt(scope, fn_lib, stmt.as_ref(), level)
                    } else {
                        Ok(().into())
                    }
                }),

            // While loop
            Stmt::While(guard, body) => loop {
                match self.eval_expr(scope, fn_lib, guard, level)?.as_bool() {
                    Ok(true) => match self.eval_stmt(scope, fn_lib, body, level) {
                        Ok(_) => (),
                        Err(err) => match *err {
                            EvalAltResult::ErrorLoopBreak(false, _) => (),
                            EvalAltResult::ErrorLoopBreak(true, _) => return Ok(().into()),
                            _ => return Err(err),
                        },
                    },
                    Ok(false) => return Ok(().into()),
                    Err(_) => {
                        return Err(Box::new(EvalAltResult::ErrorLogicGuard(guard.position())))
                    }
                }
            },

            // Loop statement
            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, fn_lib, body, level) {
                    Ok(_) => (),
                    Err(err) => match *err {
                        EvalAltResult::ErrorLoopBreak(false, _) => (),
                        EvalAltResult::ErrorLoopBreak(true, _) => return Ok(().into()),
                        _ => return Err(err),
                    },
                }
            },

            // For loop
            Stmt::For(name, expr, body) => {
                let arr = self.eval_expr(scope, fn_lib, expr, level)?;
                let tid = arr.type_id();

                if let Some(iter_fn) = self.type_iterators.get(&tid).or_else(|| {
                    self.packages
                        .iter()
                        .find(|pkg| pkg.type_iterators.contains_key(&tid))
                        .and_then(|pkg| pkg.type_iterators.get(&tid))
                }) {
                    // Add the loop variable - variable name is copied
                    // TODO - avoid copying variable name
                    scope.push(name.clone(), ());

                    let entry = ScopeSource {
                        name,
                        index: scope.len() - 1,
                        typ: ScopeEntryType::Normal,
                    };

                    for a in iter_fn(&arr) {
                        *scope.get_mut(entry) = a;

                        match self.eval_stmt(scope, fn_lib, body, level) {
                            Ok(_) => (),
                            Err(err) => match *err {
                                EvalAltResult::ErrorLoopBreak(false, _) => (),
                                EvalAltResult::ErrorLoopBreak(true, _) => break,
                                _ => return Err(err),
                            },
                        }
                    }

                    scope.rewind(scope.len() - 1);
                    Ok(().into())
                } else {
                    Err(Box::new(EvalAltResult::ErrorFor(expr.position())))
                }
            }

            // Continue statement
            Stmt::Continue(pos) => Err(Box::new(EvalAltResult::ErrorLoopBreak(false, *pos))),

            // Break statement
            Stmt::Break(pos) => Err(Box::new(EvalAltResult::ErrorLoopBreak(true, *pos))),

            // Empty return
            Stmt::ReturnWithVal(None, ReturnType::Return, pos) => {
                Err(Box::new(EvalAltResult::Return(().into(), *pos)))
            }

            // Return value
            Stmt::ReturnWithVal(Some(a), ReturnType::Return, pos) => Err(Box::new(
                EvalAltResult::Return(self.eval_expr(scope, fn_lib, a, level)?, *pos),
            )),

            // Empty throw
            Stmt::ReturnWithVal(None, ReturnType::Exception, pos) => {
                Err(Box::new(EvalAltResult::ErrorRuntime("".into(), *pos)))
            }

            // Throw value
            Stmt::ReturnWithVal(Some(a), ReturnType::Exception, pos) => {
                let val = self.eval_expr(scope, fn_lib, a, level)?;
                Err(Box::new(EvalAltResult::ErrorRuntime(
                    val.take_string().unwrap_or_else(|_| "".to_string()),
                    *pos,
                )))
            }

            // Let statement
            Stmt::Let(name, Some(expr), _) => {
                let val = self.eval_expr(scope, fn_lib, expr, level)?;
                // TODO - avoid copying variable name in inner block?
                scope.push_dynamic_value(name.clone(), ScopeEntryType::Normal, val, false);
                Ok(().into())
            }

            Stmt::Let(name, None, _) => {
                // TODO - avoid copying variable name in inner block?
                scope.push(name.clone(), ());
                Ok(().into())
            }

            // Const statement
            Stmt::Const(name, expr, _) if expr.is_constant() => {
                let val = self.eval_expr(scope, fn_lib, expr, level)?;
                // TODO - avoid copying variable name in inner block?
                scope.push_dynamic_value(name.clone(), ScopeEntryType::Constant, val, true);
                Ok(().into())
            }

            Stmt::Const(_, _, _) => panic!("constant expression not constant!"),
        }
    }

    /// Map a type_name into a pretty-print name
    pub(crate) fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .as_ref()
            .and_then(|list| list.get(name).map(String::as_str))
            .unwrap_or(name)
    }
}
