//! Main module defining the script evaluation `Engine`.

use crate::any::{Dynamic, Union};
use crate::error::ParseErrorType;
use crate::optimize::OptimizationLevel;
use crate::parser::{Expr, FnDef, ReturnType, Stmt, INT};
use crate::result::EvalAltResult;
use crate::scope::{EntryRef as ScopeSource, EntryType as ScopeEntryType, Scope};
use crate::token::Position;

use crate::stdlib::{
    any::TypeId,
    borrow::Cow,
    boxed::Box,
    cmp::Ordering,
    collections::HashMap,
    format,
    iter::once,
    ops::{Deref, DerefMut},
    rc::Rc,
    string::{String, ToString},
    sync::Arc,
    vec,
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
pub type FnAny = dyn Fn(&mut FnCallArgs, Position) -> Result<Dynamic, EvalAltResult> + Send + Sync;
#[cfg(not(feature = "sync"))]
pub type FnAny = dyn Fn(&mut FnCallArgs, Position) -> Result<Dynamic, EvalAltResult>;

#[cfg(feature = "sync")]
type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + Send + Sync;
#[cfg(not(feature = "sync"))]
type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

#[cfg(debug_assertions)]
pub const MAX_CALL_STACK_DEPTH: usize = 32;

#[cfg(not(debug_assertions))]
pub const MAX_CALL_STACK_DEPTH: usize = 256;

pub const KEYWORD_PRINT: &str = "print";
pub const KEYWORD_DEBUG: &str = "debug";
pub const KEYWORD_TYPE_OF: &str = "type_of";
pub const KEYWORD_EVAL: &str = "eval";
pub const FUNC_TO_STRING: &str = "to_string";
pub const FUNC_GETTER: &str = "get$";
pub const FUNC_SETTER: &str = "set$";

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
enum IndexSourceType {
    Expression,
    String,
    Array,
    Map,
}

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
    fn from(value: &'a mut Dynamic) -> Self {
        Self::Value(value)
    }
    fn from_src(src: ScopeSource<'a>) -> Self {
        Self::Scope(src)
    }
    fn get_mut(self, scope: &'a mut Scope) -> &'a mut Dynamic {
        match self {
            Self::Value(t) => t,
            Self::Scope(src) => scope.get_mut(src),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct FnSpec<'a> {
    pub name: Cow<'a, str>,
    pub args: Vec<TypeId>,
}

/// A type that holds a library of script-defined functions.
///
/// Since script-defined functions have `Dynamic` parameters, functions with the same name
/// and number of parameters are considered equivalent.
///
/// Since the key is a combination of the function name (a String) plus the number of parameters,
/// we cannot use a `HashMap` because we don't want to clone the function name string just
/// to search for it.
///
/// So instead this is implemented as a sorted list and binary searched.
#[derive(Debug, Clone)]
pub struct FunctionsLib(
    #[cfg(feature = "sync")] Vec<Arc<FnDef>>,
    #[cfg(not(feature = "sync"))] Vec<Rc<FnDef>>,
);

impl FnDef {
    /// Function to order two FnDef records, for binary search.
    pub fn compare(&self, name: &str, params_len: usize) -> Ordering {
        // First order by name
        match self.name.as_str().cmp(name) {
            // Then by number of parameters
            Ordering::Equal => self.params.len().cmp(&params_len),
            order => order,
        }
    }
}

impl FunctionsLib {
    /// Create a new `FunctionsLib`.
    pub fn new() -> Self {
        FunctionsLib(Vec::new())
    }
    /// Create a new `FunctionsLib` from a collection of `FnDef`.
    pub fn from_vec(vec: Vec<FnDef>) -> Self {
        #[cfg(feature = "sync")]
        {
            FunctionsLib(vec.into_iter().map(Arc::new).collect())
        }
        #[cfg(not(feature = "sync"))]
        {
            FunctionsLib(vec.into_iter().map(Rc::new).collect())
        }
    }
    /// Does a certain function exist in the `FunctionsLib`?
    pub fn has_function(&self, name: &str, params: usize) -> bool {
        self.0.binary_search_by(|f| f.compare(name, params)).is_ok()
    }
    /// Get a function definition from the `FunctionsLib`.
    pub fn get_function(&self, name: &str, params: usize) -> Option<&FnDef> {
        self.0
            .binary_search_by(|f| f.compare(name, params))
            .ok()
            .map(|n| self.0[n].as_ref())
    }
    /// Merge another `FunctionsLib` into this `FunctionsLib`.
    pub fn merge(&self, other: &Self) -> Self {
        if self.is_empty() {
            other.clone()
        } else if other.is_empty() {
            self.clone()
        } else {
            let mut functions = self.clone();

            other.iter().cloned().for_each(|fn_def| {
                if let Some((n, _)) = functions
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.name == fn_def.name && f.params.len() == fn_def.params.len())
                {
                    functions[n] = fn_def;
                } else {
                    functions.push(fn_def);
                }
            });

            functions
        }
    }
}

impl Deref for FunctionsLib {
    #[cfg(feature = "sync")]
    type Target = Vec<Arc<FnDef>>;
    #[cfg(not(feature = "sync"))]
    type Target = Vec<Rc<FnDef>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FunctionsLib {
    #[cfg(feature = "sync")]
    fn deref_mut(&mut self) -> &mut Vec<Arc<FnDef>> {
        &mut self.0
    }
    #[cfg(not(feature = "sync"))]
    fn deref_mut(&mut self) -> &mut Vec<Rc<FnDef>> {
        &mut self.0
    }
}

/// Rhai main scripting engine.
///
/// ```
/// # fn main() -> Result<(), rhai::EvalAltResult> {
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
pub struct Engine<'e> {
    /// A hashmap containing all compiled functions known to the engine.
    pub(crate) functions: Option<HashMap<FnSpec<'e>, Box<FnAny>>>,

    /// A hashmap containing all iterators known to the engine.
    pub(crate) type_iterators: Option<HashMap<TypeId, Box<IteratorFn>>>,
    /// A hashmap mapping type names to pretty-print names.
    pub(crate) type_names: Option<HashMap<String, String>>,

    /// Closure for implementing the `print` command.
    #[cfg(feature = "sync")]
    pub(crate) on_print: Option<Box<dyn Fn(&str) + Send + Sync + 'e>>,
    /// Closure for implementing the `print` command.
    #[cfg(not(feature = "sync"))]
    pub(crate) on_print: Option<Box<dyn Fn(&str) + 'e>>,

    /// Closure for implementing the `debug` command.
    #[cfg(feature = "sync")]
    pub(crate) on_debug: Option<Box<dyn Fn(&str) + Send + Sync + 'e>>,
    /// Closure for implementing the `debug` command.
    #[cfg(not(feature = "sync"))]
    pub(crate) on_debug: Option<Box<dyn Fn(&str) + 'e>>,

    /// Optimize the AST after compilation.
    pub(crate) optimization_level: OptimizationLevel,

    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Defaults to 32 for debug builds and 256 for non-debug builds.
    pub(crate) max_call_stack_depth: usize,
}

impl Default for Engine<'_> {
    fn default() -> Self {
        // Create the new scripting Engine
        let mut engine = Engine {
            functions: None,
            type_iterators: None,
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

        engine.register_core_lib();

        #[cfg(not(feature = "no_stdlib"))]
        engine.register_stdlib();

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

impl Engine<'_> {
    /// Create a new `Engine`
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new `Engine` with minimal configurations without the standard library etc.
    pub fn new_raw() -> Self {
        let mut engine = Engine {
            functions: None,
            type_iterators: None,
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
        };

        engine.register_core_lib();

        engine
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
    ) -> Result<Dynamic, EvalAltResult> {
        // Check for stack overflow
        if level > self.max_call_stack_depth {
            return Err(EvalAltResult::ErrorStackOverflow(pos));
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
                        .or_else(|err| match err {
                            // Convert return statement to return value
                            EvalAltResult::Return(x, _) => Ok(x),
                            err => Err(err.set_position(pos)),
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
                        .or_else(|err| match err {
                            // Convert return statement to return value
                            EvalAltResult::Return(x, _) => Ok(x),
                            err => Err(err.set_position(pos)),
                        });
                }
            }
        }

        let spec = FnSpec {
            name: fn_name.into(),
            args: args.iter().map(|a| a.type_id()).collect(),
        };

        // Argument must be a string
        fn cast_to_string(r: &Dynamic, pos: Position) -> Result<&str, EvalAltResult> {
            r.as_str()
                .map_err(|type_name| EvalAltResult::ErrorMismatchOutputType(type_name.into(), pos))
        }

        // Search built-in's and external functions
        if let Some(func) = self.functions.as_ref().and_then(|f| f.get(&spec)) {
            // Run external function
            let result = func(args, pos)?;

            // See if the function match print/debug (which requires special processing)
            return match fn_name {
                KEYWORD_PRINT if self.on_print.is_some() => {
                    self.on_print.as_ref().unwrap()(cast_to_string(&result, pos)?);
                    Ok(Dynamic::from_unit())
                }
                KEYWORD_DEBUG if self.on_debug.is_some() => {
                    self.on_debug.as_ref().unwrap()(cast_to_string(&result, pos)?);
                    Ok(Dynamic::from_unit())
                }
                KEYWORD_PRINT | KEYWORD_DEBUG => Ok(Dynamic::from_unit()),
                _ => Ok(result),
            };
        }

        if let Some(prop) = extract_prop_from_getter(fn_name) {
            // Map property access
            if let Ok(map) = args[0].as_map() {
                return Ok(map
                    .get(prop)
                    .cloned()
                    .unwrap_or_else(|| Dynamic::from_unit()));
            }

            // Getter function not found
            return Err(EvalAltResult::ErrorDotExpr(
                format!("- property '{}' unknown or write-only", prop),
                pos,
            ));
        }

        if let Some(prop) = extract_prop_from_setter(fn_name) {
            let value = args[1].clone();

            // Map property update
            if let Dynamic(Union::Map(map)) = args[0] {
                map.insert(prop.to_string(), value);
                return Ok(Dynamic::from_unit());
            }

            // Setter function not found
            return Err(EvalAltResult::ErrorDotExpr(
                format!("- property '{}' unknown or read-only", prop),
                pos,
            ));
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

        Err(EvalAltResult::ErrorFunctionNotFound(
            format!("{} ({})", fn_name, types_list.join(", ")),
            pos,
        ))
    }

    /// Chain-evaluate a dot setter.
    fn get_dot_val_helper(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        target: Target,
        dot_rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            // xxx.fn_name(arg_expr_list)
            Expr::FunctionCall(fn_name, arg_expr_list, def_val, pos) => {
                let mut values = arg_expr_list
                    .iter()
                    .map(|arg_expr| self.eval_expr(scope, fn_lib, arg_expr, level))
                    .collect::<Result<Vec<_>, _>>()?;

                let this_ptr = target.get_mut(scope);

                let mut args: Vec<_> = once(this_ptr).chain(values.iter_mut()).collect();

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
                let value = match idx_lhs.as_ref() {
                    // xxx.id[idx_expr]
                    Expr::Property(id, pos) => {
                        let mut args = [target.get_mut(scope)];
                        self.call_fn_raw(None, fn_lib, &make_getter(id), &mut args, None, *pos, 0)?
                    }
                    // xxx.???[???][idx_expr]
                    Expr::Index(_, _, _) => {
                        self.get_dot_val_helper(scope, fn_lib, target, idx_lhs, level)?
                    }
                    // Syntax error
                    _ => {
                        return Err(EvalAltResult::ErrorDotExpr(
                            "".to_string(),
                            dot_rhs.position(),
                        ))
                    }
                };

                self.get_indexed_value(scope, fn_lib, &value, idx_expr, *op_pos, level)
                    .map(|(val, _, _)| val)
            }

            // xxx.dot_lhs.rhs
            Expr::Dot(dot_lhs, rhs, _) => match dot_lhs.as_ref() {
                // xxx.id.rhs
                Expr::Property(id, pos) => {
                    let mut args = [target.get_mut(scope)];
                    self.call_fn_raw(None, fn_lib, &make_getter(id), &mut args, None, *pos, 0)
                        .and_then(|mut val| {
                            let target = Target::from(&mut val);
                            self.get_dot_val_helper(scope, fn_lib, target, rhs, level)
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
                            self.get_dot_val_helper(scope, fn_lib, target, idx_lhs, level)?
                        }
                        // Syntax error
                        _ => {
                            return Err(EvalAltResult::ErrorDotExpr(
                                "".to_string(),
                                dot_rhs.position(),
                            ))
                        }
                    };

                    self.get_indexed_value(scope, fn_lib, &val, idx_expr, *op_pos, level)
                        .and_then(|(mut val, _, _)| {
                            let target = Target::from(&mut val);
                            self.get_dot_val_helper(scope, fn_lib, target, rhs, level)
                        })
                }
                // Syntax error
                _ => Err(EvalAltResult::ErrorDotExpr(
                    "".to_string(),
                    dot_lhs.position(),
                )),
            },

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(
                "".to_string(),
                dot_rhs.position(),
            )),
        }
    }

    /// Evaluate a dot chain getter
    fn get_dot_val(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            // id.???
            Expr::Variable(id, pos) => {
                let (entry, _) = Self::search_scope(scope, id, *pos)?;

                // Avoid referencing scope which is used below as mut
                let entry = ScopeSource { name: id, ..entry };

                // This is a variable property access (potential function call).
                // Use a direct index into `scope` to directly mutate the variable value.
                self.get_dot_val_helper(scope, fn_lib, Target::from_src(entry), dot_rhs, level)
            }

            // idx_lhs[idx_expr].???
            Expr::Index(idx_lhs, idx_expr, op_pos) => {
                let (idx_src_type, src, index, mut val) =
                    self.eval_index_expr(scope, fn_lib, idx_lhs, idx_expr, *op_pos, level)?;
                let target = Target::from(&mut val);
                let value = self.get_dot_val_helper(scope, fn_lib, target, dot_rhs, level);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                match src.map(|s| s.typ) {
                    None => (),

                    Some(ScopeEntryType::Constant) => {
                        return Err(EvalAltResult::ErrorAssignmentToConstant(
                            src.unwrap().name.to_string(),
                            idx_lhs.position(),
                        ));
                    }

                    Some(ScopeEntryType::Normal) => {
                        Self::update_indexed_var_in_scope(
                            idx_src_type,
                            scope,
                            src.unwrap(),
                            index,
                            (val, dot_rhs.position()),
                        )?;
                    }
                }

                value
            }

            // {expr}.???
            expr => {
                let mut val = self.eval_expr(scope, fn_lib, expr, level)?;
                self.get_dot_val_helper(scope, fn_lib, Target::from(&mut val), dot_rhs, level)
            }
        }
    }

    /// Search for a variable within the scope, returning its value and index inside the Scope
    fn search_scope<'a>(
        scope: &'a Scope,
        id: &str,
        begin: Position,
    ) -> Result<(ScopeSource<'a>, Dynamic), EvalAltResult> {
        scope
            .get(id)
            .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.into(), begin))
    }

    /// Get the value at the indexed position of a base type
    fn get_indexed_value(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        val: &Dynamic,
        idx_expr: &Expr,
        op_pos: Position,
        level: usize,
    ) -> Result<(Dynamic, IndexSourceType, IndexValue), EvalAltResult> {
        let idx_pos = idx_expr.position();

        // val_array[idx]
        if let Ok(arr) = val.as_array() {
            let index = self
                .eval_expr(scope, fn_lib, idx_expr, level)?
                .as_int()
                .map_err(|_| EvalAltResult::ErrorNumericIndexExpr(idx_expr.position()))?;

            return if index >= 0 {
                arr.get(index as usize)
                    .map(|v| {
                        (
                            v.clone(),
                            IndexSourceType::Array,
                            IndexValue::from_num(index),
                        )
                    })
                    .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), index, idx_pos))
            } else {
                Err(EvalAltResult::ErrorArrayBounds(arr.len(), index, idx_pos))
            };
        }

        // val_map[idx]
        if let Ok(map) = val.as_map() {
            let index = self
                .eval_expr(scope, fn_lib, idx_expr, level)?
                .take_string()
                .map_err(|_| EvalAltResult::ErrorStringIndexExpr(idx_expr.position()))?;

            return Ok((
                map.get(&index)
                    .cloned()
                    .unwrap_or_else(|| Dynamic::from_unit()),
                IndexSourceType::Map,
                IndexValue::from_str(index),
            ));
        }

        // val_string[idx]
        if let Ok(s) = val.as_str() {
            let index = self
                .eval_expr(scope, fn_lib, idx_expr, level)?
                .as_int()
                .map_err(|_| EvalAltResult::ErrorNumericIndexExpr(idx_expr.position()))?;

            return if index >= 0 {
                s.chars()
                    .nth(index as usize)
                    .map(|ch| {
                        (
                            Dynamic::from_char(ch),
                            IndexSourceType::String,
                            IndexValue::from_num(index),
                        )
                    })
                    .ok_or_else(|| {
                        EvalAltResult::ErrorStringBounds(s.chars().count(), index, idx_pos)
                    })
            } else {
                Err(EvalAltResult::ErrorStringBounds(
                    s.chars().count(),
                    index,
                    idx_pos,
                ))
            };
        }

        // Error - cannot be indexed
        Err(EvalAltResult::ErrorIndexingType(
            self.map_type_name(val.type_name()).to_string(),
            op_pos,
        ))
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
    ) -> Result<
        (
            IndexSourceType,
            Option<ScopeSource<'a>>,
            IndexValue,
            Dynamic,
        ),
        EvalAltResult,
    > {
        match lhs {
            // id[idx_expr]
            Expr::Variable(id, _) => {
                let (
                    ScopeSource {
                        typ: src_type,
                        index: src_idx,
                        ..
                    },
                    val,
                ) = Self::search_scope(scope, &id, lhs.position())?;

                let (val, idx_src_type, index) =
                    self.get_indexed_value(scope, fn_lib, &val, idx_expr, op_pos, level)?;

                Ok((
                    idx_src_type,
                    Some(ScopeSource {
                        name: &id,
                        typ: src_type,
                        index: src_idx,
                    }),
                    index,
                    val,
                ))
            }

            // (expr)[idx_expr]
            expr => {
                let val = self.eval_expr(scope, fn_lib, expr, level)?;

                self.get_indexed_value(scope, fn_lib, &val, idx_expr, op_pos, level)
                    .map(|(val, _, index)| (IndexSourceType::Expression, None, index, val))
            }
        }
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

    /// Update the value at an index position in a variable inside the scope
    fn update_indexed_var_in_scope(
        idx_src_type: IndexSourceType,
        scope: &mut Scope,
        src: ScopeSource,
        idx: IndexValue,
        new_val: (Dynamic, Position),
    ) -> Result<Dynamic, EvalAltResult> {
        match idx_src_type {
            // array_id[idx] = val
            IndexSourceType::Array => {
                let arr = scope.get_mut_by_type::<Array>(src);
                arr[idx.as_num()] = new_val.0;
                Ok(Dynamic::from_unit())
            }

            // map_id[idx] = val
            IndexSourceType::Map => {
                let arr = scope.get_mut_by_type::<Map>(src);
                arr.insert(idx.as_str(), new_val.0);
                Ok(Dynamic::from_unit())
            }

            // string_id[idx] = val
            IndexSourceType::String => {
                let s = scope.get_mut_by_type::<String>(src);
                let pos = new_val.1;
                // Value must be a character
                let ch = new_val
                    .0
                    .as_char()
                    .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;
                Self::str_replace_char(s, idx.as_num(), ch);
                Ok(Dynamic::from_unit())
            }

            IndexSourceType::Expression => panic!("expression cannot be indexed for update"),
        }
    }

    /// Update the value at an index position
    fn update_indexed_value(
        mut target: Dynamic,
        idx: IndexValue,
        new_val: Dynamic,
        pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        match target {
            Dynamic(Union::Array(ref mut arr)) => {
                arr[idx.as_num()] = new_val;
            }
            Dynamic(Union::Map(ref mut map)) => {
                map.insert(idx.as_str(), new_val);
            }
            Dynamic(Union::Str(ref mut s)) => {
                // Value must be a character
                let ch = new_val
                    .as_char()
                    .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;

                Self::str_replace_char(s, idx.as_num(), ch);
            }
            // All other variable types should be an error
            _ => panic!("array, map or string source type expected for indexing"),
        }

        Ok(target)
    }

    /// Chain-evaluate a dot setter
    fn set_dot_val_helper(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        this_ptr: &mut Dynamic,
        dot_rhs: &Expr,
        new_val: (&mut Dynamic, Position),
        level: usize,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            // xxx.id
            Expr::Property(id, pos) => {
                let mut args = [this_ptr, new_val.0];
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
                            let (_, _, index) = self
                                .get_indexed_value(scope, fn_lib, &val, idx_expr, *op_pos, level)?;

                            Self::update_indexed_value(val, index, new_val.0.clone(), new_val.1)
                        })
                        .and_then(|mut val| {
                            let fn_name = make_setter(id);
                            let mut args = [this_ptr, &mut val];
                            self.call_fn_raw(None, fn_lib, &fn_name, &mut args, None, *pos, 0)
                        })
                }

                // All others - syntax error for setters chain
                _ => Err(EvalAltResult::ErrorDotExpr(
                    "for assignment".to_string(),
                    *op_pos,
                )),
            },

            // xxx.lhs.{...}
            Expr::Dot(lhs, rhs, _) => match lhs.as_ref() {
                // xxx.id.rhs
                Expr::Property(id, pos) => {
                    let fn_name = make_getter(id);
                    self.call_fn_raw(None, fn_lib, &fn_name, &mut [this_ptr], None, *pos, 0)
                        .and_then(|mut val| {
                            self.set_dot_val_helper(scope, fn_lib, &mut val, rhs, new_val, level)
                                .map(|_| val) // Discard Ok return value
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
                                let (mut value, _, index) = self.get_indexed_value(
                                    scope, fn_lib, &v, idx_expr, *op_pos, level,
                                )?;

                                let val_pos = new_val.1;
                                let this_ptr = &mut value;
                                self.set_dot_val_helper(
                                    scope, fn_lib, this_ptr, rhs, new_val, level,
                                )?;

                                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                                Self::update_indexed_value(v, index, value, val_pos)
                            })
                            .and_then(|mut v| {
                                let fn_name = make_setter(id);
                                let mut args = [this_ptr, &mut v];
                                self.call_fn_raw(None, fn_lib, &fn_name, &mut args, None, *pos, 0)
                            })
                    }

                    // All others - syntax error for setters chain
                    _ => Err(EvalAltResult::ErrorDotExpr(
                        "for assignment".to_string(),
                        *op_pos,
                    )),
                },

                // All others - syntax error for setters chain
                _ => Err(EvalAltResult::ErrorDotExpr(
                    "for assignment".to_string(),
                    lhs.position(),
                )),
            },

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(
                "for assignment".to_string(),
                dot_rhs.position(),
            )),
        }
    }

    // Evaluate a dot chain setter
    fn set_dot_val(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        new_val: (&mut Dynamic, Position),
        op_pos: Position,
        level: usize,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            // id.???
            Expr::Variable(id, pos) => {
                let (src, mut target) = Self::search_scope(scope, id, *pos)?;

                match src.typ {
                    ScopeEntryType::Constant => Err(EvalAltResult::ErrorAssignmentToConstant(
                        id.to_string(),
                        op_pos,
                    )),
                    _ => {
                        // Avoid referencing scope which is used below as mut
                        let entry = ScopeSource { name: id, ..src };
                        let this_ptr = &mut target;
                        let value = self
                            .set_dot_val_helper(scope, fn_lib, this_ptr, dot_rhs, new_val, level);

                        // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                        *scope.get_mut(entry) = target;

                        value
                    }
                }
            }

            // lhs[idx_expr].???
            // TODO - Allow chaining of indexing!
            Expr::Index(lhs, idx_expr, op_pos) => {
                let (idx_src_type, src, index, mut target) =
                    self.eval_index_expr(scope, fn_lib, lhs, idx_expr, *op_pos, level)?;
                let val_pos = new_val.1;
                let this_ptr = &mut target;
                let value =
                    self.set_dot_val_helper(scope, fn_lib, this_ptr, dot_rhs, new_val, level);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                match src.map(|x| x.typ) {
                    None => (),

                    Some(ScopeEntryType::Constant) => {
                        return Err(EvalAltResult::ErrorAssignmentToConstant(
                            src.unwrap().name.to_string(),
                            lhs.position(),
                        ));
                    }

                    Some(ScopeEntryType::Normal) => {
                        Self::update_indexed_var_in_scope(
                            idx_src_type,
                            scope,
                            src.unwrap(),
                            index,
                            (target, val_pos),
                        )?;
                    }
                }

                value
            }

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(
                "for assignment".to_string(),
                dot_lhs.position(),
            )),
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
    ) -> Result<Dynamic, EvalAltResult> {
        let mut lhs_value = self.eval_expr(scope, fn_lib, lhs, level)?;
        let rhs_value = self.eval_expr(scope, fn_lib, rhs, level)?;

        match rhs_value {
            Dynamic(Union::Array(mut rhs_value)) => {
                let def_value = Dynamic::from_bool(false);
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

                Ok(Dynamic::from_bool(result))
            }
            Dynamic(Union::Map(rhs_value)) => {
                // Only allows String or char
                match lhs_value {
                    Dynamic(Union::Str(s)) => Ok(Dynamic::from_bool(rhs_value.contains_key(&s))),
                    Dynamic(Union::Char(c)) => {
                        Ok(Dynamic::from_bool(rhs_value.contains_key(&c.to_string())))
                    }
                    _ => Err(EvalAltResult::ErrorInExpr(lhs.position())),
                }
            }
            Dynamic(Union::Str(rhs_value)) => {
                // Only allows String or char
                match lhs_value {
                    Dynamic(Union::Str(s)) => Ok(Dynamic::from_bool(rhs_value.contains(&s))),
                    Dynamic(Union::Char(c)) => Ok(Dynamic::from_bool(rhs_value.contains(c))),
                    _ => Err(EvalAltResult::ErrorInExpr(lhs.position())),
                }
            }
            _ => Err(EvalAltResult::ErrorInExpr(rhs.position())),
        }
    }

    /// Evaluate an expression
    fn eval_expr(
        &self,
        scope: &mut Scope,
        fn_lib: Option<&FunctionsLib>,
        expr: &Expr,
        level: usize,
    ) -> Result<Dynamic, EvalAltResult> {
        match expr {
            Expr::IntegerConstant(i, _) => Ok(Dynamic::from_int(*i)),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(f, _) => Ok(Dynamic::from_float(*f)),
            Expr::StringConstant(s, _) => Ok(Dynamic::from_string(s.to_string())),
            Expr::CharConstant(c, _) => Ok(Dynamic::from_char(*c)),
            Expr::Variable(id, pos) => Self::search_scope(scope, id, *pos).map(|(_, val)| val),
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
                            return Err(EvalAltResult::ErrorVariableNotFound(
                                name.to_string(),
                                *pos,
                            ))
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
                        )) => Err(EvalAltResult::ErrorAssignmentToConstant(
                            name.to_string(),
                            *op_pos,
                        )),
                    },

                    // idx_lhs[idx_expr] = rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(idx_lhs, idx_expr, op_pos) => {
                        let (idx_src_type, src, index, _) =
                            self.eval_index_expr(scope, fn_lib, idx_lhs, idx_expr, *op_pos, level)?;

                        match src.map(|x| x.typ) {
                            None => Err(EvalAltResult::ErrorAssignmentToUnknownLHS(
                                idx_lhs.position(),
                            )),

                            Some(ScopeEntryType::Constant) => {
                                Err(EvalAltResult::ErrorAssignmentToConstant(
                                    src.unwrap().name.to_string(),
                                    idx_lhs.position(),
                                ))
                            }

                            Some(ScopeEntryType::Normal) => Ok(Self::update_indexed_var_in_scope(
                                idx_src_type,
                                scope,
                                src.unwrap(),
                                index,
                                (rhs_val, rhs.position()),
                            )?),
                        }
                    }

                    // dot_lhs.dot_rhs = rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(dot_lhs, dot_rhs, _) => {
                        let new_val = (&mut rhs_val, rhs.position());
                        self.set_dot_val(scope, fn_lib, dot_lhs, dot_rhs, new_val, *op_pos, level)
                    }

                    // Error assignment to constant
                    expr if expr.is_constant() => Err(EvalAltResult::ErrorAssignmentToConstant(
                        expr.get_constant_str(),
                        lhs.position(),
                    )),

                    // Syntax error
                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS(lhs.position())),
                }
            }

            // lhs[idx_expr]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(lhs, idx_expr, op_pos) => self
                .eval_index_expr(scope, fn_lib, lhs, idx_expr, *op_pos, level)
                .map(|(_, _, _, x)| x),

            #[cfg(not(feature = "no_object"))]
            Expr::Dot(lhs, rhs, _) => self.get_dot_val(scope, fn_lib, lhs, rhs, level),

            #[cfg(not(feature = "no_index"))]
            Expr::Array(contents, _) => {
                let mut arr = Array::new();

                contents.into_iter().try_for_each(|item| {
                    self.eval_expr(scope, fn_lib, item, level)
                        .map(|val| arr.push(val))
                })?;

                Ok(Dynamic(Union::Array(arr)))
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Map(contents, _) => {
                let mut map = Map::new();

                contents.into_iter().try_for_each(|item| {
                    self.eval_expr(scope, fn_lib, &item.1, level).map(|val| {
                        map.insert(item.0.clone(), val);
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
                    engine.functions.as_ref().map_or(false, |lib| {
                        lib.contains_key(&FnSpec {
                            name: name.into(),
                            args: vec![TypeId::of::<String>()],
                        })
                    }) || fn_lib.map_or(false, |lib| lib.has_function(name, 1))
                }

                match fn_name.as_ref() {
                    // type_of
                    KEYWORD_TYPE_OF
                        if args_expr_list.len() == 1
                            && !has_override(self, fn_lib, KEYWORD_TYPE_OF) =>
                    {
                        let result = self.eval_expr(scope, fn_lib, &args_expr_list[0], level)?;
                        Ok(Dynamic::from_string(
                            self.map_type_name(result.type_name()).to_string(),
                        ))
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
                        let mut ast = self
                            .compile_with_scope_and_optimization_level(
                                &Scope::new(),
                                script,
                                OptimizationLevel::None,
                            )
                            .map_err(EvalAltResult::ErrorParsing)?;

                        // If new functions are defined within the eval string, it is an error
                        if ast.1.len() > 0 {
                            return Err(EvalAltResult::ErrorParsing(
                                ParseErrorType::WrongFnDefinition.into_err(pos),
                            ));
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
                            .map_err(|err| err.set_position(pos))
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

            Expr::And(lhs, rhs, _) => Ok(Dynamic::from_bool(
                self
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
                    })?,
            )),

            Expr::Or(lhs, rhs, _) => Ok(Dynamic::from_bool(
                self
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
                    })?,
            )),

            Expr::True(_) => Ok(Dynamic::from_bool(true)),
            Expr::False(_) => Ok(Dynamic::from_bool(false)),
            Expr::Unit(_) => Ok(Dynamic::from_unit()),

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
    ) -> Result<Dynamic, EvalAltResult> {
        match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Dynamic::from_unit()),

            // Expression as statement
            Stmt::Expr(expr) => {
                let result = self.eval_expr(scope, fn_lib, expr, level)?;

                Ok(if !matches!(expr.as_ref(), Expr::Assignment(_, _, _)) {
                    result
                } else {
                    // If it is an assignment, erase the result at the root
                    Dynamic::from_unit()
                })
            }

            // Block scope
            Stmt::Block(block, _) => {
                let prev_len = scope.len();

                let result = block.iter().try_fold(Dynamic::from_unit(), |_, stmt| {
                    self.eval_stmt(scope, fn_lib, stmt, level)
                });

                scope.rewind(prev_len);

                result
            }

            // If-else statement
            Stmt::IfThenElse(guard, if_body, else_body) => self
                .eval_expr(scope, fn_lib, guard, level)?
                .as_bool()
                .map_err(|_| EvalAltResult::ErrorLogicGuard(guard.position()))
                .and_then(|guard_val| {
                    if guard_val {
                        self.eval_stmt(scope, fn_lib, if_body, level)
                    } else if let Some(stmt) = else_body {
                        self.eval_stmt(scope, fn_lib, stmt.as_ref(), level)
                    } else {
                        Ok(Dynamic::from_unit())
                    }
                }),

            // While loop
            Stmt::While(guard, body) => loop {
                match self.eval_expr(scope, fn_lib, guard, level)?.as_bool() {
                    Ok(guard_val) if guard_val => {
                        match self.eval_stmt(scope, fn_lib, body, level) {
                            Ok(_) | Err(EvalAltResult::ErrorLoopBreak(false, _)) => (),
                            Err(EvalAltResult::ErrorLoopBreak(true, _)) => {
                                return Ok(Dynamic::from_unit())
                            }
                            Err(x) => return Err(x),
                        }
                    }
                    Ok(_) => return Ok(Dynamic::from_unit()),
                    Err(_) => return Err(EvalAltResult::ErrorLogicGuard(guard.position())),
                }
            },

            // Loop statement
            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, fn_lib, body, level) {
                    Ok(_) | Err(EvalAltResult::ErrorLoopBreak(false, _)) => (),
                    Err(EvalAltResult::ErrorLoopBreak(true, _)) => return Ok(Dynamic::from_unit()),
                    Err(x) => return Err(x),
                }
            },

            // For loop
            Stmt::For(name, expr, body) => {
                let arr = self.eval_expr(scope, fn_lib, expr, level)?;
                let tid = arr.type_id();

                if let Some(iter_fn) = self.type_iterators.as_ref().and_then(|t| t.get(&tid)) {
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
                            Ok(_) | Err(EvalAltResult::ErrorLoopBreak(false, _)) => (),
                            Err(EvalAltResult::ErrorLoopBreak(true, _)) => break,
                            Err(x) => return Err(x),
                        }
                    }

                    scope.rewind(scope.len() - 1);
                    Ok(Dynamic::from_unit())
                } else {
                    Err(EvalAltResult::ErrorFor(expr.position()))
                }
            }

            // Continue statement
            Stmt::Continue(pos) => Err(EvalAltResult::ErrorLoopBreak(false, *pos)),

            // Break statement
            Stmt::Break(pos) => Err(EvalAltResult::ErrorLoopBreak(true, *pos)),

            // Empty return
            Stmt::ReturnWithVal(None, ReturnType::Return, pos) => {
                Err(EvalAltResult::Return(Dynamic::from_unit(), *pos))
            }

            // Return value
            Stmt::ReturnWithVal(Some(a), ReturnType::Return, pos) => Err(EvalAltResult::Return(
                self.eval_expr(scope, fn_lib, a, level)?,
                *pos,
            )),

            // Empty throw
            Stmt::ReturnWithVal(None, ReturnType::Exception, pos) => {
                Err(EvalAltResult::ErrorRuntime("".into(), *pos))
            }

            // Throw value
            Stmt::ReturnWithVal(Some(a), ReturnType::Exception, pos) => {
                let val = self.eval_expr(scope, fn_lib, a, level)?;
                Err(EvalAltResult::ErrorRuntime(
                    val.take_string().unwrap_or_else(|_| "".to_string()),
                    *pos,
                ))
            }

            // Let statement
            Stmt::Let(name, Some(expr), _) => {
                let val = self.eval_expr(scope, fn_lib, expr, level)?;
                // TODO - avoid copying variable name in inner block?
                scope.push_dynamic_value(name.clone(), ScopeEntryType::Normal, val, false);
                Ok(Dynamic::from_unit())
            }

            Stmt::Let(name, None, _) => {
                // TODO - avoid copying variable name in inner block?
                scope.push(name.clone(), ());
                Ok(Dynamic::from_unit())
            }

            // Const statement
            Stmt::Const(name, expr, _) if expr.is_constant() => {
                let val = self.eval_expr(scope, fn_lib, expr, level)?;
                // TODO - avoid copying variable name in inner block?
                scope.push_dynamic_value(name.clone(), ScopeEntryType::Constant, val, true);
                Ok(Dynamic::from_unit())
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

/// Print/debug to stdout
fn default_print(s: &str) {
    #[cfg(not(feature = "no_std"))]
    println!("{}", s);
}
