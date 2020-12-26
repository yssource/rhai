//! Main module defining the script evaluation [`Engine`].

use crate::ast::{Expr, FnCallExpr, Ident, ReturnType, Stmt};
use crate::dynamic::{map_std_type_name, AccessMode, Union, Variant};
use crate::fn_call::run_builtin_op_assignment;
use crate::fn_native::{
    CallableFunction, IteratorFn, OnDebugCallback, OnPrintCallback, OnProgressCallback,
    OnVarCallback,
};
use crate::module::NamespaceRef;
use crate::optimize::OptimizationLevel;
use crate::packages::{Package, StandardPackage};
use crate::r#unsafe::unsafe_cast_var_name_to_lifetime;
use crate::stdlib::{
    any::{type_name, TypeId},
    borrow::Cow,
    boxed::Box,
    collections::{HashMap, HashSet},
    fmt, format,
    hash::{Hash, Hasher},
    iter::{empty, once, FromIterator},
    num::NonZeroU64,
    num::NonZeroUsize,
    ops::DerefMut,
    string::{String, ToString},
};
use crate::syntax::CustomSyntax;
use crate::utils::{get_hasher, StraightHasherBuilder};
use crate::{
    calc_native_fn_hash, Dynamic, EvalAltResult, FnPtr, ImmutableString, Module, Position, Scope,
    Shared, StaticVec,
};

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_index"))]
pub const TYPICAL_ARRAY_SIZE: usize = 8; // Small arrays are typical

#[cfg(not(feature = "no_object"))]
use crate::Map;

#[cfg(not(feature = "no_object"))]
pub const TYPICAL_MAP_SIZE: usize = 8; // Small maps are typical

/// _(INTERNALS)_ A stack of imported [modules][Module].
/// Exported under the `internals` feature only.
///
/// ## WARNING
///
/// This type is volatile and may change.
//
// # Implementation Notes
//
// We cannot use &str or Cow<str> here because `eval` may load a [module][Module] and
// the module name will live beyond the AST of the eval script text.
// The best we can do is a shared reference.
#[derive(Debug, Clone, Default)]
pub struct Imports(StaticVec<(ImmutableString, Shared<Module>)>);

impl Imports {
    /// Get the length of this stack of imported [modules][Module].
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Is this stack of imported [modules][Module] empty?
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// Get the imported [modules][Module] at a particular index.
    pub fn get(&self, index: usize) -> Option<Shared<Module>> {
        self.0.get(index).map(|(_, m)| m).cloned()
    }
    /// Get the index of an imported [modules][Module] by name.
    pub fn find(&self, name: &str) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (key, _))| key.as_str() == name)
            .map(|(index, _)| index)
    }
    /// Push an imported [modules][Module] onto the stack.
    pub fn push(&mut self, name: impl Into<ImmutableString>, module: impl Into<Shared<Module>>) {
        self.0.push((name.into(), module.into()));
    }
    /// Truncate the stack of imported [modules][Module] to a particular length.
    pub fn truncate(&mut self, size: usize) {
        self.0.truncate(size);
    }
    /// Get an iterator to this stack of imported [modules][Module] in reverse order.
    #[allow(dead_code)]
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (&'a str, &'a Module)> + 'a {
        self.0
            .iter()
            .rev()
            .map(|(name, module)| (name.as_str(), module.as_ref()))
    }
    /// Get an iterator to this stack of imported [modules][Module] in reverse order.
    #[allow(dead_code)]
    pub(crate) fn iter_raw<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a ImmutableString, &'a Shared<Module>)> + 'a {
        self.0.iter().rev().map(|(n, m)| (n, m))
    }
    /// Get a consuming iterator to this stack of imported [modules][Module] in reverse order.
    pub fn into_iter(self) -> impl Iterator<Item = (ImmutableString, Shared<Module>)> {
        self.0.into_iter().rev()
    }
    /// Add a stream of imported [modules][Module].
    pub fn extend(&mut self, stream: impl Iterator<Item = (ImmutableString, Shared<Module>)>) {
        self.0.extend(stream)
    }
    /// Does the specified function hash key exist in this stack of imported [modules][Module]?
    #[allow(dead_code)]
    pub fn contains_fn(&self, hash: NonZeroU64) -> bool {
        self.0.iter().any(|(_, m)| m.contains_qualified_fn(hash))
    }
    /// Get specified function via its hash key.
    pub fn get_fn(&self, hash: NonZeroU64) -> Option<&CallableFunction> {
        self.0
            .iter()
            .rev()
            .find_map(|(_, m)| m.get_qualified_fn(hash))
    }
    /// Does the specified [`TypeId`][std::any::TypeId] iterator exist in this stack of imported [modules][Module]?
    #[allow(dead_code)]
    pub fn contains_iter(&self, id: TypeId) -> bool {
        self.0.iter().any(|(_, m)| m.contains_qualified_iter(id))
    }
    /// Get the specified [`TypeId`][std::any::TypeId] iterator.
    pub fn get_iter(&self, id: TypeId) -> Option<IteratorFn> {
        self.0
            .iter()
            .rev()
            .find_map(|(_, m)| m.get_qualified_iter(id))
    }
}

impl<'a, T: IntoIterator<Item = (&'a ImmutableString, &'a Shared<Module>)>> From<T> for Imports {
    fn from(value: T) -> Self {
        Self(
            value
                .into_iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        )
    }
}
impl FromIterator<(ImmutableString, Shared<Module>)> for Imports {
    fn from_iter<T: IntoIterator<Item = (ImmutableString, Shared<Module>)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

#[cfg(not(feature = "unchecked"))]
#[cfg(debug_assertions)]
pub const MAX_CALL_STACK_DEPTH: usize = 8;
#[cfg(not(feature = "unchecked"))]
#[cfg(debug_assertions)]
pub const MAX_EXPR_DEPTH: usize = 32;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(feature = "no_function"))]
#[cfg(debug_assertions)]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = 16;

#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_CALL_STACK_DEPTH: usize = 128;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_EXPR_DEPTH: usize = 128;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(feature = "no_function"))]
#[cfg(not(debug_assertions))]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = 32;

pub const KEYWORD_PRINT: &str = "print";
pub const KEYWORD_DEBUG: &str = "debug";
pub const KEYWORD_TYPE_OF: &str = "type_of";
pub const KEYWORD_EVAL: &str = "eval";
pub const KEYWORD_FN_PTR: &str = "Fn";
pub const KEYWORD_FN_PTR_CALL: &str = "call";
pub const KEYWORD_FN_PTR_CURRY: &str = "curry";
#[cfg(not(feature = "no_closure"))]
pub const KEYWORD_IS_SHARED: &str = "is_shared";
pub const KEYWORD_IS_DEF_VAR: &str = "is_def_var";
pub const KEYWORD_THIS: &str = "this";
#[cfg(not(feature = "no_object"))]
pub const FN_GET: &str = "get$";
#[cfg(not(feature = "no_object"))]
pub const FN_SET: &str = "set$";
#[cfg(not(feature = "no_index"))]
pub const FN_IDX_GET: &str = "index$get$";
#[cfg(not(feature = "no_index"))]
pub const FN_IDX_SET: &str = "index$set$";
#[cfg(not(feature = "no_function"))]
pub const FN_ANONYMOUS: &str = "anon$";
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
pub const OP_EQUALS: &str = "==";

/// A type specifying the method of chaining.
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ChainType {
    None,
    Index,
    Dot,
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
#[derive(Debug, Clone)]
pub enum IndexChainValue {
    None,
    FnCallArgs(StaticVec<Dynamic>),
    Value(Dynamic),
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
impl IndexChainValue {
    /// Return the `Dynamic` value.
    ///
    /// # Panics
    ///
    /// Panics if not `IndexChainValue::Value`.
    #[cfg(not(feature = "no_index"))]
    pub fn as_value(self) -> Dynamic {
        match self {
            Self::None | Self::FnCallArgs(_) => unreachable!("expecting IndexChainValue::Value"),
            Self::Value(value) => value,
        }
    }
    /// Return the `StaticVec<Dynamic>` value.
    ///
    /// # Panics
    ///
    /// Panics if not `IndexChainValue::FnCallArgs`.
    #[cfg(not(feature = "no_object"))]
    pub fn as_fn_call_args(self) -> StaticVec<Dynamic> {
        match self {
            Self::None | Self::Value(_) => unreachable!("expecting IndexChainValue::FnCallArgs"),
            Self::FnCallArgs(value) => value,
        }
    }
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
impl From<StaticVec<Dynamic>> for IndexChainValue {
    fn from(value: StaticVec<Dynamic>) -> Self {
        Self::FnCallArgs(value)
    }
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
impl From<Dynamic> for IndexChainValue {
    fn from(value: Dynamic) -> Self {
        Self::Value(value)
    }
}

/// A type that encapsulates a mutation target for an expression with side effects.
#[derive(Debug)]
pub enum Target<'a> {
    /// The target is a mutable reference to a `Dynamic` value somewhere.
    Ref(&'a mut Dynamic),
    /// The target is a mutable reference to a Shared `Dynamic` value.
    /// It holds both the access guard and the original shared value.
    #[cfg(not(feature = "no_closure"))]
    #[cfg(not(feature = "no_object"))]
    LockGuard((crate::dynamic::DynamicWriteLock<'a, Dynamic>, Dynamic)),
    /// The target is a temporary `Dynamic` value (i.e. the mutation can cause no side effects).
    Value(Dynamic),
    /// The target is a character inside a String.
    /// This is necessary because directly pointing to a char inside a String is impossible.
    #[cfg(not(feature = "no_index"))]
    StringChar(&'a mut Dynamic, usize, Dynamic),
}

impl<'a> Target<'a> {
    /// Is the `Target` a reference pointing to other data?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is_ref(&self) -> bool {
        match self {
            Self::Ref(_) => true,
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard(_) => true,
            Self::Value(_) => false,
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, _) => false,
        }
    }
    /// Is the `Target` an owned value?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is_value(&self) -> bool {
        match self {
            Self::Ref(_) => false,
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard(_) => false,
            Self::Value(_) => true,
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, _) => false,
        }
    }
    /// Is the `Target` a shared value?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is_shared(&self) -> bool {
        match self {
            Self::Ref(r) => r.is_shared(),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard(_) => true,
            Self::Value(r) => r.is_shared(),
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, _) => false,
        }
    }
    /// Is the `Target` a specific type?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn is<T: Variant + Clone>(&self) -> bool {
        match self {
            Target::Ref(r) => r.is::<T>(),
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Target::LockGuard((r, _)) => r.is::<T>(),
            Target::Value(r) => r.is::<T>(),
            #[cfg(not(feature = "no_index"))]
            Target::StringChar(_, _, _) => TypeId::of::<T>() == TypeId::of::<char>(),
        }
    }
    /// Get the value of the `Target` as a `Dynamic`, cloning a referenced value if necessary.
    #[inline(always)]
    pub fn take_or_clone(self) -> Dynamic {
        match self {
            Self::Ref(r) => r.clone(), // Referenced value is cloned
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard((_, orig)) => orig, // Original value is simply taken
            Self::Value(v) => v,       // Owned value is simply taken
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, ch) => ch, // Character is taken
        }
    }
    /// Take a `&mut Dynamic` reference from the `Target`.
    #[inline(always)]
    pub fn take_ref(self) -> Option<&'a mut Dynamic> {
        match self {
            Self::Ref(r) => Some(r),
            _ => None,
        }
    }
    /// Convert a shared or reference `Target` into a target with an owned value.
    #[inline(always)]
    pub fn into_owned(self) -> Target<'static> {
        self.take_or_clone().into()
    }
    /// Propagate a changed value back to the original source.
    /// This has no effect except for string indexing.
    #[cfg(not(feature = "no_object"))]
    #[inline(always)]
    pub fn propagate_changed_value(&mut self) {
        match self {
            Self::Ref(_) | Self::Value(_) => (),
            #[cfg(not(feature = "no_closure"))]
            Self::LockGuard(_) => (),
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, ch) => {
                let char_value = ch.clone();
                self.set_value(char_value, Position::NONE).unwrap();
            }
        }
    }
    /// Update the value of the `Target`.
    #[cfg(any(not(feature = "no_object"), not(feature = "no_index")))]
    pub fn set_value(&mut self, new_val: Dynamic, pos: Position) -> Result<(), Box<EvalAltResult>> {
        match self {
            Self::Ref(r) => **r = new_val,
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard((r, _)) => **r = new_val,
            Self::Value(_) => unreachable!(),
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(string, index, _) if string.is::<ImmutableString>() => {
                let mut s = string.write_lock::<ImmutableString>().unwrap();

                // Replace the character at the specified index position
                let new_ch = new_val.as_char().map_err(|err| {
                    Box::new(EvalAltResult::ErrorMismatchDataType(
                        err.to_string(),
                        "char".to_string(),
                        pos,
                    ))
                })?;

                let mut chars = s.chars().collect::<StaticVec<_>>();

                // See if changed - if so, update the String
                if chars[*index] != new_ch {
                    chars[*index] = new_ch;
                    *s = chars.iter().collect::<String>().into();
                }
            }
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, _) => unreachable!(),
        }

        Ok(())
    }
}

impl<'a> From<&'a mut Dynamic> for Target<'a> {
    #[inline(always)]
    fn from(value: &'a mut Dynamic) -> Self {
        #[cfg(not(feature = "no_closure"))]
        #[cfg(not(feature = "no_object"))]
        if value.is_shared() {
            // Cloning is cheap for a shared value
            let container = value.clone();
            return Self::LockGuard((value.write_lock::<Dynamic>().unwrap(), container));
        }

        Self::Ref(value)
    }
}

impl AsRef<Dynamic> for Target<'_> {
    #[inline(always)]
    fn as_ref(&self) -> &Dynamic {
        match self {
            Self::Ref(r) => *r,
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard((r, _)) => &**r,
            Self::Value(ref r) => r,
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, ref r) => r,
        }
    }
}

impl AsMut<Dynamic> for Target<'_> {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut Dynamic {
        match self {
            Self::Ref(r) => *r,
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard((r, _)) => r.deref_mut(),
            Self::Value(ref mut r) => r,
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(_, _, ref mut r) => r,
        }
    }
}

impl<T: Into<Dynamic>> From<T> for Target<'_> {
    #[inline(always)]
    fn from(value: T) -> Self {
        Self::Value(value.into())
    }
}

/// _(INTERNALS)_ A type that holds all the current states of the [`Engine`].
/// Exported under the `internals` feature only.
///
/// ## WARNING
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Default)]
pub struct State {
    /// Source of the current context.
    pub source: Option<ImmutableString>,
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
    /// Cached lookup values for function hashes.
    pub functions_cache: HashMap<NonZeroU64, Option<CallableFunction>, StraightHasherBuilder>,
}

impl State {
    /// Is the state currently at global (root) level?
    #[inline(always)]
    pub fn is_global(&self) -> bool {
        self.scope_level == 0
    }
}

/// _(INTERNALS)_ A type containing all the limits imposed by the [`Engine`].
/// Exported under the `internals` feature only.
///
/// ## WARNING
///
/// This type is volatile and may change.
#[cfg(not(feature = "unchecked"))]
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Limits {
    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Defaults to 16 for debug builds and 128 for non-debug builds.
    pub max_call_stack_depth: usize,
    /// Maximum depth of statements/expressions at global level (0 = unlimited).
    pub max_expr_depth: usize,
    /// Maximum depth of statements/expressions in functions (0 = unlimited).
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    pub max_function_expr_depth: usize,
    /// Maximum number of operations allowed to run (0 = unlimited).
    pub max_operations: u64,
    /// Maximum number of [modules][Module] allowed to load.
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    pub max_modules: usize,
    /// Maximum length of a [string][ImmutableString] (0 = unlimited).
    pub max_string_size: usize,
    /// Maximum length of an [array][Array] (0 = unlimited).
    /// Not available under `no_index`.
    #[cfg(not(feature = "no_index"))]
    pub max_array_size: usize,
    /// Maximum number of properties in an [object map][Map] (0 = unlimited).
    /// Not available under `no_object`.
    #[cfg(not(feature = "no_object"))]
    pub max_map_size: usize,
}

/// Context of a script evaluation process.
#[derive(Debug)]
pub struct EvalContext<'e, 'x, 'px: 'x, 'a, 's, 'm, 'pm: 'm, 't, 'pt: 't> {
    pub(crate) engine: &'e Engine,
    pub(crate) scope: &'x mut Scope<'px>,
    pub(crate) mods: &'a mut Imports,
    pub(crate) state: &'s mut State,
    pub(crate) lib: &'m [&'pm Module],
    pub(crate) this_ptr: &'t mut Option<&'pt mut Dynamic>,
    pub(crate) level: usize,
}

impl<'e, 'x, 'px, 'a, 's, 'm, 'pm, 't, 'pt> EvalContext<'e, 'x, 'px, 'a, 's, 'm, 'pm, 't, 'pt> {
    /// The current [`Engine`].
    #[inline(always)]
    pub fn engine(&self) -> &'e Engine {
        self.engine
    }
    /// The current source.
    #[inline(always)]
    pub fn source<'z: 's>(&'z self) -> Option<&'s str> {
        self.state.source.as_ref().map(|s| s.as_str())
    }
    /// The current [`Scope`].
    #[inline(always)]
    pub fn scope(&self) -> &Scope<'px> {
        self.scope
    }
    /// Mutable reference to the current [`Scope`].
    #[inline(always)]
    pub fn scope_mut(&mut self) -> &mut &'x mut Scope<'px> {
        &mut self.scope
    }
    /// _(INTERNALS)_ The current set of modules imported via `import` statements.
    /// Available under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn imports(&'a self) -> &'a Imports {
        self.mods
    }
    /// Get an iterator over the namespaces containing definition of all script-defined functions.
    #[inline(always)]
    pub fn iter_namespaces(&self) -> impl Iterator<Item = &'pm Module> + 'm {
        self.lib.iter().cloned()
    }
    /// The current bound `this` pointer, if any.
    #[inline(always)]
    pub fn this_ptr(&self) -> Option<&Dynamic> {
        self.this_ptr.as_ref().map(|v| &**v)
    }
    /// The current nesting level of function calls.
    #[inline(always)]
    pub fn call_level(&self) -> usize {
        self.level
    }
}

/// Rhai main scripting engine.
///
/// # Thread Safety
///
/// [`Engine`] is re-entrant.
///
/// Currently, [`Engine`] is neither [`Send`] nor [`Sync`].
/// Use the `sync` feature to make it [`Send`] `+` [`Sync`].
///
/// # Example
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
pub struct Engine {
    /// A unique ID identifying this scripting [`Engine`].
    pub id: String,

    /// A module containing all functions directly loaded into the Engine.
    pub(crate) global_namespace: Module,
    /// A collection of all modules loaded into the global namespace of the Engine.
    pub(crate) global_modules: StaticVec<Shared<Module>>,
    /// A collection of all sub-modules directly loaded into the Engine.
    pub(crate) global_sub_modules: HashMap<ImmutableString, Shared<Module>>,

    /// A module resolution service.
    #[cfg(not(feature = "no_module"))]
    pub(crate) module_resolver: Box<dyn crate::ModuleResolver>,

    /// A hashmap mapping type names to pretty-print names.
    pub(crate) type_names: HashMap<String, String>,

    /// A hashset containing symbols to disable.
    pub(crate) disabled_symbols: HashSet<String>,
    /// A hashmap containing custom keywords and precedence to recognize.
    pub(crate) custom_keywords: HashMap<String, Option<u8>>,
    /// Custom syntax.
    pub(crate) custom_syntax: HashMap<ImmutableString, CustomSyntax>,
    /// Callback closure for resolving variable access.
    pub(crate) resolve_var: Option<OnVarCallback>,

    /// Callback closure for implementing the `print` command.
    pub(crate) print: OnPrintCallback,
    /// Callback closure for implementing the `debug` command.
    pub(crate) debug: OnDebugCallback,
    /// Callback closure for progress reporting.
    pub(crate) progress: Option<OnProgressCallback>,

    /// Optimize the AST after compilation.
    pub(crate) optimization_level: OptimizationLevel,

    /// Max limits.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) limits: Limits,

    /// Disable doc-comments?
    pub(crate) disable_doc_comments: bool,
}

impl fmt::Debug for Engine {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.id.is_empty() {
            write!(f, "Engine({})", self.id)
        } else {
            f.write_str("Engine")
        }
    }
}

impl Default for Engine {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

/// Make getter function
#[cfg(not(feature = "no_object"))]
#[inline(always)]
pub fn make_getter(id: &str) -> String {
    format!("{}{}", FN_GET, id)
}

/// Make setter function
#[cfg(not(feature = "no_object"))]
#[inline(always)]
pub fn make_setter(id: &str) -> String {
    format!("{}{}", FN_SET, id)
}

/// Is this function an anonymous function?
#[cfg(not(feature = "no_function"))]
#[inline(always)]
pub fn is_anonymous_fn(fn_name: &str) -> bool {
    fn_name.starts_with(FN_ANONYMOUS)
}

/// Print to stdout
#[inline(always)]
fn default_print(_s: &str) {
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(target_arch = "wasm32"))]
    println!("{}", _s);
}

/// Debug to stdout
#[inline(always)]
fn default_debug(_s: &str, _source: Option<&str>, _pos: Position) {
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(target_arch = "wasm32"))]
    if let Some(source) = _source {
        println!("{} @ {:?} | {}", source, _pos, _s);
    } else {
        println!("{:?} | {}", _pos, _s);
    }
}

/// Search for a module within an imports stack.
/// [`Position`] in [`EvalAltResult`] is [`None`][Position::None] and must be set afterwards.
pub fn search_imports(
    mods: &Imports,
    state: &mut State,
    namespace: &NamespaceRef,
) -> Result<Shared<Module>, Box<EvalAltResult>> {
    let Ident { name: root, pos } = &namespace[0];

    // Qualified - check if the root module is directly indexed
    let index = if state.always_search {
        0
    } else {
        namespace.index().map_or(0, NonZeroUsize::get)
    };

    Ok(if index > 0 {
        let offset = mods.len() - index;
        mods.get(offset).expect("invalid index in Imports")
    } else {
        mods.find(root)
            .map(|n| mods.get(n).expect("invalid index in Imports"))
            .ok_or_else(|| EvalAltResult::ErrorModuleNotFound(root.to_string(), *pos))?
    })
}

impl Engine {
    /// Create a new [`Engine`]
    #[inline]
    pub fn new() -> Self {
        // Create the new scripting Engine
        let mut engine = Self {
            id: Default::default(),

            global_namespace: Default::default(),
            global_modules: Default::default(),
            global_sub_modules: Default::default(),

            #[cfg(not(feature = "no_module"))]
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(target_arch = "wasm32"))]
            module_resolver: Box::new(crate::module::resolvers::FileModuleResolver::new()),
            #[cfg(not(feature = "no_module"))]
            #[cfg(any(feature = "no_std", target_arch = "wasm32",))]
            module_resolver: None,

            type_names: Default::default(),
            disabled_symbols: Default::default(),
            custom_keywords: Default::default(),
            custom_syntax: Default::default(),

            // variable resolver
            resolve_var: None,

            // default print/debug implementations
            print: Box::new(default_print),
            debug: Box::new(default_debug),

            // progress callback
            progress: None,

            // optimization level
            optimization_level: if cfg!(feature = "no_optimize") {
                OptimizationLevel::None
            } else {
                OptimizationLevel::Simple
            },

            #[cfg(not(feature = "unchecked"))]
            limits: Limits {
                max_call_stack_depth: MAX_CALL_STACK_DEPTH,
                max_expr_depth: MAX_EXPR_DEPTH,
                #[cfg(not(feature = "no_function"))]
                max_function_expr_depth: MAX_FUNCTION_EXPR_DEPTH,
                max_operations: 0,
                #[cfg(not(feature = "no_module"))]
                max_modules: usize::MAX,
                max_string_size: 0,
                #[cfg(not(feature = "no_index"))]
                max_array_size: 0,
                #[cfg(not(feature = "no_object"))]
                max_map_size: 0,
            },

            disable_doc_comments: false,
        };

        engine.register_global_module(StandardPackage::new().as_shared_module());

        engine
    }

    /// Create a new [`Engine`] with minimal built-in functions.
    /// Use the [`register_global_module`][Engine::register_global_module] method to load additional packages of functions.
    #[inline]
    pub fn new_raw() -> Self {
        Self {
            id: Default::default(),

            global_namespace: Default::default(),
            global_modules: Default::default(),
            global_sub_modules: Default::default(),

            #[cfg(not(feature = "no_module"))]
            module_resolver: Box::new(crate::module::resolvers::DummyModuleResolver::new()),

            type_names: Default::default(),
            disabled_symbols: Default::default(),
            custom_keywords: Default::default(),
            custom_syntax: Default::default(),

            resolve_var: None,

            print: Box::new(|_| {}),
            debug: Box::new(|_, _, _| {}),
            progress: None,

            optimization_level: if cfg!(feature = "no_optimize") {
                OptimizationLevel::None
            } else {
                OptimizationLevel::Simple
            },

            #[cfg(not(feature = "unchecked"))]
            limits: Limits {
                max_call_stack_depth: MAX_CALL_STACK_DEPTH,
                max_expr_depth: MAX_EXPR_DEPTH,
                #[cfg(not(feature = "no_function"))]
                max_function_expr_depth: MAX_FUNCTION_EXPR_DEPTH,
                max_operations: 0,
                #[cfg(not(feature = "no_module"))]
                max_modules: usize::MAX,
                max_string_size: 0,
                #[cfg(not(feature = "no_index"))]
                max_array_size: 0,
                #[cfg(not(feature = "no_object"))]
                max_map_size: 0,
            },

            disable_doc_comments: false,
        }
    }

    /// Search for a variable within the scope or within imports,
    /// depending on whether the variable name is namespace-qualified.
    pub(crate) fn search_namespace<'s>(
        &self,
        scope: &'s mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &'s mut Option<&mut Dynamic>,
        expr: &Expr,
    ) -> Result<(Target<'s>, Position), Box<EvalAltResult>> {
        match expr {
            Expr::Variable(v) => match v.as_ref() {
                // Qualified variable
                (_, Some((hash_var, modules)), Ident { name, pos }) => {
                    let module = search_imports(mods, state, modules)?;
                    let target = module.get_qualified_var(*hash_var).map_err(|mut err| {
                        match *err {
                            EvalAltResult::ErrorVariableNotFound(ref mut err_name, _) => {
                                *err_name = format!("{}{}", modules, name);
                            }
                            _ => (),
                        }
                        err.fill_position(*pos)
                    })?;

                    // Module variables are constant
                    let mut target = target.clone();
                    target.set_access_mode(AccessMode::ReadOnly);
                    Ok((target.into(), *pos))
                }
                // Normal variable access
                _ => self.search_scope_only(scope, mods, state, lib, this_ptr, expr),
            },
            _ => unreachable!(),
        }
    }

    /// Search for a variable within the scope
    pub(crate) fn search_scope_only<'s>(
        &self,
        scope: &'s mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &'s mut Option<&mut Dynamic>,
        expr: &Expr,
    ) -> Result<(Target<'s>, Position), Box<EvalAltResult>> {
        let (index, _, Ident { name, pos }) = match expr {
            Expr::Variable(v) => v.as_ref(),
            _ => unreachable!(),
        };

        // Check if the variable is `this`
        if name.as_str() == KEYWORD_THIS {
            if let Some(val) = this_ptr {
                return Ok(((*val).into(), *pos));
            } else {
                return EvalAltResult::ErrorUnboundThis(*pos).into();
            }
        }

        // Check if it is directly indexed
        let index = if state.always_search {
            0
        } else {
            index.map_or(0, NonZeroUsize::get)
        };

        // Check the variable resolver, if any
        if let Some(ref resolve_var) = self.resolve_var {
            let context = EvalContext {
                engine: self,
                scope,
                mods,
                state,
                lib,
                this_ptr,
                level: 0,
            };
            if let Some(mut result) =
                resolve_var(name, index, &context).map_err(|err| err.fill_position(*pos))?
            {
                result.set_access_mode(AccessMode::ReadOnly);
                return Ok((result.into(), *pos));
            }
        }

        let index = if index > 0 {
            scope.len() - index
        } else {
            // Find the variable in the scope
            scope
                .get_index(name)
                .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(name.to_string(), *pos))?
                .0
        };

        let val = scope.get_mut_by_index(index);

        // Check for data race - probably not necessary because the only place it should conflict is in a method call
        //                       when the object variable is also used as a parameter.
        // if cfg!(not(feature = "no_closure")) && val.is_locked() {
        //     return EvalAltResult::ErrorDataRace(name.into(), *pos).into();
        // }

        Ok((val.into(), *pos))
    }

    /// Chain-evaluate a dot/index chain.
    /// [`Position`] in [`EvalAltResult`] is [`None`][Position::None] and must be set afterwards.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_dot_index_chain_helper(
        &self,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        target: &mut Target,
        rhs: &Expr,
        idx_values: &mut StaticVec<IndexChainValue>,
        chain_type: ChainType,
        level: usize,
        new_val: Option<(Dynamic, Position)>,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        if chain_type == ChainType::None {
            panic!();
        }

        let is_ref = target.is_ref();

        let next_chain = match rhs {
            Expr::Index(_, _) => ChainType::Index,
            Expr::Dot(_, _) => ChainType::Dot,
            _ => ChainType::None,
        };

        // Pop the last index value
        let idx_val = idx_values.pop().unwrap();

        let target_val = target.as_mut();

        match chain_type {
            #[cfg(not(feature = "no_index"))]
            ChainType::Index => {
                let pos = rhs.position();

                match rhs {
                    // xxx[idx].expr... | xxx[idx][expr]...
                    Expr::Dot(x, x_pos) | Expr::Index(x, x_pos) => {
                        let idx_pos = x.lhs.position();
                        let idx_val = idx_val.as_value();
                        let obj_ptr = &mut self.get_indexed_mut(
                            mods, state, lib, target_val, idx_val, idx_pos, false, is_ref, true,
                            level,
                        )?;

                        self.eval_dot_index_chain_helper(
                            mods, state, lib, this_ptr, obj_ptr, &x.rhs, idx_values, next_chain,
                            level, new_val,
                        )
                        .map_err(|err| err.fill_position(*x_pos))
                    }
                    // xxx[rhs] = new_val
                    _ if new_val.is_some() => {
                        let idx_val = idx_val.as_value();
                        let mut idx_val2 = idx_val.clone();

                        // `call_setter` is introduced to bypass double mutable borrowing of target
                        let _call_setter = match self.get_indexed_mut(
                            mods, state, lib, target_val, idx_val, pos, true, is_ref, false, level,
                        ) {
                            // Indexed value is a reference - update directly
                            Ok(ref mut obj_ptr) => {
                                let (new_val, new_val_pos) = new_val.unwrap();
                                obj_ptr.set_value(new_val, new_val_pos)?;
                                None
                            }
                            Err(err) => match *err {
                                // No index getter - try to call an index setter
                                #[cfg(not(feature = "no_index"))]
                                EvalAltResult::ErrorIndexingType(_, _) => Some(new_val.unwrap()),
                                // Any other error - return
                                err => return Err(Box::new(err)),
                            },
                        };

                        #[cfg(not(feature = "no_index"))]
                        if let Some(mut new_val) = _call_setter {
                            let val_type_name = target_val.type_name();
                            let args = &mut [target_val, &mut idx_val2, &mut new_val.0];

                            self.exec_fn_call(
                                mods, state, lib, FN_IDX_SET, None, args, is_ref, true, false,
                                new_val.1, None, None, level,
                            )
                            .map_err(|err| match *err {
                                EvalAltResult::ErrorFunctionNotFound(fn_sig, _)
                                    if fn_sig.ends_with("]=") =>
                                {
                                    EvalAltResult::ErrorIndexingType(
                                        self.map_type_name(val_type_name).into(),
                                        Position::NONE,
                                    )
                                }
                                err => err,
                            })?;
                        }

                        Ok((Dynamic::UNIT, true))
                    }
                    // xxx[rhs]
                    _ => {
                        let idx_val = idx_val.as_value();
                        self.get_indexed_mut(
                            mods, state, lib, target_val, idx_val, pos, false, is_ref, true, level,
                        )
                        .map(|v| (v.take_or_clone(), false))
                    }
                }
            }

            #[cfg(not(feature = "no_object"))]
            ChainType::Dot => {
                match rhs {
                    // xxx.fn_name(arg_expr_list)
                    Expr::FnCall(x, pos) if x.namespace.is_none() => {
                        let FnCallExpr {
                            name,
                            hash_script: hash,
                            def_value,
                            ..
                        } = x.as_ref();
                        let def_value = def_value.as_ref();
                        let args = idx_val.as_fn_call_args();
                        self.make_method_call(
                            mods, state, lib, name, *hash, target, args, def_value, false, *pos,
                            level,
                        )
                    }
                    // xxx.module::fn_name(...) - syntax error
                    Expr::FnCall(_, _) => unreachable!(),
                    // {xxx:map}.id = ???
                    Expr::Property(x) if target_val.is::<Map>() && new_val.is_some() => {
                        let Ident { name, pos } = &x.2;
                        let index = name.clone().into();
                        let mut val = self.get_indexed_mut(
                            mods, state, lib, target_val, index, *pos, true, is_ref, false, level,
                        )?;

                        let (new_val, new_val_pos) = new_val.unwrap();
                        val.set_value(new_val, new_val_pos)?;

                        Ok((Default::default(), true))
                    }
                    // {xxx:map}.id
                    Expr::Property(x) if target_val.is::<Map>() => {
                        let Ident { name, pos } = &x.2;
                        let index = name.clone().into();
                        let val = self.get_indexed_mut(
                            mods, state, lib, target_val, index, *pos, false, is_ref, false, level,
                        )?;

                        Ok((val.take_or_clone(), false))
                    }
                    // xxx.id = ???
                    Expr::Property(x) if new_val.is_some() => {
                        let (_, setter, Ident { pos, .. }) = x.as_ref();
                        let mut new_val = new_val;
                        let mut args = [target_val, &mut new_val.as_mut().unwrap().0];
                        self.exec_fn_call(
                            mods, state, lib, setter, None, &mut args, is_ref, true, false, *pos,
                            None, None, level,
                        )
                        .map(|(v, _)| (v, true))
                        .map_err(|err| err.fill_position(*pos))
                    }
                    // xxx.id
                    Expr::Property(x) => {
                        let (getter, _, Ident { pos, .. }) = x.as_ref();
                        let mut args = [target_val];
                        self.exec_fn_call(
                            mods, state, lib, getter, None, &mut args, is_ref, true, false, *pos,
                            None, None, level,
                        )
                        .map(|(v, _)| (v, false))
                        .map_err(|err| err.fill_position(*pos))
                    }
                    // {xxx:map}.sub_lhs[expr] | {xxx:map}.sub_lhs.expr
                    Expr::Index(x, x_pos) | Expr::Dot(x, x_pos) if target_val.is::<Map>() => {
                        let mut val = match &x.lhs {
                            Expr::Property(p) => {
                                let Ident { name, pos } = &p.2;
                                let index = name.clone().into();
                                self.get_indexed_mut(
                                    mods, state, lib, target_val, index, *pos, false, is_ref, true,
                                    level,
                                )?
                            }
                            // {xxx:map}.fn_name(arg_expr_list)[expr] | {xxx:map}.fn_name(arg_expr_list).expr
                            Expr::FnCall(x, pos) if x.namespace.is_none() => {
                                let FnCallExpr {
                                    name,
                                    hash_script: hash,
                                    def_value,
                                    ..
                                } = x.as_ref();
                                let def_value = def_value.as_ref();
                                let args = idx_val.as_fn_call_args();
                                let (val, _) = self.make_method_call(
                                    mods, state, lib, name, *hash, target, args, def_value, false,
                                    *pos, level,
                                )?;
                                val.into()
                            }
                            // {xxx:map}.module::fn_name(...) - syntax error
                            Expr::FnCall(_, _) => unreachable!(),
                            // Others - syntax error
                            _ => unreachable!(),
                        };

                        self.eval_dot_index_chain_helper(
                            mods, state, lib, this_ptr, &mut val, &x.rhs, idx_values, next_chain,
                            level, new_val,
                        )
                        .map_err(|err| err.fill_position(*x_pos))
                    }
                    // xxx.sub_lhs[expr] | xxx.sub_lhs.expr
                    Expr::Index(x, x_pos) | Expr::Dot(x, x_pos) => {
                        match &x.lhs {
                            // xxx.prop[expr] | xxx.prop.expr
                            Expr::Property(p) => {
                                let (getter, setter, Ident { pos, .. }) = p.as_ref();
                                let arg_values = &mut [target_val, &mut Default::default()];
                                let args = &mut arg_values[..1];
                                let (mut val, updated) = self
                                    .exec_fn_call(
                                        mods, state, lib, getter, None, args, is_ref, true, false,
                                        *pos, None, None, level,
                                    )
                                    .map_err(|err| err.fill_position(*pos))?;

                                let val = &mut val;

                                let (result, may_be_changed) = self
                                    .eval_dot_index_chain_helper(
                                        mods,
                                        state,
                                        lib,
                                        this_ptr,
                                        &mut val.into(),
                                        &x.rhs,
                                        idx_values,
                                        next_chain,
                                        level,
                                        new_val,
                                    )
                                    .map_err(|err| err.fill_position(*x_pos))?;

                                // Feed the value back via a setter just in case it has been updated
                                if updated || may_be_changed {
                                    // Re-use args because the first &mut parameter will not be consumed
                                    arg_values[1] = val;
                                    self.exec_fn_call(
                                        mods, state, lib, setter, None, arg_values, is_ref, true,
                                        false, *pos, None, None, level,
                                    )
                                    .or_else(
                                        |err| match *err {
                                            // If there is no setter, no need to feed it back because the property is read-only
                                            EvalAltResult::ErrorDotExpr(_, _) => {
                                                Ok((Dynamic::UNIT, false))
                                            }
                                            _ => Err(err.fill_position(*x_pos)),
                                        },
                                    )?;
                                }

                                Ok((result, may_be_changed))
                            }
                            // xxx.fn_name(arg_expr_list)[expr] | xxx.fn_name(arg_expr_list).expr
                            Expr::FnCall(f, pos) if f.namespace.is_none() => {
                                let FnCallExpr {
                                    name,
                                    hash_script: hash,
                                    def_value,
                                    ..
                                } = f.as_ref();
                                let def_value = def_value.as_ref();
                                let args = idx_val.as_fn_call_args();
                                let (mut val, _) = self.make_method_call(
                                    mods, state, lib, name, *hash, target, args, def_value, false,
                                    *pos, level,
                                )?;
                                let val = &mut val;
                                let target = &mut val.into();

                                self.eval_dot_index_chain_helper(
                                    mods, state, lib, this_ptr, target, &x.rhs, idx_values,
                                    next_chain, level, new_val,
                                )
                                .map_err(|err| err.fill_position(*pos))
                            }
                            // xxx.module::fn_name(...) - syntax error
                            Expr::FnCall(_, _) => unreachable!(),
                            // Others - syntax error
                            _ => unreachable!(),
                        }
                    }
                    // Syntax error
                    _ => EvalAltResult::ErrorDotExpr("".into(), rhs.position()).into(),
                }
            }

            _ => unreachable!(),
        }
    }

    /// Evaluate a dot/index chain.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_dot_index_chain(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
        new_val: Option<(Dynamic, Position)>,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let (crate::ast::BinaryExpr { lhs, rhs }, chain_type, op_pos) = match expr {
            Expr::Index(x, pos) => (x.as_ref(), ChainType::Index, *pos),
            Expr::Dot(x, pos) => (x.as_ref(), ChainType::Dot, *pos),
            _ => unreachable!(),
        };

        let idx_values = &mut Default::default();

        self.eval_indexed_chain(
            scope, mods, state, lib, this_ptr, rhs, chain_type, idx_values, 0, level,
        )?;

        match lhs {
            // id.??? or id[???]
            Expr::Variable(x) => {
                let Ident {
                    name: var_name,
                    pos: var_pos,
                } = &x.2;

                self.inc_operations(state, *var_pos)?;

                let (target, pos) =
                    self.search_namespace(scope, mods, state, lib, this_ptr, lhs)?;

                // Constants cannot be modified
                if target.as_ref().is_read_only() && new_val.is_some() {
                    return EvalAltResult::ErrorAssignmentToConstant(var_name.to_string(), pos)
                        .into();
                }

                let obj_ptr = &mut target.into();
                self.eval_dot_index_chain_helper(
                    mods, state, lib, &mut None, obj_ptr, rhs, idx_values, chain_type, level,
                    new_val,
                )
                .map(|(v, _)| v)
                .map_err(|err| err.fill_position(op_pos))
            }
            // {expr}.??? = ??? or {expr}[???] = ???
            _ if new_val.is_some() => unreachable!(),
            // {expr}.??? or {expr}[???]
            expr => {
                let val = self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
                let obj_ptr = &mut val.into();
                self.eval_dot_index_chain_helper(
                    mods, state, lib, this_ptr, obj_ptr, rhs, idx_values, chain_type, level,
                    new_val,
                )
                .map(|(v, _)| v)
                .map_err(|err| err.fill_position(op_pos))
            }
        }
    }

    /// Evaluate a chain of indexes and store the results in a [`StaticVec`].
    /// [`StaticVec`] is used to avoid an allocation in the overwhelming cases of just a few levels of indexing.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_indexed_chain(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        chain_type: ChainType,
        idx_values: &mut StaticVec<IndexChainValue>,
        size: usize,
        level: usize,
    ) -> Result<(), Box<EvalAltResult>> {
        self.inc_operations(state, expr.position())?;

        match expr {
            Expr::FnCall(x, _) if x.namespace.is_none() => {
                let arg_values = x
                    .args
                    .iter()
                    .map(|arg_expr| {
                        self.eval_expr(scope, mods, state, lib, this_ptr, arg_expr, level)
                    })
                    .collect::<Result<StaticVec<_>, _>>()?;

                idx_values.push(arg_values.into());
            }
            Expr::FnCall(_, _) => unreachable!(),
            Expr::Property(_) => idx_values.push(IndexChainValue::None),
            Expr::Index(x, _) | Expr::Dot(x, _) => {
                let crate::ast::BinaryExpr { lhs, rhs, .. } = x.as_ref();

                // Evaluate in left-to-right order
                let lhs_val = match lhs {
                    Expr::Property(_) => IndexChainValue::None,
                    Expr::FnCall(x, _) if chain_type == ChainType::Dot && x.namespace.is_none() => {
                        x.args
                            .iter()
                            .map(|arg_expr| {
                                self.eval_expr(scope, mods, state, lib, this_ptr, arg_expr, level)
                            })
                            .collect::<Result<StaticVec<Dynamic>, _>>()?
                            .into()
                    }
                    Expr::FnCall(_, _) => unreachable!(),
                    _ => self
                        .eval_expr(scope, mods, state, lib, this_ptr, lhs, level)?
                        .into(),
                };

                // Push in reverse order
                let chain_type = match expr {
                    Expr::Index(_, _) => ChainType::Index,
                    Expr::Dot(_, _) => ChainType::Dot,
                    _ => unreachable!(),
                };
                self.eval_indexed_chain(
                    scope, mods, state, lib, this_ptr, rhs, chain_type, idx_values, size, level,
                )?;

                idx_values.push(lhs_val);
            }
            _ => idx_values.push(
                self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .into(),
            ),
        }

        Ok(())
    }

    /// Get the value at the indexed position of a base type.
    /// [`Position`] in [`EvalAltResult`] may be None and should be set afterwards.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn get_indexed_mut<'t>(
        &self,
        _mods: &mut Imports,
        state: &mut State,
        _lib: &[&Module],
        target: &'t mut Dynamic,
        idx: Dynamic,
        idx_pos: Position,
        _create: bool,
        _is_ref: bool,
        _indexers: bool,
        _level: usize,
    ) -> Result<Target<'t>, Box<EvalAltResult>> {
        self.inc_operations(state, Position::NONE)?;

        match target {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(arr, _)) => {
                // val_array[idx]
                let index = idx
                    .as_int()
                    .map_err(|err| self.make_type_mismatch_err::<crate::INT>(err, idx_pos))?;

                let arr_len = arr.len();

                if index >= 0 {
                    arr.get_mut(index as usize)
                        .map(Target::from)
                        .ok_or_else(|| {
                            EvalAltResult::ErrorArrayBounds(arr_len, index, idx_pos).into()
                        })
                } else {
                    EvalAltResult::ErrorArrayBounds(arr_len, index, idx_pos).into()
                }
            }

            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(map, _)) => {
                // val_map[idx]
                Ok(if _create {
                    let index = idx.take_immutable_string().map_err(|err| {
                        self.make_type_mismatch_err::<ImmutableString>(err, idx_pos)
                    })?;

                    map.entry(index).or_insert_with(Default::default).into()
                } else {
                    let index = idx.read_lock::<ImmutableString>().ok_or_else(|| {
                        self.make_type_mismatch_err::<ImmutableString>("", idx_pos)
                    })?;

                    map.get_mut(&*index)
                        .map(Target::from)
                        .unwrap_or_else(|| Target::from(()))
                })
            }

            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Str(s, _)) => {
                // val_string[idx]
                let chars_len = s.chars().count();
                let index = idx
                    .as_int()
                    .map_err(|err| self.make_type_mismatch_err::<crate::INT>(err, idx_pos))?;

                if index >= 0 {
                    let offset = index as usize;
                    let ch = s.chars().nth(offset).ok_or_else(|| {
                        EvalAltResult::ErrorStringBounds(chars_len, index, idx_pos)
                    })?;
                    Ok(Target::StringChar(target, offset, ch.into()))
                } else {
                    EvalAltResult::ErrorStringBounds(chars_len, index, idx_pos).into()
                }
            }

            #[cfg(not(feature = "no_index"))]
            _ if _indexers => {
                let type_name = target.type_name();
                let mut idx = idx;
                let args = &mut [target, &mut idx];
                self.exec_fn_call(
                    _mods, state, _lib, FN_IDX_GET, None, args, _is_ref, true, false, idx_pos,
                    None, None, _level,
                )
                .map(|(v, _)| v.into())
                .map_err(|err| match *err {
                    EvalAltResult::ErrorFunctionNotFound(fn_sig, _) if fn_sig.ends_with(']') => {
                        Box::new(EvalAltResult::ErrorIndexingType(
                            type_name.into(),
                            Position::NONE,
                        ))
                    }
                    _ => err,
                })
            }

            _ => EvalAltResult::ErrorIndexingType(
                self.map_type_name(target.type_name()).into(),
                Position::NONE,
            )
            .into(),
        }
    }

    // Evaluate an 'in' expression.
    fn eval_in_expr(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        lhs: &Expr,
        rhs: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, rhs.position())?;

        let lhs_value = self.eval_expr(scope, mods, state, lib, this_ptr, lhs, level)?;
        let rhs_value = self.eval_expr(scope, mods, state, lib, this_ptr, rhs, level)?;

        match rhs_value {
            #[cfg(not(feature = "no_index"))]
            Dynamic(Union::Array(mut rhs_value, _)) => {
                // Call the `==` operator to compare each value
                let def_value = Some(false.into());
                let def_value = def_value.as_ref();

                for value in rhs_value.iter_mut() {
                    let args = &mut [&mut lhs_value.clone(), value];

                    // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
                    let hash =
                        calc_native_fn_hash(empty(), OP_EQUALS, args.iter().map(|a| a.type_id()))
                            .unwrap();

                    let pos = rhs.position();

                    if self
                        .call_native_fn(
                            mods, state, lib, OP_EQUALS, hash, args, false, false, pos, def_value,
                        )?
                        .0
                        .as_bool()
                        .unwrap_or(false)
                    {
                        return Ok(true.into());
                    }
                }

                Ok(false.into())
            }
            #[cfg(not(feature = "no_object"))]
            Dynamic(Union::Map(rhs_value, _)) => match lhs_value {
                // Only allows string or char
                Dynamic(Union::Str(s, _)) => Ok(rhs_value.contains_key(&s).into()),
                Dynamic(Union::Char(c, _)) => Ok(rhs_value.contains_key(&c.to_string()).into()),
                _ => EvalAltResult::ErrorInExpr(lhs.position()).into(),
            },
            Dynamic(Union::Str(rhs_value, _)) => match lhs_value {
                // Only allows string or char
                Dynamic(Union::Str(s, _)) => Ok(rhs_value.contains(s.as_str()).into()),
                Dynamic(Union::Char(c, _)) => Ok(rhs_value.contains(c).into()),
                _ => EvalAltResult::ErrorInExpr(lhs.position()).into(),
            },
            _ => EvalAltResult::ErrorInExpr(rhs.position()).into(),
        }
    }

    /// Get a [`Target`] from an expression.
    pub(crate) fn eval_expr_as_target<'s>(
        &self,
        scope: &'s mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &'s mut Option<&mut Dynamic>,
        expr: &Expr,
        _no_const: bool,
        level: usize,
    ) -> Result<(Target<'s>, Position), Box<EvalAltResult>> {
        match expr {
            // var - point directly to the value
            Expr::Variable(_) => {
                let (mut target, pos) =
                    self.search_namespace(scope, mods, state, lib, this_ptr, expr)?;

                // If necessary, constants are cloned
                if target.as_ref().is_read_only() {
                    target = target.into_owned();
                }

                Ok((target, pos))
            }
            // var[...]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(x, _) if x.lhs.get_variable_access(false).is_some() => match x.rhs {
                Expr::Property(_) => unreachable!(),
                // var[...]...
                Expr::FnCall(_, _) | Expr::Index(_, _) | Expr::Dot(_, _) => self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                    .map(|v| (v.into(), expr.position())),
                // var[expr] - point directly to the item
                _ => {
                    let idx = self.eval_expr(scope, mods, state, lib, this_ptr, &x.rhs, level)?;
                    let idx_pos = x.rhs.position();
                    let (mut target, pos) = self.eval_expr_as_target(
                        scope, mods, state, lib, this_ptr, &x.lhs, _no_const, level,
                    )?;

                    let is_ref = target.is_ref();

                    if target.is_shared() || target.is_value() {
                        let target_ref = target.as_mut();
                        self.get_indexed_mut(
                            mods, state, lib, target_ref, idx, idx_pos, false, is_ref, true, level,
                        )
                        .map(Target::into_owned)
                    } else {
                        let target_ref = target.take_ref().unwrap();
                        self.get_indexed_mut(
                            mods, state, lib, target_ref, idx, idx_pos, false, is_ref, true, level,
                        )
                    }
                    .map(|v| (v, pos))
                }
            },
            // var.prop
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(x, _) if x.lhs.get_variable_access(false).is_some() => match x.rhs {
                Expr::Variable(_) => unreachable!(),
                // var.prop
                Expr::Property(ref p) => {
                    let (mut target, _) = self.eval_expr_as_target(
                        scope, mods, state, lib, this_ptr, &x.lhs, _no_const, level,
                    )?;
                    let is_ref = target.is_ref();

                    if target.is::<Map>() {
                        // map.prop - point directly to the item
                        let (_, _, Ident { name, pos }) = p.as_ref();
                        let idx = name.clone().into();

                        if target.is_shared() || target.is_value() {
                            let target_ref = target.as_mut();
                            self.get_indexed_mut(
                                mods, state, lib, target_ref, idx, *pos, false, is_ref, true, level,
                            )
                            .map(Target::into_owned)
                        } else {
                            let target_ref = target.take_ref().unwrap();
                            self.get_indexed_mut(
                                mods, state, lib, target_ref, idx, *pos, false, is_ref, true, level,
                            )
                        }
                        .map(|v| (v, *pos))
                    } else {
                        // var.prop - call property getter
                        let (getter, _, Ident { pos, .. }) = p.as_ref();
                        let mut args = [target.as_mut()];
                        self.exec_fn_call(
                            mods, state, lib, getter, None, &mut args, is_ref, true, false, *pos,
                            None, None, level,
                        )
                        .map(|(v, _)| (v.into(), *pos))
                    }
                }
                // var.???
                _ => self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                    .map(|v| (v.into(), expr.position())),
            },
            // expr
            _ => self
                .eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                .map(|v| (v.into(), expr.position())),
        }
    }

    /// Evaluate an expression.
    pub(crate) fn eval_expr(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, expr.position())?;

        let result = match expr {
            Expr::Expr(x) => self.eval_expr(scope, mods, state, lib, this_ptr, x, level),

            Expr::DynamicConstant(x, _) => Ok(x.as_ref().clone()),
            Expr::IntegerConstant(x, _) => Ok((*x).into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(x, _) => Ok((*x).into()),
            Expr::StringConstant(x, _) => Ok(x.clone().into()),
            Expr::CharConstant(x, _) => Ok((*x).into()),
            Expr::FnPointer(x, _) => Ok(FnPtr::new_unchecked(x.clone(), Default::default()).into()),
            Expr::Variable(x) if (x.2).name == KEYWORD_THIS => {
                if let Some(val) = this_ptr {
                    Ok(val.clone())
                } else {
                    EvalAltResult::ErrorUnboundThis((x.2).pos).into()
                }
            }
            Expr::Variable(_) => {
                let (val, _) = self.search_namespace(scope, mods, state, lib, this_ptr, expr)?;
                Ok(val.take_or_clone())
            }
            Expr::Property(_) => unreachable!(),

            // Statement block
            Expr::Stmt(x, _) => {
                self.eval_stmt_block(scope, mods, state, lib, this_ptr, x.as_ref(), level)
            }

            // lhs[idx_expr]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(_, _) => {
                self.eval_dot_index_chain(scope, mods, state, lib, this_ptr, expr, level, None)
            }

            // lhs.dot_rhs
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(_, _) => {
                self.eval_dot_index_chain(scope, mods, state, lib, this_ptr, expr, level, None)
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Array(x, _) => {
                let mut arr =
                    Array::with_capacity(crate::stdlib::cmp::max(TYPICAL_ARRAY_SIZE, x.len()));
                for item in x.as_ref() {
                    arr.push(self.eval_expr(scope, mods, state, lib, this_ptr, item, level)?);
                }
                Ok(Dynamic(Union::Array(Box::new(arr), AccessMode::ReadWrite)))
            }

            #[cfg(not(feature = "no_object"))]
            Expr::Map(x, _) => {
                let mut map =
                    Map::with_capacity(crate::stdlib::cmp::max(TYPICAL_MAP_SIZE, x.len()));
                for (Ident { name: key, .. }, expr) in x.as_ref() {
                    map.insert(
                        key.clone(),
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?,
                    );
                }
                Ok(Dynamic(Union::Map(Box::new(map), AccessMode::ReadWrite)))
            }

            // Normal function call
            Expr::FnCall(x, pos) if x.namespace.is_none() => {
                let FnCallExpr {
                    name,
                    capture: cap_scope,
                    hash_script: hash,
                    args,
                    def_value,
                    ..
                } = x.as_ref();
                let def_value = def_value.as_ref();
                self.make_function_call(
                    scope, mods, state, lib, this_ptr, name, args, def_value, *hash, false, *pos,
                    *cap_scope, level,
                )
            }

            // Namespace-qualified function call
            Expr::FnCall(x, pos) if x.namespace.is_some() => {
                let FnCallExpr {
                    name,
                    namespace,
                    hash_script: hash,
                    args,
                    def_value,
                    ..
                } = x.as_ref();
                let namespace = namespace.as_ref();
                let hash = hash.unwrap();
                let def_value = def_value.as_ref();
                self.make_qualified_function_call(
                    scope, mods, state, lib, this_ptr, namespace, name, args, def_value, hash,
                    *pos, level,
                )
            }

            Expr::In(x, _) => {
                self.eval_in_expr(scope, mods, state, lib, this_ptr, &x.lhs, &x.rhs, level)
            }

            Expr::And(x, _) => {
                Ok((self
                    .eval_expr(scope, mods, state, lib, this_ptr, &x.lhs, level)?
                    .as_bool()
                    .map_err(|err| self.make_type_mismatch_err::<bool>(err, x.lhs.position()))?
                    && // Short-circuit using &&
                self
                    .eval_expr(scope, mods, state, lib, this_ptr, &x.rhs, level)?
                    .as_bool()
                    .map_err(|err| self.make_type_mismatch_err::<bool>(err, x.rhs.position()))?)
                .into())
            }

            Expr::Or(x, _) => {
                Ok((self
                    .eval_expr(scope, mods, state, lib, this_ptr, &x.lhs, level)?
                    .as_bool()
                    .map_err(|err| self.make_type_mismatch_err::<bool>(err, x.lhs.position()))?
                    || // Short-circuit using ||
                self
                    .eval_expr(scope, mods, state, lib, this_ptr, &x.rhs, level)?
                    .as_bool()
                    .map_err(|err| self.make_type_mismatch_err::<bool>(err, x.rhs.position()))?)
                .into())
            }

            Expr::BoolConstant(x, _) => Ok((*x).into()),
            Expr::Unit(_) => Ok(Dynamic::UNIT),

            Expr::Custom(custom, _) => {
                let expressions = custom
                    .keywords
                    .iter()
                    .map(Into::into)
                    .collect::<StaticVec<_>>();
                let mut context = EvalContext {
                    engine: self,
                    scope,
                    mods,
                    state,
                    lib,
                    this_ptr,
                    level,
                };
                (custom.func)(&mut context, &expressions)
            }

            _ => unreachable!(),
        };

        self.check_data_size(result, expr.position())
    }

    /// Evaluate a statements block.
    pub(crate) fn eval_stmt_block<'a>(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        statements: impl IntoIterator<Item = &'a Stmt>,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let prev_always_search = state.always_search;
        let prev_scope_len = scope.len();
        let prev_mods_len = mods.len();
        state.scope_level += 1;

        let result = statements
            .into_iter()
            .try_fold(Default::default(), |_, stmt| {
                self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level)
            });

        scope.rewind(prev_scope_len);
        if mods.len() != prev_mods_len {
            // If imports list is modified, clear the functions lookup cache
            state.functions_cache.clear();
        }
        mods.truncate(prev_mods_len);
        state.scope_level -= 1;

        // The impact of new local variables goes away at the end of a block
        // because any new variables introduced will go out of scope
        state.always_search = prev_always_search;

        result
    }

    /// Evaluate a statement.
    ///
    /// # Safety
    ///
    /// This method uses some unsafe code, mainly for avoiding cloning of local variable names via
    /// direct lifetime casting.
    pub(crate) fn eval_stmt(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        stmt: &Stmt,
        level: usize,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.inc_operations(state, stmt.position())?;

        let result = match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Dynamic::UNIT),

            // Expression as statement
            Stmt::Expr(expr) => self.eval_expr(scope, mods, state, lib, this_ptr, expr, level),

            // var op= rhs
            Stmt::Assignment(x, op_pos) if x.0.get_variable_access(false).is_some() => {
                let (lhs_expr, op, rhs_expr) = x.as_ref();
                let mut rhs_val = self
                    .eval_expr(scope, mods, state, lib, this_ptr, rhs_expr, level)?
                    .flatten();
                let (mut lhs_ptr, pos) =
                    self.search_namespace(scope, mods, state, lib, this_ptr, lhs_expr)?;

                if !lhs_ptr.is_ref() {
                    return EvalAltResult::ErrorAssignmentToConstant(
                        lhs_expr.get_variable_access(false).unwrap().to_string(),
                        pos,
                    )
                    .into();
                }

                self.inc_operations(state, pos)?;

                if lhs_ptr.as_ref().is_read_only() {
                    // Assignment to constant variable
                    Err(Box::new(EvalAltResult::ErrorAssignmentToConstant(
                        lhs_expr.get_variable_access(false).unwrap().to_string(),
                        pos,
                    )))
                } else if op.is_empty() {
                    // Normal assignment
                    if cfg!(not(feature = "no_closure")) && lhs_ptr.is_shared() {
                        *lhs_ptr.as_mut().write_lock::<Dynamic>().unwrap() = rhs_val;
                    } else {
                        *lhs_ptr.as_mut() = rhs_val;
                    }
                    Ok(Dynamic::UNIT)
                } else {
                    // Op-assignment - in order of precedence:
                    // 1) Native registered overriding function
                    // 2) Built-in implementation
                    // 3) Map to `var = var op rhs`

                    // Qualifiers (none) + function name + number of arguments + argument `TypeId`'s.
                    let arg_types = once(lhs_ptr.as_mut().type_id()).chain(once(rhs_val.type_id()));
                    let hash_fn = calc_native_fn_hash(empty(), op, arg_types).unwrap();

                    match self
                        .global_namespace
                        .get_fn(hash_fn, false)
                        .or_else(|| {
                            self.global_modules
                                .iter()
                                .find_map(|m| m.get_fn(hash_fn, false))
                        })
                        .or_else(|| mods.get_fn(hash_fn))
                    {
                        // op= function registered as method
                        Some(func) if func.is_method() => {
                            let mut lock_guard;
                            let lhs_ptr_inner;

                            if cfg!(not(feature = "no_closure")) && lhs_ptr.is_shared() {
                                lock_guard = lhs_ptr.as_mut().write_lock::<Dynamic>().unwrap();
                                lhs_ptr_inner = lock_guard.deref_mut();
                            } else {
                                lhs_ptr_inner = lhs_ptr.as_mut();
                            }

                            let args = &mut [lhs_ptr_inner, &mut rhs_val];

                            // Overriding exact implementation
                            if func.is_plugin_fn() {
                                func.get_plugin_fn()
                                    .call((self, &state.source, &*mods, lib).into(), args)?;
                            } else {
                                func.get_native_fn()(
                                    (self, &state.source, &*mods, lib).into(),
                                    args,
                                )?;
                            }
                        }
                        // Built-in op-assignment function
                        _ if run_builtin_op_assignment(op, lhs_ptr.as_mut(), &rhs_val)?
                            .is_some() => {}
                        // Not built-in: expand to `var = var op rhs`
                        _ => {
                            let op = &op[..op.len() - 1]; // extract operator without =

                            // Clone the LHS value
                            let args = &mut [&mut lhs_ptr.as_mut().clone(), &mut rhs_val];

                            // Run function
                            let (value, _) = self.exec_fn_call(
                                mods, state, lib, op, None, args, false, false, false, *op_pos,
                                None, None, level,
                            )?;

                            let value = value.flatten();

                            if cfg!(not(feature = "no_closure")) && lhs_ptr.is_shared() {
                                *lhs_ptr.as_mut().write_lock::<Dynamic>().unwrap() = value;
                            } else {
                                *lhs_ptr.as_mut() = value;
                            }
                        }
                    }
                    Ok(Dynamic::UNIT)
                }
            }

            // lhs op= rhs
            Stmt::Assignment(x, op_pos) => {
                let (lhs_expr, op, rhs_expr) = x.as_ref();
                let mut rhs_val =
                    self.eval_expr(scope, mods, state, lib, this_ptr, rhs_expr, level)?;

                let _new_val = if op.is_empty() {
                    // Normal assignment
                    Some((rhs_val, rhs_expr.position()))
                } else {
                    // Op-assignment - always map to `lhs = lhs op rhs`
                    let op = &op[..op.len() - 1]; // extract operator without =
                    let args = &mut [
                        &mut self.eval_expr(scope, mods, state, lib, this_ptr, lhs_expr, level)?,
                        &mut rhs_val,
                    ];

                    let result = self
                        .exec_fn_call(
                            mods, state, lib, op, None, args, false, false, false, *op_pos, None,
                            None, level,
                        )
                        .map(|(v, _)| v)?;

                    Some((result, rhs_expr.position()))
                };

                // Must be either `var[index] op= val` or `var.prop op= val`
                match lhs_expr {
                    // name op= rhs (handled above)
                    Expr::Variable(_) => unreachable!(),
                    // idx_lhs[idx_expr] op= rhs
                    #[cfg(not(feature = "no_index"))]
                    Expr::Index(_, _) => {
                        self.eval_dot_index_chain(
                            scope, mods, state, lib, this_ptr, lhs_expr, level, _new_val,
                        )?;
                        Ok(Dynamic::UNIT)
                    }
                    // dot_lhs.dot_rhs op= rhs
                    #[cfg(not(feature = "no_object"))]
                    Expr::Dot(_, _) => {
                        self.eval_dot_index_chain(
                            scope, mods, state, lib, this_ptr, lhs_expr, level, _new_val,
                        )?;
                        Ok(Dynamic::UNIT)
                    }
                    // Non-lvalue expression (should be caught during parsing)
                    _ => unreachable!(),
                }
            }

            // Block scope
            Stmt::Block(statements, _) => {
                self.eval_stmt_block(scope, mods, state, lib, this_ptr, statements, level)
            }

            // If statement
            Stmt::If(expr, x, _) => {
                let (if_block, else_block) = x.as_ref();
                self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .as_bool()
                    .map_err(|err| self.make_type_mismatch_err::<bool>(err, expr.position()))
                    .and_then(|guard_val| {
                        if guard_val {
                            self.eval_stmt(scope, mods, state, lib, this_ptr, if_block, level)
                        } else if let Some(stmt) = else_block {
                            self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level)
                        } else {
                            Ok(Dynamic::UNIT)
                        }
                    })
            }

            // Switch statement
            Stmt::Switch(match_expr, x, _) => {
                let (table, def_stmt) = x.as_ref();

                let hasher = &mut get_hasher();
                self.eval_expr_as_target(
                    scope, mods, state, lib, this_ptr, match_expr, false, level,
                )?
                .0
                .as_ref()
                .hash(hasher);
                let hash = hasher.finish();

                if let Some(stmt) = table.get(&hash) {
                    self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level)
                } else if let Some(def_stmt) = def_stmt {
                    self.eval_stmt(scope, mods, state, lib, this_ptr, def_stmt, level)
                } else {
                    Ok(Dynamic::UNIT)
                }
            }

            // While loop
            Stmt::While(expr, body, _) => loop {
                match self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .as_bool()
                {
                    Ok(true) => {
                        match self.eval_stmt(scope, mods, state, lib, this_ptr, body, level) {
                            Ok(_) => (),
                            Err(err) => match *err {
                                EvalAltResult::LoopBreak(false, _) => (),
                                EvalAltResult::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                                _ => return Err(err),
                            },
                        }
                    }
                    Ok(false) => return Ok(Dynamic::UNIT),
                    Err(err) => {
                        return Err(self.make_type_mismatch_err::<bool>(err, expr.position()))
                    }
                }
            },

            // Do loop
            Stmt::Do(body, expr, is_while, _) => loop {
                match self.eval_stmt(scope, mods, state, lib, this_ptr, body, level) {
                    Ok(_) => (),
                    Err(err) => match *err {
                        EvalAltResult::LoopBreak(false, _) => continue,
                        EvalAltResult::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                        _ => return Err(err),
                    },
                }

                match self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .as_bool()
                {
                    Ok(true) if !*is_while => return Ok(Dynamic::UNIT),
                    Ok(false) if *is_while => return Ok(Dynamic::UNIT),
                    Ok(_) => (),
                    Err(err) => {
                        return Err(self.make_type_mismatch_err::<bool>(err, expr.position()))
                    }
                }
            },

            // For loop
            Stmt::For(expr, x, _) => {
                let (name, stmt) = x.as_ref();
                let iter_obj = self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
                let iter_type = iter_obj.type_id();

                let func = self
                    .global_namespace
                    .get_iter(iter_type)
                    .or_else(|| {
                        self.global_modules
                            .iter()
                            .find_map(|m| m.get_iter(iter_type))
                    })
                    .or_else(|| mods.get_iter(iter_type));

                if let Some(func) = func {
                    // Add the loop variable
                    let var_name: Cow<'_, str> = if state.is_global() {
                        name.clone().into()
                    } else {
                        unsafe_cast_var_name_to_lifetime(name).into()
                    };
                    scope.push(var_name, ());
                    let index = scope.len() - 1;
                    state.scope_level += 1;

                    for iter_value in func(iter_obj) {
                        let loop_var = scope.get_mut_by_index(index);

                        let value = iter_value.flatten();
                        if cfg!(not(feature = "no_closure")) && loop_var.is_shared() {
                            *loop_var.write_lock().unwrap() = value;
                        } else {
                            *loop_var = value;
                        }

                        self.inc_operations(state, stmt.position())?;

                        match self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level) {
                            Ok(_) => (),
                            Err(err) => match *err {
                                EvalAltResult::LoopBreak(false, _) => (),
                                EvalAltResult::LoopBreak(true, _) => break,
                                _ => return Err(err),
                            },
                        }
                    }

                    state.scope_level -= 1;
                    scope.rewind(scope.len() - 1);
                    Ok(Dynamic::UNIT)
                } else {
                    EvalAltResult::ErrorFor(expr.position()).into()
                }
            }

            // Continue statement
            Stmt::Continue(pos) => EvalAltResult::LoopBreak(false, *pos).into(),

            // Break statement
            Stmt::Break(pos) => EvalAltResult::LoopBreak(true, *pos).into(),

            // Try/Catch statement
            Stmt::TryCatch(x, _, _) => {
                let (try_body, var_def, catch_body) = x.as_ref();

                let result = self
                    .eval_stmt(scope, mods, state, lib, this_ptr, try_body, level)
                    .map(|_| ().into());

                match result {
                    Ok(_) => result,
                    Err(err) => match *err {
                        mut err @ EvalAltResult::ErrorRuntime(_, _) | mut err
                            if err.is_catchable() =>
                        {
                            let value = if let EvalAltResult::ErrorRuntime(ref x, _) = err {
                                x.clone()
                            } else {
                                err.set_position(Position::NONE);
                                err.to_string().into()
                            };

                            let orig_scope_len = scope.len();
                            state.scope_level += 1;

                            if let Some(Ident { name, .. }) = var_def {
                                let var_name: Cow<'_, str> = if state.is_global() {
                                    name.to_string().into()
                                } else {
                                    unsafe_cast_var_name_to_lifetime(&name).into()
                                };
                                scope.push(var_name, value);
                            }

                            let mut result = self
                                .eval_stmt(scope, mods, state, lib, this_ptr, catch_body, level)
                                .map(|_| ().into());

                            if let Some(result_err) = result.as_ref().err() {
                                if let EvalAltResult::ErrorRuntime(
                                    Dynamic(Union::Unit(_, _)),
                                    pos,
                                ) = result_err.as_ref()
                                {
                                    err.set_position(*pos);
                                    result = Err(Box::new(err));
                                }
                            }

                            state.scope_level -= 1;
                            scope.rewind(orig_scope_len);

                            result
                        }
                        _ => Err(err),
                    },
                }
            }

            // Return value
            Stmt::Return((ReturnType::Return, pos), Some(expr), _) => EvalAltResult::Return(
                self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?,
                *pos,
            )
            .into(),

            // Empty return
            Stmt::Return((ReturnType::Return, pos), None, _) => {
                EvalAltResult::Return(Default::default(), *pos).into()
            }

            // Throw value
            Stmt::Return((ReturnType::Exception, pos), Some(expr), _) => {
                let val = self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
                EvalAltResult::ErrorRuntime(val, *pos).into()
            }

            // Empty throw
            Stmt::Return((ReturnType::Exception, pos), None, _) => {
                EvalAltResult::ErrorRuntime(().into(), *pos).into()
            }

            // Let/const statement
            Stmt::Let(var_def, expr, export, _) | Stmt::Const(var_def, expr, export, _) => {
                let entry_type = match stmt {
                    Stmt::Let(_, _, _, _) => AccessMode::ReadWrite,
                    Stmt::Const(_, _, _, _) => AccessMode::ReadOnly,
                    _ => unreachable!(),
                };

                let val = if let Some(expr) = expr {
                    self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                        .flatten()
                } else {
                    ().into()
                };
                let (var_name, _alias): (Cow<'_, str>, _) = if state.is_global() {
                    (
                        var_def.name.to_string().into(),
                        if *export {
                            Some(var_def.name.clone())
                        } else {
                            None
                        },
                    )
                } else if *export {
                    unreachable!();
                } else {
                    (unsafe_cast_var_name_to_lifetime(&var_def.name).into(), None)
                };
                scope.push_dynamic_value(var_name, entry_type, val);

                #[cfg(not(feature = "no_module"))]
                if let Some(alias) = _alias {
                    scope.add_entry_alias(scope.len() - 1, alias);
                }
                Ok(Dynamic::UNIT)
            }

            // Import statement
            #[cfg(not(feature = "no_module"))]
            Stmt::Import(expr, alias, _pos) => {
                // Guard against too many modules
                #[cfg(not(feature = "unchecked"))]
                if state.modules >= self.max_modules() {
                    return EvalAltResult::ErrorTooManyModules(*_pos).into();
                }

                if let Some(path) = self
                    .eval_expr(scope, mods, state, lib, this_ptr, &expr, level)?
                    .try_cast::<ImmutableString>()
                {
                    let module = self.module_resolver.resolve(self, &path, expr.position())?;

                    if let Some(name_def) = alias {
                        if !module.is_indexed() {
                            // Index the module (making a clone copy if necessary) if it is not indexed
                            let mut module = crate::fn_native::shared_take_or_clone(module);
                            module.build_index();
                            mods.push(name_def.name.clone(), module);
                        } else {
                            mods.push(name_def.name.clone(), module);
                        }
                        // When imports list is modified, clear the functions lookup cache
                        state.functions_cache.clear();
                    }

                    state.modules += 1;

                    Ok(Dynamic::UNIT)
                } else {
                    Err(self.make_type_mismatch_err::<ImmutableString>("", expr.position()))
                }
            }

            // Export statement
            #[cfg(not(feature = "no_module"))]
            Stmt::Export(list, _) => {
                for (Ident { name, pos: id_pos }, rename) in list.iter() {
                    // Mark scope variables as public
                    if let Some(index) = scope.get_index(name).map(|(i, _)| i) {
                        let alias = rename.as_ref().map(|x| &x.name).unwrap_or_else(|| name);
                        scope.add_entry_alias(index, alias.clone());
                    } else {
                        return EvalAltResult::ErrorVariableNotFound(name.to_string(), *id_pos)
                            .into();
                    }
                }
                Ok(Dynamic::UNIT)
            }

            // Share statement
            #[cfg(not(feature = "no_closure"))]
            Stmt::Share(x) => {
                if let Some((index, _)) = scope.get_index(&x.name) {
                    let val = scope.get_mut_by_index(index);

                    if !val.is_shared() {
                        // Replace the variable with a shared value.
                        *val = crate::stdlib::mem::take(val).into_shared();
                    }
                }
                Ok(Dynamic::UNIT)
            }
        };

        self.check_data_size(result, stmt.position())
    }

    /// Check a result to ensure that the data size is within allowable limit.
    /// [`Position`] in [`EvalAltResult`] may be None and should be set afterwards.
    #[cfg(feature = "unchecked")]
    #[inline(always)]
    fn check_data_size(
        &self,
        result: Result<Dynamic, Box<EvalAltResult>>,
        _pos: Position,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        result
    }

    /// Check a result to ensure that the data size is within allowable limit.
    #[cfg(not(feature = "unchecked"))]
    fn check_data_size(
        &self,
        result: Result<Dynamic, Box<EvalAltResult>>,
        pos: Position,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        // If no data size limits, just return
        let mut total = 0;

        total += self.max_string_size();
        #[cfg(not(feature = "no_index"))]
        {
            total += self.max_array_size();
        }
        #[cfg(not(feature = "no_object"))]
        {
            total += self.max_map_size();
        }

        if total == 0 {
            return result;
        }

        // Recursively calculate the size of a value (especially `Array` and `Map`)
        fn calc_size(value: &Dynamic) -> (usize, usize, usize) {
            match value {
                #[cfg(not(feature = "no_index"))]
                Dynamic(Union::Array(arr, _)) => {
                    let mut arrays = 0;
                    let mut maps = 0;

                    arr.iter().for_each(|value| match value {
                        Dynamic(Union::Array(_, _)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        #[cfg(not(feature = "no_object"))]
                        Dynamic(Union::Map(_, _)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        _ => arrays += 1,
                    });

                    (arrays, maps, 0)
                }
                #[cfg(not(feature = "no_object"))]
                Dynamic(Union::Map(map, _)) => {
                    let mut arrays = 0;
                    let mut maps = 0;

                    map.values().for_each(|value| match value {
                        #[cfg(not(feature = "no_index"))]
                        Dynamic(Union::Array(_, _)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        Dynamic(Union::Map(_, _)) => {
                            let (a, m, _) = calc_size(value);
                            arrays += a;
                            maps += m;
                        }
                        _ => maps += 1,
                    });

                    (arrays, maps, 0)
                }
                Dynamic(Union::Str(s, _)) => (0, 0, s.len()),
                _ => (0, 0, 0),
            }
        }

        match result {
            // Simply return all errors
            Err(_) => return result,
            // String with limit
            Ok(Dynamic(Union::Str(_, _))) if self.max_string_size() > 0 => (),
            // Array with limit
            #[cfg(not(feature = "no_index"))]
            Ok(Dynamic(Union::Array(_, _))) if self.max_array_size() > 0 => (),
            // Map with limit
            #[cfg(not(feature = "no_object"))]
            Ok(Dynamic(Union::Map(_, _))) if self.max_map_size() > 0 => (),
            // Everything else is simply returned
            Ok(_) => return result,
        };

        let (_arr, _map, s) = calc_size(result.as_ref().unwrap());

        if s > self.max_string_size() {
            return EvalAltResult::ErrorDataTooLarge("Length of string".to_string(), pos).into();
        }

        #[cfg(not(feature = "no_index"))]
        if _arr > self.max_array_size() {
            return EvalAltResult::ErrorDataTooLarge("Size of array".to_string(), pos).into();
        }

        #[cfg(not(feature = "no_object"))]
        if _map > self.max_map_size() {
            return EvalAltResult::ErrorDataTooLarge("Size of object map".to_string(), pos).into();
        }

        result
    }

    /// Check if the number of operations stay within limit.
    pub(crate) fn inc_operations(
        &self,
        state: &mut State,
        pos: Position,
    ) -> Result<(), Box<EvalAltResult>> {
        state.operations += 1;

        #[cfg(not(feature = "unchecked"))]
        // Guard against too many operations
        if self.max_operations() > 0 && state.operations > self.max_operations() {
            return EvalAltResult::ErrorTooManyOperations(pos).into();
        }

        // Report progress - only in steps
        if let Some(progress) = &self.progress {
            if let Some(token) = progress(state.operations) {
                // Terminate script if progress returns a termination token
                return EvalAltResult::ErrorTerminated(token, pos).into();
            }
        }

        Ok(())
    }

    /// Map a type_name into a pretty-print name
    #[inline(always)]
    pub(crate) fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .get(name)
            .map(String::as_str)
            .unwrap_or_else(|| map_std_type_name(name))
    }

    /// Make a `Box<`[`EvalAltResult<ErrorMismatchDataType>`][EvalAltResult::ErrorMismatchDataType]`>`.
    #[inline(always)]
    pub(crate) fn make_type_mismatch_err<T>(&self, typ: &str, pos: Position) -> Box<EvalAltResult> {
        EvalAltResult::ErrorMismatchDataType(
            typ.into(),
            self.map_type_name(type_name::<T>()).into(),
            pos,
        )
        .into()
    }
}
