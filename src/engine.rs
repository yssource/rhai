//! Main module defining the script evaluation [`Engine`].

use crate::ast::{Expr, FnCallExpr, FnCallHash, Ident, OpAssignment, ReturnType, Stmt, StmtBlock};
use crate::dynamic::{map_std_type_name, AccessMode, Union, Variant};
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
    num::{NonZeroU8, NonZeroUsize},
    ops::DerefMut,
    string::{String, ToString},
    vec::Vec,
};
use crate::syntax::CustomSyntax;
use crate::utils::{get_hasher, StraightHasherBuilder};
use crate::{
    Dynamic, EvalAltResult, FnPtr, ImmutableString, Module, Position, RhaiResult, Scope, Shared,
    StaticVec,
};

#[cfg(not(feature = "no_index"))]
use crate::{calc_fn_hash, stdlib::iter::empty, Array};

#[cfg(not(feature = "no_index"))]
pub const TYPICAL_ARRAY_SIZE: usize = 8; // Small arrays are typical

#[cfg(not(feature = "no_object"))]
use crate::Map;

#[cfg(not(feature = "no_object"))]
pub const TYPICAL_MAP_SIZE: usize = 8; // Small maps are typical

pub type Precedence = NonZeroU8;

/// _(INTERNALS)_ A stack of imported [modules][Module].
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
//
// # Implementation Notes
//
// We cannot use Cow<str> here because `eval` may load a [module][Module] and
// the module name will live beyond the AST of the eval script text.
// The best we can do is a shared reference.
#[derive(Debug, Clone, Default)]
pub struct Imports(StaticVec<ImmutableString>, StaticVec<Shared<Module>>);

impl Imports {
    /// Get the length of this stack of imported [modules][Module].
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Is this stack of imported [modules][Module] empty?
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// Get the imported [modules][Module] at a particular index.
    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<Shared<Module>> {
        self.1.get(index).cloned()
    }
    /// Get the index of an imported [modules][Module] by name.
    #[inline(always)]
    pub fn find(&self, name: &str) -> Option<usize> {
        self.0
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, key)| if key.as_str() == name { Some(i) } else { None })
    }
    /// Push an imported [modules][Module] onto the stack.
    #[inline(always)]
    pub fn push(&mut self, name: impl Into<ImmutableString>, module: impl Into<Shared<Module>>) {
        self.0.push(name.into());
        self.1.push(module.into());
    }
    /// Truncate the stack of imported [modules][Module] to a particular length.
    #[inline(always)]
    pub fn truncate(&mut self, size: usize) {
        self.0.truncate(size);
        self.1.truncate(size);
    }
    /// Get an iterator to this stack of imported [modules][Module] in reverse order.
    #[allow(dead_code)]
    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = (&str, &Module)> {
        self.0
            .iter()
            .zip(self.1.iter())
            .rev()
            .map(|(name, module)| (name.as_str(), module.as_ref()))
    }
    /// Get an iterator to this stack of imported [modules][Module] in reverse order.
    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn iter_raw(&self) -> impl Iterator<Item = (&ImmutableString, &Shared<Module>)> {
        self.0.iter().rev().zip(self.1.iter().rev())
    }
    /// Get an iterator to this stack of imported [modules][Module] in forward order.
    #[allow(dead_code)]
    #[inline(always)]
    pub(crate) fn scan_raw(&self) -> impl Iterator<Item = (&ImmutableString, &Shared<Module>)> {
        self.0.iter().zip(self.1.iter())
    }
    /// Get a consuming iterator to this stack of imported [modules][Module] in reverse order.
    #[inline(always)]
    pub fn into_iter(self) -> impl Iterator<Item = (ImmutableString, Shared<Module>)> {
        self.0.into_iter().rev().zip(self.1.into_iter().rev())
    }
    /// Does the specified function hash key exist in this stack of imported [modules][Module]?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn contains_fn(&self, hash: u64) -> bool {
        self.1.iter().any(|m| m.contains_qualified_fn(hash))
    }
    /// Get specified function via its hash key.
    #[inline(always)]
    pub fn get_fn(&self, hash: u64) -> Option<(&CallableFunction, Option<&ImmutableString>)> {
        self.1
            .iter()
            .rev()
            .find_map(|m| m.get_qualified_fn(hash).map(|f| (f, m.id_raw())))
    }
    /// Does the specified [`TypeId`][std::any::TypeId] iterator exist in this stack of
    /// imported [modules][Module]?
    #[allow(dead_code)]
    #[inline(always)]
    pub fn contains_iter(&self, id: TypeId) -> bool {
        self.1.iter().any(|m| m.contains_qualified_iter(id))
    }
    /// Get the specified [`TypeId`][std::any::TypeId] iterator.
    #[inline(always)]
    pub fn get_iter(&self, id: TypeId) -> Option<IteratorFn> {
        self.1.iter().rev().find_map(|m| m.get_qualified_iter(id))
    }
}

#[cfg(not(feature = "unchecked"))]
#[cfg(debug_assertions)]
#[cfg(not(feature = "no_function"))]
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
#[cfg(not(feature = "no_function"))]
pub const MAX_CALL_STACK_DEPTH: usize = 64;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(debug_assertions))]
pub const MAX_EXPR_DEPTH: usize = 64;
#[cfg(not(feature = "unchecked"))]
#[cfg(not(feature = "no_function"))]
#[cfg(not(debug_assertions))]
pub const MAX_FUNCTION_EXPR_DEPTH: usize = 32;

pub const MAX_DYNAMIC_PARAMETERS: usize = 16;

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
#[cfg(not(feature = "no_function"))]
pub const KEYWORD_IS_DEF_FN: &str = "is_def_fn";
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

/// Standard equality comparison operator.
pub const OP_EQUALS: &str = "==";

/// Standard method function for containment testing.
///
/// The `in` operator is implemented as a call to this method.
pub const OP_CONTAINS: &str = "contains";

/// Method of chaining.
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ChainType {
    /// Not a chaining type.
    NonChaining,
    /// Indexing.
    Index,
    /// Dotting.
    Dot,
}

/// Value of a chaining argument.
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
#[derive(Debug, Clone, Hash)]
pub enum ChainArgument {
    /// Dot-property access.
    Property(Position),
    /// Arguments to a dot-function call.
    FnCallArgs(StaticVec<Dynamic>, StaticVec<Position>),
    /// Index value.
    IndexValue(Dynamic, Position),
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
impl ChainArgument {
    /// Return the `Dynamic` value.
    ///
    /// # Panics
    ///
    /// Panics if not `ChainArgument::IndexValue`.
    #[inline(always)]
    #[cfg(not(feature = "no_index"))]
    pub fn as_index_value(self) -> Dynamic {
        match self {
            Self::Property(_) | Self::FnCallArgs(_, _) => {
                panic!("expecting ChainArgument::IndexValue")
            }
            Self::IndexValue(value, _) => value,
        }
    }
    /// Return the `StaticVec<Dynamic>` value.
    ///
    /// # Panics
    ///
    /// Panics if not `ChainArgument::FnCallArgs`.
    #[inline(always)]
    #[cfg(not(feature = "no_object"))]
    pub fn as_fn_call_args(self) -> (StaticVec<Dynamic>, StaticVec<Position>) {
        match self {
            Self::Property(_) | Self::IndexValue(_, _) => {
                panic!("expecting ChainArgument::FnCallArgs")
            }
            Self::FnCallArgs(values, positions) => (values, positions),
        }
    }
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
impl From<(StaticVec<Dynamic>, StaticVec<Position>)> for ChainArgument {
    #[inline(always)]
    fn from((values, positions): (StaticVec<Dynamic>, StaticVec<Position>)) -> Self {
        Self::FnCallArgs(values, positions)
    }
}

#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
impl From<(Dynamic, Position)> for ChainArgument {
    #[inline(always)]
    fn from((value, pos): (Dynamic, Position)) -> Self {
        Self::IndexValue(value, pos)
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
    pub fn set_value(
        &mut self,
        new_val: Dynamic,
        _pos: Position,
    ) -> Result<(), Box<EvalAltResult>> {
        match self {
            Self::Ref(r) => **r = new_val,
            #[cfg(not(feature = "no_closure"))]
            #[cfg(not(feature = "no_object"))]
            Self::LockGuard((r, _)) => **r = new_val,
            Self::Value(_) => panic!("cannot update a value"),
            #[cfg(not(feature = "no_index"))]
            Self::StringChar(s, index, _) => {
                let s = &mut *s.write_lock::<ImmutableString>().unwrap();

                // Replace the character at the specified index position
                let new_ch = new_val.as_char().map_err(|err| {
                    Box::new(EvalAltResult::ErrorMismatchDataType(
                        "char".to_string(),
                        err.to_string(),
                        _pos,
                    ))
                })?;

                let index = *index;

                *s = s
                    .chars()
                    .enumerate()
                    .map(|(i, ch)| if i == index { new_ch } else { ch })
                    .collect();
            }
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

/// An entry in a function resolution cache.
#[derive(Debug, Clone)]
pub struct FnResolutionCacheEntry {
    /// Function.
    pub func: CallableFunction,
    /// Optional source.
    pub source: Option<ImmutableString>,
}

/// A function resolution cache.
pub type FnResolutionCache = HashMap<u64, Option<FnResolutionCacheEntry>, StraightHasherBuilder>;

/// _(INTERNALS)_ A type that holds all the current states of the [`Engine`].
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
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
    /// Level of the current scope.  The global (root) level is zero, a new block
    /// (or function call) is one level higher, and so on.
    pub scope_level: usize,
    /// Number of operations performed.
    pub operations: u64,
    /// Number of modules loaded.
    pub modules: usize,
    /// Embedded module resolver.
    #[cfg(not(feature = "no_module"))]
    pub resolver: Option<Shared<crate::module::resolvers::StaticModuleResolver>>,
    /// Function resolution cache and free list.
    fn_resolution_caches: (StaticVec<FnResolutionCache>, Vec<FnResolutionCache>),
}

impl State {
    /// Is the state currently at global (root) level?
    #[inline(always)]
    pub fn is_global(&self) -> bool {
        self.scope_level == 0
    }
    /// Get a mutable reference to the current function resolution cache.
    pub fn fn_resolution_cache_mut(&mut self) -> &mut FnResolutionCache {
        if self.fn_resolution_caches.0.is_empty() {
            self.fn_resolution_caches
                .0
                .push(HashMap::with_capacity_and_hasher(64, StraightHasherBuilder));
        }
        self.fn_resolution_caches.0.last_mut().unwrap()
    }
    /// Push an empty function resolution cache onto the stack and make it current.
    #[allow(dead_code)]
    pub fn push_fn_resolution_cache(&mut self) {
        self.fn_resolution_caches
            .0
            .push(self.fn_resolution_caches.1.pop().unwrap_or_default());
    }
    /// Remove the current function resolution cache from the stack and make the last one current.
    ///
    /// # Panics
    ///
    /// Panics if there are no more function resolution cache in the stack.
    pub fn pop_fn_resolution_cache(&mut self) {
        let mut cache = self.fn_resolution_caches.0.pop().unwrap();
        cache.clear();
        self.fn_resolution_caches.1.push(cache);
    }
}

/// _(INTERNALS)_ A type containing all the limits imposed by the [`Engine`].
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[cfg(not(feature = "unchecked"))]
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Limits {
    /// Maximum levels of call-stack to prevent infinite recursion.
    ///
    /// Set to zero to effectively disable function calls.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    pub max_call_stack_depth: usize,
    /// Maximum depth of statements/expressions at global level.
    pub max_expr_depth: Option<NonZeroUsize>,
    /// Maximum depth of statements/expressions in functions.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    pub max_function_expr_depth: Option<NonZeroUsize>,
    /// Maximum number of operations allowed to run.
    pub max_operations: Option<crate::stdlib::num::NonZeroU64>,
    /// Maximum number of [modules][Module] allowed to load.
    ///
    /// Set to zero to effectively disable loading any [module][Module].
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    pub max_modules: usize,
    /// Maximum length of a [string][ImmutableString].
    pub max_string_size: Option<NonZeroUsize>,
    /// Maximum length of an [array][Array].
    ///
    /// Not available under `no_index`.
    #[cfg(not(feature = "no_index"))]
    pub max_array_size: Option<NonZeroUsize>,
    /// Maximum number of properties in an [object map][Map].
    ///
    /// Not available under `no_object`.
    #[cfg(not(feature = "no_object"))]
    pub max_map_size: Option<NonZeroUsize>,
}

/// Context of a script evaluation process.
#[derive(Debug)]
pub struct EvalContext<'a, 'x, 'px, 'm, 's, 't, 'pt> {
    pub(crate) engine: &'a Engine,
    pub(crate) scope: &'x mut Scope<'px>,
    pub(crate) mods: &'m mut Imports,
    pub(crate) state: &'s mut State,
    pub(crate) lib: &'a [&'a Module],
    pub(crate) this_ptr: &'t mut Option<&'pt mut Dynamic>,
    pub(crate) level: usize,
}

impl<'x, 'px> EvalContext<'_, 'x, 'px, '_, '_, '_, '_> {
    /// The current [`Engine`].
    #[inline(always)]
    pub fn engine(&self) -> &Engine {
        self.engine
    }
    /// The current source.
    #[inline(always)]
    pub fn source(&self) -> Option<&str> {
        self.state.source.as_ref().map(|s| s.as_str())
    }
    /// The current [`Scope`].
    #[inline(always)]
    pub fn scope(&self) -> &Scope {
        self.scope
    }
    /// Mutable reference to the current [`Scope`].
    #[inline(always)]
    pub fn scope_mut(&mut self) -> &mut &'x mut Scope<'px> {
        &mut self.scope
    }
    /// Get an iterator over the current set of modules imported via `import` statements.
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn iter_imports(&self) -> impl Iterator<Item = (&str, &Module)> {
        self.mods.iter()
    }
    /// _(INTERNALS)_ The current set of modules imported via `import` statements.
    /// Available under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub fn imports(&self) -> &Imports {
        self.mods
    }
    /// Get an iterator over the namespaces containing definition of all script-defined functions.
    #[inline(always)]
    pub fn iter_namespaces(&self) -> impl Iterator<Item = &Module> {
        self.lib.iter().cloned()
    }
    /// _(INTERNALS)_ The current set of namespaces containing definitions of all script-defined functions.
    /// Available under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline(always)]
    pub fn namespaces(&self) -> &[&Module] {
        self.lib
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
    pub(crate) custom_keywords: HashMap<String, Option<Precedence>>,
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
        f.write_str("Engine")
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
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    println!("{}", _s);
}

/// Debug to stdout
#[inline(always)]
fn default_debug(_s: &str, _source: Option<&str>, _pos: Position) {
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
    if let Some(source) = _source {
        println!("{} @ {:?} | {}", source, _pos, _s);
    } else {
        println!("{:?} | {}", _pos, _s);
    }
}

impl Engine {
    /// Create a new [`Engine`]
    #[inline]
    pub fn new() -> Self {
        // Create the new scripting Engine
        let mut engine = Self {
            global_namespace: Default::default(),
            global_modules: Default::default(),
            global_sub_modules: Default::default(),

            #[cfg(not(feature = "no_module"))]
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
            module_resolver: Box::new(crate::module::resolvers::FileModuleResolver::new()),
            #[cfg(not(feature = "no_module"))]
            #[cfg(any(feature = "no_std", target_arch = "wasm32",))]
            module_resolver: Box::new(crate::module::resolvers::DummyModuleResolver::new()),

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
                #[cfg(not(feature = "no_function"))]
                max_call_stack_depth: MAX_CALL_STACK_DEPTH,
                max_expr_depth: NonZeroUsize::new(MAX_EXPR_DEPTH),
                #[cfg(not(feature = "no_function"))]
                max_function_expr_depth: NonZeroUsize::new(MAX_FUNCTION_EXPR_DEPTH),
                max_operations: None,
                #[cfg(not(feature = "no_module"))]
                max_modules: usize::MAX,
                max_string_size: None,
                #[cfg(not(feature = "no_index"))]
                max_array_size: None,
                #[cfg(not(feature = "no_object"))]
                max_map_size: None,
            },

            disable_doc_comments: false,
        };

        engine.register_global_module(StandardPackage::new().as_shared_module());

        engine
    }

    /// Create a new [`Engine`] with minimal built-in functions.
    ///
    /// Use [`register_global_module`][Engine::register_global_module] to add packages of functions.
    #[inline(always)]
    pub fn new_raw() -> Self {
        Self {
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
                #[cfg(not(feature = "no_function"))]
                max_call_stack_depth: MAX_CALL_STACK_DEPTH,
                max_expr_depth: NonZeroUsize::new(MAX_EXPR_DEPTH),
                #[cfg(not(feature = "no_function"))]
                max_function_expr_depth: NonZeroUsize::new(MAX_FUNCTION_EXPR_DEPTH),
                max_operations: None,
                #[cfg(not(feature = "no_module"))]
                max_modules: usize::MAX,
                max_string_size: None,
                #[cfg(not(feature = "no_index"))]
                max_array_size: None,
                #[cfg(not(feature = "no_object"))]
                max_map_size: None,
            },

            disable_doc_comments: false,
        }
    }

    /// Search for a module within an imports stack.
    #[inline]
    pub(crate) fn search_imports(
        &self,
        mods: &Imports,
        state: &mut State,
        namespace: &NamespaceRef,
    ) -> Option<Shared<Module>> {
        let root = &namespace[0].name;

        // Qualified - check if the root module is directly indexed
        let index = if state.always_search {
            None
        } else {
            namespace.index()
        };

        if let Some(index) = index {
            let offset = mods.len() - index.get();
            Some(mods.get(offset).expect("invalid index in Imports"))
        } else {
            mods.find(root)
                .map(|n| mods.get(n).expect("invalid index in Imports"))
                .or_else(|| self.global_sub_modules.get(root).cloned())
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
                (_, Some((hash_var, modules)), Ident { name, pos, .. }) => {
                    let module = self.search_imports(mods, state, modules).ok_or_else(|| {
                        EvalAltResult::ErrorModuleNotFound(
                            modules[0].name.to_string(),
                            modules[0].pos,
                        )
                    })?;
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
            _ => unreachable!("Expr::Variable expected, but gets {:?}", expr),
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
        let (index, _, Ident { name, pos, .. }) = match expr {
            Expr::Variable(v) => v.as_ref(),
            _ => unreachable!("Expr::Variable expected, but gets {:?}", expr),
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
        let index = if state.always_search { &None } else { index };

        // Check the variable resolver, if any
        if let Some(ref resolve_var) = self.resolve_var {
            let index = index.map(NonZeroUsize::get).unwrap_or(0);
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

        let index = if let Some(index) = index {
            scope.len() - index.get()
        } else {
            // Find the variable in the scope
            scope
                .get_index(name)
                .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(name.to_string(), *pos))?
                .0
        };

        let val = scope.get_mut_by_index(index);

        // Check for data race - probably not necessary because the only place it should conflict is
        //                       in a method call when the object variable is also used as a parameter.
        // if cfg!(not(feature = "no_closure")) && val.is_locked() {
        //     return EvalAltResult::ErrorDataRace(name.into(), *pos).into();
        // }

        Ok((val.into(), *pos))
    }

    /// Chain-evaluate a dot/index chain.
    /// [`Position`] in [`EvalAltResult`] is [`NONE`][Position::NONE] and must be set afterwards.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_dot_index_chain_helper(
        &self,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        target: &mut Target,
        rhs: &Expr,
        idx_values: &mut StaticVec<ChainArgument>,
        chain_type: ChainType,
        level: usize,
        new_val: Option<((Dynamic, Position), (&Option<OpAssignment>, Position))>,
    ) -> Result<(Dynamic, bool), Box<EvalAltResult>> {
        assert!(chain_type != ChainType::NonChaining);

        let is_ref = target.is_ref();

        let next_chain = match rhs {
            Expr::Index(_, _) => ChainType::Index,
            Expr::Dot(_, _) => ChainType::Dot,
            _ => ChainType::NonChaining,
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
                        let idx_val = idx_val.as_index_value();
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
                    // xxx[rhs] op= new_val
                    _ if new_val.is_some() => {
                        let idx_val = idx_val.as_index_value();
                        let mut idx_val2 = idx_val.clone();

                        // `call_setter` is introduced to bypass double mutable borrowing of target
                        let _call_setter = match self.get_indexed_mut(
                            mods, state, lib, target_val, idx_val, pos, true, is_ref, false, level,
                        ) {
                            // Indexed value is a reference - update directly
                            Ok(obj_ptr) => {
                                let ((new_val, new_pos), (op_info, op_pos)) = new_val.unwrap();
                                self.eval_op_assignment(
                                    mods, state, lib, op_info, op_pos, obj_ptr, new_val, new_pos,
                                )?;
                                None
                            }
                            Err(err) => match *err {
                                // No index getter - try to call an index setter
                                #[cfg(not(feature = "no_index"))]
                                EvalAltResult::ErrorIndexingType(_, _) => Some(new_val.unwrap()),
                                // Any other error - return
                                err => return err.into(),
                            },
                        };

                        #[cfg(not(feature = "no_index"))]
                        if let Some(mut new_val) = _call_setter {
                            let val_type_name = target_val.type_name();
                            let ((_, val_pos), _) = new_val;

                            let hash_set =
                                FnCallHash::from_native(calc_fn_hash(empty(), FN_IDX_SET, 3));
                            let args = &mut [target_val, &mut idx_val2, &mut (new_val.0).0];

                            self.exec_fn_call(
                                mods, state, lib, FN_IDX_SET, hash_set, args, is_ref, true,
                                val_pos, None, level,
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
                        let idx_val = idx_val.as_index_value();
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
                    Expr::FnCall(x, pos) if x.namespace.is_none() && new_val.is_none() => {
                        let FnCallExpr { name, hash, .. } = x.as_ref();
                        let mut args = idx_val.as_fn_call_args();
                        self.make_method_call(
                            mods, state, lib, name, *hash, target, &mut args, *pos, level,
                        )
                    }
                    // xxx.fn_name(...) = ???
                    Expr::FnCall(_, _) if new_val.is_some() => {
                        unreachable!("method call cannot be assigned to")
                    }
                    // xxx.module::fn_name(...) - syntax error
                    Expr::FnCall(_, _) => {
                        unreachable!("function call in dot chain should not be namespace-qualified")
                    }
                    // {xxx:map}.id op= ???
                    Expr::Property(x) if target_val.is::<Map>() && new_val.is_some() => {
                        let Ident { name, pos, .. } = &x.2;
                        let index = name.clone().into();
                        let val = self.get_indexed_mut(
                            mods, state, lib, target_val, index, *pos, true, is_ref, false, level,
                        )?;
                        let ((new_val, new_pos), (op_info, op_pos)) = new_val.unwrap();
                        self.eval_op_assignment(
                            mods, state, lib, op_info, op_pos, val, new_val, new_pos,
                        )?;
                        Ok((Dynamic::UNIT, true))
                    }
                    // {xxx:map}.id
                    Expr::Property(x) if target_val.is::<Map>() => {
                        let Ident { name, pos, .. } = &x.2;
                        let index = name.clone().into();
                        let val = self.get_indexed_mut(
                            mods, state, lib, target_val, index, *pos, false, is_ref, false, level,
                        )?;

                        Ok((val.take_or_clone(), false))
                    }
                    // xxx.id = ???
                    Expr::Property(x) if new_val.is_some() => {
                        let (_, (setter, hash_set), Ident { pos, .. }) = x.as_ref();
                        let hash = FnCallHash::from_native(*hash_set);
                        let mut new_val = new_val;
                        let mut args = [target_val, &mut (new_val.as_mut().unwrap().0).0];
                        self.exec_fn_call(
                            mods, state, lib, setter, hash, &mut args, is_ref, true, *pos, None,
                            level,
                        )
                        .map(|(v, _)| (v, true))
                    }
                    // xxx.id
                    Expr::Property(x) => {
                        let ((getter, hash_get), _, Ident { pos, .. }) = x.as_ref();
                        let hash = FnCallHash::from_native(*hash_get);
                        let mut args = [target_val];
                        self.exec_fn_call(
                            mods, state, lib, getter, hash, &mut args, is_ref, true, *pos, None,
                            level,
                        )
                        .map(|(v, _)| (v, false))
                    }
                    // {xxx:map}.sub_lhs[expr] | {xxx:map}.sub_lhs.expr
                    Expr::Index(x, x_pos) | Expr::Dot(x, x_pos) if target_val.is::<Map>() => {
                        let mut val = match &x.lhs {
                            Expr::Property(p) => {
                                let Ident { name, pos, .. } = &p.2;
                                let index = name.clone().into();
                                self.get_indexed_mut(
                                    mods, state, lib, target_val, index, *pos, false, is_ref, true,
                                    level,
                                )?
                            }
                            // {xxx:map}.fn_name(arg_expr_list)[expr] | {xxx:map}.fn_name(arg_expr_list).expr
                            Expr::FnCall(x, pos) if x.namespace.is_none() => {
                                let FnCallExpr { name, hash, .. } = x.as_ref();
                                let mut args = idx_val.as_fn_call_args();
                                let (val, _) = self.make_method_call(
                                    mods, state, lib, name, *hash, target, &mut args, *pos, level,
                                )?;
                                val.into()
                            }
                            // {xxx:map}.module::fn_name(...) - syntax error
                            Expr::FnCall(_, _) => unreachable!(
                                "function call in dot chain should not be namespace-qualified"
                            ),
                            // Others - syntax error
                            expr => unreachable!("invalid dot expression: {:?}", expr),
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
                                let ((getter, hash_get), (setter, hash_set), Ident { pos, .. }) =
                                    p.as_ref();
                                let hash_get = FnCallHash::from_native(*hash_get);
                                let hash_set = FnCallHash::from_native(*hash_set);
                                let arg_values = &mut [target_val, &mut Default::default()];
                                let args = &mut arg_values[..1];
                                let (mut val, updated) = self.exec_fn_call(
                                    mods, state, lib, getter, hash_get, args, is_ref, true, *pos,
                                    None, level,
                                )?;

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
                                        mods, state, lib, setter, hash_set, arg_values, is_ref,
                                        true, *pos, None, level,
                                    )
                                    .or_else(
                                        |err| match *err {
                                            // If there is no setter, no need to feed it back because
                                            // the property is read-only
                                            EvalAltResult::ErrorDotExpr(_, _) => {
                                                Ok((Dynamic::UNIT, false))
                                            }
                                            _ => Err(err),
                                        },
                                    )?;
                                }

                                Ok((result, may_be_changed))
                            }
                            // xxx.fn_name(arg_expr_list)[expr] | xxx.fn_name(arg_expr_list).expr
                            Expr::FnCall(f, pos) if f.namespace.is_none() => {
                                let FnCallExpr { name, hash, .. } = f.as_ref();
                                let mut args = idx_val.as_fn_call_args();
                                let (mut val, _) = self.make_method_call(
                                    mods, state, lib, name, *hash, target, &mut args, *pos, level,
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
                            Expr::FnCall(_, _) => unreachable!(
                                "function call in dot chain should not be namespace-qualified"
                            ),
                            // Others - syntax error
                            expr => unreachable!("invalid dot expression: {:?}", expr),
                        }
                    }
                    // Syntax error
                    _ => EvalAltResult::ErrorDotExpr("".into(), rhs.position()).into(),
                }
            }

            chain_type => unreachable!("invalid ChainType: {:?}", chain_type),
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
        new_val: Option<((Dynamic, Position), (&Option<OpAssignment>, Position))>,
    ) -> RhaiResult {
        let (crate::ast::BinaryExpr { lhs, rhs }, chain_type, op_pos) = match expr {
            Expr::Index(x, pos) => (x.as_ref(), ChainType::Index, *pos),
            Expr::Dot(x, pos) => (x.as_ref(), ChainType::Dot, *pos),
            _ => unreachable!("index or dot chain expected, but gets {:?}", expr),
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
                    ..
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
            _ if new_val.is_some() => unreachable!("cannot assign to an expression"),
            // {expr}.??? or {expr}[???]
            expr => {
                let value = self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?;
                let obj_ptr = &mut value.into();
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
    /// [`StaticVec`] is used to avoid an allocation in the overwhelming cases of
    /// just a few levels of indexing.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn eval_indexed_chain(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        expr: &Expr,
        parent_chain_type: ChainType,
        idx_values: &mut StaticVec<ChainArgument>,
        size: usize,
        level: usize,
    ) -> Result<(), Box<EvalAltResult>> {
        self.inc_operations(state, expr.position())?;

        match expr {
            Expr::FnCall(x, _) if parent_chain_type == ChainType::Dot && x.namespace.is_none() => {
                let mut arg_positions: StaticVec<_> = Default::default();

                let arg_values = x
                    .args
                    .iter()
                    .map(|arg_expr| {
                        arg_positions.push(arg_expr.position());
                        self.eval_expr(scope, mods, state, lib, this_ptr, arg_expr, level)
                            .map(Dynamic::flatten)
                    })
                    .collect::<Result<StaticVec<_>, _>>()?;

                idx_values.push((arg_values, arg_positions).into());
            }
            Expr::FnCall(_, _) if parent_chain_type == ChainType::Dot => {
                unreachable!("function call in dot chain should not be namespace-qualified")
            }

            Expr::Property(x) if parent_chain_type == ChainType::Dot => {
                idx_values.push(ChainArgument::Property(x.2.pos))
            }
            Expr::Property(_) => unreachable!("unexpected Expr::Property for indexing"),

            Expr::Index(x, _) | Expr::Dot(x, _) => {
                let crate::ast::BinaryExpr { lhs, rhs, .. } = x.as_ref();

                // Evaluate in left-to-right order
                let lhs_val = match lhs {
                    Expr::Property(x) if parent_chain_type == ChainType::Dot => {
                        ChainArgument::Property(x.2.pos)
                    }
                    Expr::Property(_) => unreachable!("unexpected Expr::Property for indexing"),
                    Expr::FnCall(x, _)
                        if parent_chain_type == ChainType::Dot && x.namespace.is_none() =>
                    {
                        let mut arg_positions: StaticVec<_> = Default::default();

                        let arg_values = x
                            .args
                            .iter()
                            .map(|arg_expr| {
                                arg_positions.push(arg_expr.position());
                                self.eval_expr(scope, mods, state, lib, this_ptr, arg_expr, level)
                                    .map(Dynamic::flatten)
                            })
                            .collect::<Result<StaticVec<_>, _>>()?;

                        (arg_values, arg_positions).into()
                    }
                    Expr::FnCall(_, _) if parent_chain_type == ChainType::Dot => {
                        unreachable!("function call in dot chain should not be namespace-qualified")
                    }
                    _ => self
                        .eval_expr(scope, mods, state, lib, this_ptr, lhs, level)
                        .map(|v| (v.flatten(), lhs.position()).into())?,
                };

                // Push in reverse order
                let chain_type = match expr {
                    Expr::Index(_, _) => ChainType::Index,
                    Expr::Dot(_, _) => ChainType::Dot,
                    _ => unreachable!("index or dot chain expected, but gets {:?}", expr),
                };
                self.eval_indexed_chain(
                    scope, mods, state, lib, this_ptr, rhs, chain_type, idx_values, size, level,
                )?;

                idx_values.push(lhs_val);
            }

            _ => idx_values.push(
                self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)
                    .map(|v| (v.flatten(), expr.position()).into())?,
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
                let hash_get = FnCallHash::from_native(calc_fn_hash(empty(), FN_IDX_GET, 2));
                self.exec_fn_call(
                    _mods, state, _lib, FN_IDX_GET, hash_get, args, _is_ref, true, idx_pos, None,
                    _level,
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
    ) -> RhaiResult {
        self.inc_operations(state, expr.position())?;

        let result = match expr {
            Expr::DynamicConstant(x, _) => Ok(x.as_ref().clone()),
            Expr::IntegerConstant(x, _) => Ok((*x).into()),
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(x, _) => Ok((*x).into()),
            Expr::StringConstant(x, _) => Ok(x.clone().into()),
            Expr::CharConstant(x, _) => Ok((*x).into()),
            Expr::FnPointer(x, _) => Ok(FnPtr::new_unchecked(x.clone(), Default::default()).into()),

            Expr::Variable(x) if (x.2).name == KEYWORD_THIS => this_ptr
                .as_deref()
                .cloned()
                .ok_or_else(|| EvalAltResult::ErrorUnboundThis((x.2).pos).into()),
            Expr::Variable(_) => self
                .search_namespace(scope, mods, state, lib, this_ptr, expr)
                .map(|(val, _)| val.take_or_clone()),

            // Statement block
            Expr::Stmt(x) if x.is_empty() => Ok(Dynamic::UNIT),
            Expr::Stmt(x) => {
                let statements = &x.statements;
                self.eval_stmt_block(scope, mods, state, lib, this_ptr, statements, true, level)
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
                    arr.push(
                        self.eval_expr(scope, mods, state, lib, this_ptr, item, level)?
                            .flatten(),
                    );
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
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                            .flatten(),
                    );
                }
                Ok(Dynamic(Union::Map(Box::new(map), AccessMode::ReadWrite)))
            }

            // Normal function call
            Expr::FnCall(x, pos) if x.namespace.is_none() => {
                let FnCallExpr {
                    name,
                    capture,
                    hash,
                    args,
                    ..
                } = x.as_ref();
                self.make_function_call(
                    scope, mods, state, lib, this_ptr, name, args, *hash, *pos, *capture, level,
                )
            }

            // Namespace-qualified function call
            Expr::FnCall(x, pos) if x.namespace.is_some() => {
                let FnCallExpr {
                    name,
                    namespace,
                    hash,
                    args,
                    ..
                } = x.as_ref();
                let namespace = namespace.as_ref();
                let hash = hash.native_hash();
                self.make_qualified_function_call(
                    scope, mods, state, lib, this_ptr, namespace, name, args, hash, *pos, level,
                )
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
                let custom_def = self
                    .custom_syntax
                    .get(custom.tokens.first().unwrap())
                    .unwrap();
                let mut context = EvalContext {
                    engine: self,
                    scope,
                    mods,
                    state,
                    lib,
                    this_ptr,
                    level,
                };
                (custom_def.func)(&mut context, &expressions)
            }

            _ => unreachable!("expression cannot be evaluated: {:?}", expr),
        };

        self.check_data_size(result, expr.position())
    }

    /// Evaluate a statements block.
    pub(crate) fn eval_stmt_block(
        &self,
        scope: &mut Scope,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        statements: &[Stmt],
        restore_prev_state: bool,
        level: usize,
    ) -> RhaiResult {
        if statements.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        let mut _extra_fn_resolution_cache = false;
        let prev_always_search = state.always_search;
        let prev_scope_len = scope.len();
        let prev_mods_len = mods.len();

        if restore_prev_state {
            state.scope_level += 1;
        }

        let result = statements.iter().try_fold(Dynamic::UNIT, |_, stmt| {
            let _mods_len = mods.len();

            let r = self.eval_stmt(scope, mods, state, lib, this_ptr, stmt, level)?;

            #[cfg(not(feature = "no_module"))]
            if matches!(stmt, Stmt::Import(_, _, _)) {
                // Get the extra modules - see if any functions are marked global.
                // Without global functions, the extra modules never affect function resolution.
                if mods
                    .scan_raw()
                    .skip(_mods_len)
                    .any(|(_, m)| m.contains_indexed_global_functions())
                {
                    if _extra_fn_resolution_cache {
                        // When new module is imported with global functions and there is already
                        // a new cache, clear it - notice that this is expensive as all function
                        // resolutions must start again
                        state.fn_resolution_cache_mut().clear();
                    } else if restore_prev_state {
                        // When new module is imported with global functions, push a new cache
                        state.push_fn_resolution_cache();
                        _extra_fn_resolution_cache = true;
                    } else {
                        // When the block is to be evaluated in-place, just clear the current cache
                        state.fn_resolution_cache_mut().clear();
                    }
                }
            }

            Ok(r)
        });

        if _extra_fn_resolution_cache {
            // If imports list is modified, pop the functions lookup cache
            state.pop_fn_resolution_cache();
        }

        if restore_prev_state {
            scope.rewind(prev_scope_len);
            mods.truncate(prev_mods_len);
            state.scope_level -= 1;

            // The impact of new local variables goes away at the end of a block
            // because any new variables introduced will go out of scope
            state.always_search = prev_always_search;
        }

        result
    }

    pub(crate) fn eval_op_assignment(
        &self,
        mods: &mut Imports,
        state: &mut State,
        lib: &[&Module],
        op_info: &Option<OpAssignment>,
        op_pos: Position,
        mut target: Target,
        mut new_value: Dynamic,
        new_value_pos: Position,
    ) -> Result<(), Box<EvalAltResult>> {
        if target.as_ref().is_read_only() {
            unreachable!("LHS should not be read-only");
        }

        if let Some(OpAssignment {
            hash_op_assign,
            hash_op,
            op,
        }) = op_info
        {
            let mut lock_guard;
            let lhs_ptr_inner;

            if cfg!(not(feature = "no_closure")) && target.is_shared() {
                lock_guard = target.as_mut().write_lock::<Dynamic>().unwrap();
                lhs_ptr_inner = lock_guard.deref_mut();
            } else {
                lhs_ptr_inner = target.as_mut();
            }

            let hash = *hash_op_assign;
            let args = &mut [lhs_ptr_inner, &mut new_value];

            match self.call_native_fn(mods, state, lib, op, hash, args, true, true, op_pos) {
                Ok(_) => (),
                Err(err) if matches!(err.as_ref(), EvalAltResult::ErrorFunctionNotFound(f, _) if f.starts_with(op.as_ref())) =>
                {
                    // Expand to `var = var op rhs`
                    let op = &op[..op.len() - 1]; // extract operator without =

                    // Run function
                    let (value, _) = self.call_native_fn(
                        mods, state, lib, op, *hash_op, args, true, false, op_pos,
                    )?;

                    *args[0] = value.flatten();
                }
                err => return err.map(|_| ()),
            }

            Ok(())
        } else {
            // Normal assignment
            target.set_value(new_value, new_value_pos)?;
            Ok(())
        }
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
    ) -> RhaiResult {
        self.inc_operations(state, stmt.position())?;

        let result = match stmt {
            // No-op
            Stmt::Noop(_) => Ok(Dynamic::UNIT),

            // Expression as statement
            Stmt::Expr(expr) => Ok(self
                .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                .flatten()),

            // var op= rhs
            Stmt::Assignment(x, op_pos) if x.0.get_variable_access(false).is_some() => {
                let (lhs_expr, rhs_expr, op_info) = x.as_ref();
                let rhs_val = self
                    .eval_expr(scope, mods, state, lib, this_ptr, rhs_expr, level)?
                    .flatten();
                let (lhs_ptr, pos) =
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
                    EvalAltResult::ErrorAssignmentToConstant(
                        lhs_expr.get_variable_access(false).unwrap().to_string(),
                        pos,
                    )
                    .into()
                } else {
                    self.eval_op_assignment(
                        mods,
                        state,
                        lib,
                        op_info,
                        *op_pos,
                        lhs_ptr,
                        rhs_val,
                        rhs_expr.position(),
                    )?;
                    Ok(Dynamic::UNIT)
                }
            }

            // lhs op= rhs
            Stmt::Assignment(x, op_pos) => {
                let (lhs_expr, rhs_expr, op_info) = x.as_ref();
                let rhs_val = self
                    .eval_expr(scope, mods, state, lib, this_ptr, rhs_expr, level)?
                    .flatten();
                let _new_val = Some(((rhs_val, rhs_expr.position()), (op_info, *op_pos)));

                // Must be either `var[index] op= val` or `var.prop op= val`
                match lhs_expr {
                    // name op= rhs (handled above)
                    Expr::Variable(_) => {
                        unreachable!("Expr::Variable case should already been handled")
                    }
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
                    _ => unreachable!("cannot assign to expression: {:?}", lhs_expr),
                }
            }

            // Block scope
            Stmt::Block(statements, _) if statements.is_empty() => Ok(Dynamic::UNIT),
            Stmt::Block(statements, _) => {
                self.eval_stmt_block(scope, mods, state, lib, this_ptr, statements, true, level)
            }

            // If statement
            Stmt::If(expr, x, _) => {
                let (
                    StmtBlock {
                        statements: if_stmt,
                        ..
                    },
                    StmtBlock {
                        statements: else_stmt,
                        ..
                    },
                ) = x.as_ref();
                self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .as_bool()
                    .map_err(|err| self.make_type_mismatch_err::<bool>(err, expr.position()))
                    .and_then(|guard_val| {
                        if guard_val {
                            if !if_stmt.is_empty() {
                                self.eval_stmt_block(
                                    scope, mods, state, lib, this_ptr, if_stmt, true, level,
                                )
                            } else {
                                Ok(Dynamic::UNIT)
                            }
                        } else {
                            if !else_stmt.is_empty() {
                                self.eval_stmt_block(
                                    scope, mods, state, lib, this_ptr, else_stmt, true, level,
                                )
                            } else {
                                Ok(Dynamic::UNIT)
                            }
                        }
                    })
            }

            // Switch statement
            Stmt::Switch(match_expr, x, _) => {
                let (table, def_stmt) = x.as_ref();

                let value = self.eval_expr(scope, mods, state, lib, this_ptr, match_expr, level)?;

                if value.is_hashable() {
                    let hasher = &mut get_hasher();
                    value.hash(hasher);
                    let hash = hasher.finish();

                    table.get(&hash).map(|StmtBlock { statements, .. }| {
                        if !statements.is_empty() {
                            self.eval_stmt_block(
                                scope, mods, state, lib, this_ptr, statements, true, level,
                            )
                        } else {
                            Ok(Dynamic::UNIT)
                        }
                    })
                } else {
                    // Non-hashable values never match any specific clause
                    None
                }
                .unwrap_or_else(|| {
                    // Default match clause
                    let def_stmt = &def_stmt.statements;
                    if !def_stmt.is_empty() {
                        self.eval_stmt_block(
                            scope, mods, state, lib, this_ptr, def_stmt, true, level,
                        )
                    } else {
                        Ok(Dynamic::UNIT)
                    }
                })
            }

            // While loop
            Stmt::While(expr, body, _) => {
                let body = &body.statements;
                loop {
                    let condition = if !expr.is_unit() {
                        self.eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                            .as_bool()
                            .map_err(|err| {
                                self.make_type_mismatch_err::<bool>(err, expr.position())
                            })?
                    } else {
                        true
                    };

                    if !condition {
                        return Ok(Dynamic::UNIT);
                    }
                    if body.is_empty() {
                        continue;
                    }

                    match self.eval_stmt_block(scope, mods, state, lib, this_ptr, body, true, level)
                    {
                        Ok(_) => (),
                        Err(err) => match *err {
                            EvalAltResult::LoopBreak(false, _) => (),
                            EvalAltResult::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                            _ => return Err(err),
                        },
                    }
                }
            }

            // Do loop
            Stmt::Do(body, expr, is_while, _) => {
                let body = &body.statements;

                loop {
                    if !body.is_empty() {
                        match self
                            .eval_stmt_block(scope, mods, state, lib, this_ptr, body, true, level)
                        {
                            Ok(_) => (),
                            Err(err) => match *err {
                                EvalAltResult::LoopBreak(false, _) => continue,
                                EvalAltResult::LoopBreak(true, _) => return Ok(Dynamic::UNIT),
                                _ => return Err(err),
                            },
                        }
                    }

                    if self
                        .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                        .as_bool()
                        .map_err(|err| self.make_type_mismatch_err::<bool>(err, expr.position()))?
                    {
                        if !*is_while {
                            return Ok(Dynamic::UNIT);
                        }
                    } else {
                        if *is_while {
                            return Ok(Dynamic::UNIT);
                        }
                    }
                }
            }

            // For loop
            Stmt::For(expr, x, _) => {
                let (name, StmtBlock { statements, pos }) = x.as_ref();
                let iter_obj = self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .flatten();
                let iter_type = iter_obj.type_id();

                // lib should only contain scripts, so technically they cannot have iterators

                // Search order:
                // 1) Global namespace - functions registered via Engine::register_XXX
                // 2) Global modules - packages
                // 3) Imported modules - functions marked with global namespace
                // 4) Global sub-modules - functions marked with global namespace
                let func = self
                    .global_namespace
                    .get_iter(iter_type)
                    .or_else(|| {
                        self.global_modules
                            .iter()
                            .find_map(|m| m.get_iter(iter_type))
                    })
                    .or_else(|| mods.get_iter(iter_type))
                    .or_else(|| {
                        self.global_sub_modules
                            .values()
                            .find_map(|m| m.get_qualified_iter(iter_type))
                    });

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

                        self.inc_operations(state, *pos)?;

                        if statements.is_empty() {
                            continue;
                        }

                        match self.eval_stmt_block(
                            scope, mods, state, lib, this_ptr, statements, true, level,
                        ) {
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
                let (
                    StmtBlock {
                        statements: try_body,
                        ..
                    },
                    err_var,
                    StmtBlock {
                        statements: catch_body,
                        ..
                    },
                ) = x.as_ref();

                let result = self
                    .eval_stmt_block(scope, mods, state, lib, this_ptr, try_body, true, level)
                    .map(|_| Dynamic::UNIT);

                match result {
                    Ok(_) => result,
                    Err(err) if err.is_pseudo_error() => Err(err),
                    Err(err) if !err.is_catchable() => Err(err),
                    Err(mut err) => {
                        let err_value = match *err {
                            EvalAltResult::ErrorRuntime(ref x, _) => x.clone(),

                            #[cfg(feature = "no_object")]
                            _ => {
                                err.take_position();
                                err.to_string().into()
                            }
                            #[cfg(not(feature = "no_object"))]
                            _ => {
                                use crate::INT;

                                let mut err_map: Map = Default::default();
                                let err_pos = err.take_position();

                                err_map.insert("message".into(), err.to_string().into());

                                if let Some(ref source) = state.source {
                                    err_map.insert("source".into(), source.clone().into());
                                }

                                if err_pos.is_none() {
                                    // No position info
                                } else {
                                    err_map.insert(
                                        "line".into(),
                                        (err_pos.line().unwrap() as INT).into(),
                                    );
                                    err_map.insert(
                                        "position".into(),
                                        if err_pos.is_beginning_of_line() {
                                            0
                                        } else {
                                            err_pos.position().unwrap() as INT
                                        }
                                        .into(),
                                    );
                                }

                                err.dump_fields(&mut err_map);
                                err_map.into()
                            }
                        };

                        let orig_scope_len = scope.len();
                        state.scope_level += 1;

                        if let Some(Ident { name, .. }) = err_var {
                            scope.push(unsafe_cast_var_name_to_lifetime(&name), err_value);
                        }

                        let result = self.eval_stmt_block(
                            scope, mods, state, lib, this_ptr, catch_body, true, level,
                        );

                        state.scope_level -= 1;
                        scope.rewind(orig_scope_len);

                        match result {
                            Ok(_) => Ok(Dynamic::UNIT),
                            Err(result_err) => match *result_err {
                                // Re-throw exception
                                EvalAltResult::ErrorRuntime(Dynamic(Union::Unit(_, _)), pos) => {
                                    err.set_position(pos);
                                    Err(err)
                                }
                                _ => Err(result_err),
                            },
                        }
                    }
                }
            }

            // Return value
            Stmt::Return(ReturnType::Return, Some(expr), pos) => {
                let value = self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .flatten();
                EvalAltResult::Return(value, *pos).into()
            }

            // Empty return
            Stmt::Return(ReturnType::Return, None, pos) => {
                EvalAltResult::Return(Default::default(), *pos).into()
            }

            // Throw value
            Stmt::Return(ReturnType::Exception, Some(expr), pos) => {
                let value = self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .flatten();
                EvalAltResult::ErrorRuntime(value, *pos).into()
            }

            // Empty throw
            Stmt::Return(ReturnType::Exception, None, pos) => {
                EvalAltResult::ErrorRuntime(Dynamic::UNIT, *pos).into()
            }

            // Let/const statement
            Stmt::Let(expr, Ident { name, .. }, export, _)
            | Stmt::Const(expr, Ident { name, .. }, export, _) => {
                let entry_type = match stmt {
                    Stmt::Let(_, _, _, _) => AccessMode::ReadWrite,
                    Stmt::Const(_, _, _, _) => AccessMode::ReadOnly,
                    _ => unreachable!("should be Stmt::Let or Stmt::Const, but gets {:?}", stmt),
                };

                let value = self
                    .eval_expr(scope, mods, state, lib, this_ptr, expr, level)?
                    .flatten();

                let (var_name, _alias): (Cow<'_, str>, _) = if state.is_global() {
                    (
                        name.to_string().into(),
                        if *export { Some(name.clone()) } else { None },
                    )
                } else if *export {
                    unreachable!("exported variable not on global level");
                } else {
                    (unsafe_cast_var_name_to_lifetime(name).into(), None)
                };

                scope.push_dynamic_value(var_name, entry_type, value);

                #[cfg(not(feature = "no_module"))]
                if let Some(alias) = _alias {
                    scope.add_entry_alias(scope.len() - 1, alias);
                }
                Ok(Dynamic::UNIT)
            }

            // Import statement
            #[cfg(not(feature = "no_module"))]
            Stmt::Import(expr, export, _pos) => {
                // Guard against too many modules
                #[cfg(not(feature = "unchecked"))]
                if state.modules >= self.max_modules() {
                    return EvalAltResult::ErrorTooManyModules(*_pos).into();
                }

                if let Some(path) = self
                    .eval_expr(scope, mods, state, lib, this_ptr, &expr, level)?
                    .try_cast::<ImmutableString>()
                {
                    use crate::ModuleResolver;

                    let expr_pos = expr.position();

                    let module = state
                        .resolver
                        .as_ref()
                        .and_then(|r| match r.resolve(self, &path, expr_pos) {
                            Ok(m) => return Some(Ok(m)),
                            Err(err) => match *err {
                                EvalAltResult::ErrorModuleNotFound(_, _) => None,
                                _ => return Some(Err(err)),
                            },
                        })
                        .unwrap_or_else(|| self.module_resolver.resolve(self, &path, expr_pos))?;

                    if let Some(name) = export.as_ref().map(|x| x.name.clone()) {
                        if !module.is_indexed() {
                            // Index the module (making a clone copy if necessary) if it is not indexed
                            let mut module = crate::fn_native::shared_take_or_clone(module);
                            module.build_index();
                            mods.push(name, module);
                        } else {
                            mods.push(name, module);
                        }
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
                for (Ident { name, pos, .. }, rename) in list.iter() {
                    // Mark scope variables as public
                    if let Some(index) = scope.get_index(name).map(|(i, _)| i) {
                        let alias = rename.as_ref().map(|x| &x.name).unwrap_or_else(|| name);
                        scope.add_entry_alias(index, alias.clone());
                    } else {
                        return EvalAltResult::ErrorVariableNotFound(name.to_string(), *pos).into();
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
    fn check_data_size(&self, result: RhaiResult, _pos: Position) -> RhaiResult {
        result
    }

    /// Check a result to ensure that the data size is within allowable limit.
    #[cfg(not(feature = "unchecked"))]
    #[inline(always)]
    fn check_data_size(&self, result: RhaiResult, pos: Position) -> RhaiResult {
        // Simply return all errors
        if result.is_err() {
            return result;
        }

        // If no data size limits, just return
        let mut has_limit = self.limits.max_string_size.is_some();
        #[cfg(not(feature = "no_index"))]
        {
            has_limit = has_limit || self.limits.max_array_size.is_some();
        }
        #[cfg(not(feature = "no_object"))]
        {
            has_limit = has_limit || self.limits.max_map_size.is_some();
        }

        if !has_limit {
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

        let (_arr, _map, s) = calc_size(result.as_ref().unwrap());

        if s > self
            .limits
            .max_string_size
            .map_or(usize::MAX, NonZeroUsize::get)
        {
            return EvalAltResult::ErrorDataTooLarge("Length of string".to_string(), pos).into();
        }

        #[cfg(not(feature = "no_index"))]
        if _arr
            > self
                .limits
                .max_array_size
                .map_or(usize::MAX, NonZeroUsize::get)
        {
            return EvalAltResult::ErrorDataTooLarge("Size of array".to_string(), pos).into();
        }

        #[cfg(not(feature = "no_object"))]
        if _map
            > self
                .limits
                .max_map_size
                .map_or(usize::MAX, NonZeroUsize::get)
        {
            return EvalAltResult::ErrorDataTooLarge("Size of object map".to_string(), pos).into();
        }

        result
    }

    /// Check if the number of operations stay within limit.
    #[inline]
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

    /// Pretty-print a type name.
    ///
    /// If a type is registered via [`register_type_with_name`][Engine::register_type_with_name],
    /// the type name provided for the registration will be used.
    #[inline(always)]
    pub fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .get(name)
            .map(String::as_str)
            .unwrap_or_else(|| map_std_type_name(name))
    }

    /// Make a `Box<`[`EvalAltResult<ErrorMismatchDataType>`][EvalAltResult::ErrorMismatchDataType]`>`.
    #[inline(always)]
    pub(crate) fn make_type_mismatch_err<T>(&self, typ: &str, pos: Position) -> Box<EvalAltResult> {
        EvalAltResult::ErrorMismatchDataType(
            self.map_type_name(type_name::<T>()).into(),
            typ.into(),
            pos,
        )
        .into()
    }
}
