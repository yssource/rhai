//! Main module defining the script evaluation [`Engine`].

use crate::api::custom_syntax::CustomSyntax;
use crate::func::native::{OnDebugCallback, OnParseTokenCallback, OnPrintCallback, OnVarCallback};
use crate::packages::{Package, StandardPackage};
use crate::tokenizer::Token;
use crate::types::dynamic::{map_std_type_name, Union};
use crate::{
    Dynamic, Identifier, ImmutableString, Module, Position, RhaiError, RhaiResult, Shared,
    StaticVec, ERR,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::type_name,
    collections::{BTreeMap, BTreeSet},
    fmt,
    num::NonZeroU8,
};

pub type Precedence = NonZeroU8;

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
#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_module"))]
pub const KEYWORD_GLOBAL: &str = "global";
#[cfg(not(feature = "no_object"))]
pub const FN_GET: &str = "get$";
#[cfg(not(feature = "no_object"))]
pub const FN_SET: &str = "set$";
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
pub const FN_IDX_GET: &str = "index$get$";
#[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
pub const FN_IDX_SET: &str = "index$set$";
#[cfg(not(feature = "no_function"))]
pub const FN_ANONYMOUS: &str = "anon$";

/// Standard equality comparison operator.
///
/// Some standard functions (e.g. searching an [`Array`][crate::Array]) implicitly call this
/// function to compare two [`Dynamic`] values.
pub const OP_EQUALS: &str = Token::EqualsTo.literal_syntax();

/// Standard concatenation operator.
///
/// Used primarily to build up interpolated strings.
pub const OP_CONCAT: &str = Token::PlusAssign.literal_syntax();

/// Standard containment testing function.
///
/// The `in` operator is implemented as a call to this function.
pub const OP_CONTAINS: &str = "contains";

/// Standard exclusive range operator.
pub const OP_EXCLUSIVE_RANGE: &str = Token::ExclusiveRange.literal_syntax();

/// Standard inclusive range operator.
pub const OP_INCLUSIVE_RANGE: &str = Token::InclusiveRange.literal_syntax();

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
    /// A collection of all modules loaded into the global namespace of the Engine.
    pub(crate) global_modules: StaticVec<Shared<Module>>,
    /// A collection of all sub-modules directly loaded into the Engine.
    pub(crate) global_sub_modules: BTreeMap<Identifier, Shared<Module>>,

    /// A module resolution service.
    #[cfg(not(feature = "no_module"))]
    pub(crate) module_resolver: Option<Box<dyn crate::ModuleResolver>>,

    /// A map mapping type names to pretty-print names.
    pub(crate) type_names: BTreeMap<Identifier, Identifier>,

    /// An empty [`ImmutableString`] for cloning purposes.
    pub(crate) empty_string: ImmutableString,

    /// A set of symbols to disable.
    pub(crate) disabled_symbols: BTreeSet<Identifier>,
    /// A map containing custom keywords and precedence to recognize.
    pub(crate) custom_keywords: BTreeMap<Identifier, Option<Precedence>>,
    /// Custom syntax.
    pub(crate) custom_syntax: BTreeMap<Identifier, Box<CustomSyntax>>,
    /// Callback closure for resolving variable access.
    pub(crate) resolve_var: Option<OnVarCallback>,
    /// Callback closure to remap tokens during parsing.
    pub(crate) token_mapper: Option<Box<OnParseTokenCallback>>,

    /// Callback closure for implementing the `print` command.
    pub(crate) print: Option<OnPrintCallback>,
    /// Callback closure for implementing the `debug` command.
    pub(crate) debug: Option<OnDebugCallback>,
    /// Callback closure for progress reporting.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) progress: Option<crate::func::native::OnProgressCallback>,

    /// Optimize the [`AST`][crate::AST] after compilation.
    #[cfg(not(feature = "no_optimize"))]
    pub(crate) optimization_level: crate::OptimizationLevel,

    /// Language options.
    pub(crate) options: crate::api::options::LanguageOptions,

    /// Max limits.
    #[cfg(not(feature = "unchecked"))]
    pub(crate) limits: crate::api::limits::Limits,
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
#[must_use]
pub fn make_getter(id: &str) -> Identifier {
    let mut buf = Identifier::new_const();
    buf.push_str(FN_GET);
    buf.push_str(id);
    buf
}

/// Make setter function
#[cfg(not(feature = "no_object"))]
#[inline(always)]
#[must_use]
pub fn make_setter(id: &str) -> Identifier {
    let mut buf = Identifier::new_const();
    buf.push_str(FN_SET);
    buf.push_str(id);
    buf
}

/// Is this function an anonymous function?
#[cfg(not(feature = "no_function"))]
#[inline(always)]
#[must_use]
pub fn is_anonymous_fn(fn_name: &str) -> bool {
    fn_name.starts_with(FN_ANONYMOUS)
}

/// Print to `stdout`
#[inline]
#[allow(unused_variables)]
fn print_to_stdout(s: &str) {
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg(not(target_arch = "wasm64"))]
    println!("{}", s);
}

/// Debug to `stdout`
#[inline]
#[allow(unused_variables)]
fn debug_to_stdout(s: &str, source: Option<&str>, pos: Position) {
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg(not(target_arch = "wasm64"))]
    if let Some(source) = source {
        println!("{}{:?} | {}", source, pos, s);
    } else if pos.is_none() {
        println!("{}", s);
    } else {
        println!("{:?} | {}", pos, s);
    }
}

impl Engine {
    /// Create a new [`Engine`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        // Create the new scripting Engine
        let mut engine = Self::new_raw();

        #[cfg(not(feature = "no_module"))]
        #[cfg(not(feature = "no_std"))]
        #[cfg(not(target_arch = "wasm32"))]
        #[cfg(not(target_arch = "wasm64"))]
        {
            engine.module_resolver =
                Some(Box::new(crate::module::resolvers::FileModuleResolver::new()));
        }

        // default print/debug implementations
        engine.print = Some(Box::new(print_to_stdout));
        engine.debug = Some(Box::new(debug_to_stdout));

        engine.register_global_module(StandardPackage::new().as_shared_module());

        engine
    }

    /// Create a new [`Engine`] with minimal built-in functions.
    ///
    /// Use [`register_global_module`][Engine::register_global_module] to add packages of functions.
    #[inline]
    #[must_use]
    pub fn new_raw() -> Self {
        let mut engine = Self {
            global_modules: StaticVec::new_const(),
            global_sub_modules: BTreeMap::new(),

            #[cfg(not(feature = "no_module"))]
            module_resolver: None,

            type_names: BTreeMap::new(),
            empty_string: ImmutableString::new(),
            disabled_symbols: BTreeSet::new(),
            custom_keywords: BTreeMap::new(),
            custom_syntax: BTreeMap::new(),

            resolve_var: None,
            token_mapper: None,

            print: None,
            debug: None,

            #[cfg(not(feature = "unchecked"))]
            progress: None,

            #[cfg(not(feature = "no_optimize"))]
            optimization_level: crate::OptimizationLevel::default(),

            options: crate::api::options::LanguageOptions::new(),

            #[cfg(not(feature = "unchecked"))]
            limits: crate::api::limits::Limits::new(),
        };

        // Add the global namespace module
        let mut global_namespace = Module::new();
        global_namespace.internal = true;
        engine.global_modules.push(global_namespace.into());

        engine
    }

    /// Get an empty [`ImmutableString`].
    ///
    /// [`Engine`] keeps a single instance of an empty [`ImmutableString`] and uses this to create
    /// shared instances for subsequent uses. This minimizes unnecessary allocations for empty strings.
    #[inline(always)]
    #[must_use]
    pub fn const_empty_string(&self) -> ImmutableString {
        self.empty_string.clone()
    }

    /// Check a result to ensure that it is valid.
    pub(crate) fn check_return_value(&self, mut result: RhaiResult, pos: Position) -> RhaiResult {
        let _pos = pos;

        match result {
            Ok(ref mut r) => {
                // Concentrate all empty strings into one instance to save memory
                if let Dynamic(Union::Str(s, _, _)) = r {
                    if s.is_empty() {
                        if !s.ptr_eq(&self.empty_string) {
                            *s = self.const_empty_string();
                        }
                        return result;
                    }
                }

                #[cfg(not(feature = "unchecked"))]
                self.check_data_size(&r, _pos)?;
            }
            _ => (),
        }

        result
    }

    /// Pretty-print a type name.
    ///
    /// If a type is registered via [`register_type_with_name`][Engine::register_type_with_name],
    /// the type name provided for the registration will be used.
    #[inline]
    #[must_use]
    pub fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .get(name)
            .map(|s| s.as_str())
            .unwrap_or_else(|| map_std_type_name(name))
    }

    /// Make a `Box<`[`EvalAltResult<ErrorMismatchDataType>`][ERR::ErrorMismatchDataType]`>`.
    #[inline]
    #[must_use]
    pub(crate) fn make_type_mismatch_err<T>(&self, typ: &str, pos: Position) -> RhaiError {
        ERR::ErrorMismatchDataType(self.map_type_name(type_name::<T>()).into(), typ.into(), pos)
            .into()
    }
}
