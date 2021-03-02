//! # Rhai - embedded scripting for Rust
//!
//! ![Rhai logo](https://rhai.rs/book/images/logo/rhai-banner-transparent-colour.svg)
//!
//! Rhai is a tiny, simple and fast embedded scripting language for Rust
//! that gives you a safe and easy way to add scripting to your applications.
//!
//! It provides a familiar syntax based on JavaScript+Rust and a simple Rust interface.
//!
//! # A Quick Example
//!
//! ## Contents of `my_script.rhai`
//!
//! ```,ignore
//! /// Brute force factorial function
//! fn factorial(x) {
//!     if x == 1 { return 1; }
//!     x * factorial(x - 1)
//! }
//!
//! // Calling an external function 'compute'
//! compute(factorial(10))
//! ```
//!
//! ## The Rust part
//!
//! ```,no_run
//! use rhai::{Engine, EvalAltResult, RegisterFn};
//!
//! fn main() -> Result<(), Box<EvalAltResult>>
//! {
//!     // Define external function
//!     fn compute_something(x: i64) -> bool {
//!         (x % 40) == 0
//!     }
//!
//!     // Create scripting engine
//!     let mut engine = Engine::new();
//!
//!     // Register external function as 'compute'
//!     engine.register_fn("compute", compute_something);
//!
//! #   #[cfg(not(feature = "no_std"))]
//! #   #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
//!     assert_eq!(
//!         // Evaluate the script, expects a 'bool' return
//!         engine.eval_file::<bool>("my_script.rhai".into())?,
//!         true
//!     );
//!
//!     Ok(())
//! }
//! ```
//!
//! # Documentation
//!
//! See [The Rhai Book](https://rhai.rs/book) for details on the Rhai scripting engine and language.

#![cfg_attr(feature = "no_std", no_std)]

#[cfg(feature = "no_std")]
extern crate alloc;

// Internal modules

mod ast;
mod dynamic;
mod engine;
mod engine_api;
mod engine_settings;
mod fn_args;
mod fn_builtin;
mod fn_call;
mod fn_func;
mod fn_native;
mod fn_register;
mod module;
mod optimize;
pub mod packages;
mod parse_error;
mod parser;
pub mod plugin;
mod result;
mod scope;
mod stdlib;
mod syntax;
mod token;
mod r#unsafe;
mod utils;

pub type RhaiResult = Result<Dynamic, Box<EvalAltResult>>;

/// The system integer type. It is defined as [`i64`].
///
/// If the `only_i32` feature is enabled, this will be [`i32`] instead.
#[cfg(not(feature = "only_i32"))]
pub type INT = i64;

/// The system integer type.
/// It is defined as [`i32`] since the `only_i32` feature is used.
///
/// If the `only_i32` feature is not used, this will be `i64` instead.
#[cfg(feature = "only_i32")]
pub type INT = i32;

/// The system floating-point type. It is defined as [`f64`].
///
/// If the `f32_float` feature is enabled, this will be [`i32`] instead.
///
/// Not available under `no_float`.
#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "f32_float"))]
pub type FLOAT = f64;

/// The system floating-point type.
/// It is defined as [`f32`] since the `f32_float` feature is used.
///
/// If the `f32_float` feature is not used, this will be `f64` instead.
///
/// Not available under `no_float`.
#[cfg(not(feature = "no_float"))]
#[cfg(feature = "f32_float")]
pub type FLOAT = f32;

pub use ast::{FnAccess, ScriptFnMetadata, AST};
pub use dynamic::Dynamic;
pub use engine::{Engine, EvalContext};
pub use fn_native::{FnPtr, NativeCallContext, Shared};
pub use fn_register::{RegisterFn, RegisterResultFn};
pub use module::{FnNamespace, Module};
pub use parse_error::{LexError, ParseError, ParseErrorType};
pub use result::EvalAltResult;
pub use scope::Scope;
pub use syntax::Expression;
pub use token::Position;
pub use utils::ImmutableString;

#[cfg(not(feature = "no_closure"))]
use fn_native::Locked;

#[cfg(feature = "internals")]
pub use utils::{calc_native_fn_hash, calc_script_fn_hash, HashableHashMap};

#[cfg(not(feature = "internals"))]
pub(crate) use utils::{calc_native_fn_hash, calc_script_fn_hash};

pub use rhai_codegen::*;

#[cfg(not(feature = "no_function"))]
pub use fn_func::Func;

#[cfg(not(feature = "no_function"))]
pub use fn_args::FuncArgs;

/// Variable-sized array of [`Dynamic`] values.
///
/// Not available under `no_index`.
#[cfg(not(feature = "no_index"))]
pub type Array = stdlib::vec::Vec<Dynamic>;

/// Hash map of [`Dynamic`] values with [`ImmutableString`] keys.
///
/// Not available under `no_object`.
#[cfg(not(feature = "no_object"))]
pub type Map = stdlib::collections::HashMap<ImmutableString, Dynamic>;

#[cfg(not(feature = "no_module"))]
pub use module::ModuleResolver;

/// Module containing all built-in _module resolvers_ available to Rhai.
#[cfg(not(feature = "no_module"))]
pub use module::resolvers as module_resolvers;

#[cfg(feature = "serde")]
pub mod serde;

#[cfg(not(feature = "no_optimize"))]
pub use optimize::OptimizationLevel;

// Expose internal data structures.
#[cfg(feature = "internals")]
#[deprecated = "this type is volatile and may change"]
pub use token::{get_next_token, parse_string_literal, InputStream, Token, TokenizeState};

#[cfg(feature = "internals")]
#[deprecated = "this type is volatile and may change"]
pub use ast::{
    ASTNode, BinaryExpr, CustomExpr, Expr, FloatWrapper, FnCallExpr, Ident, ReturnType,
    ScriptFnDef, Stmt,
};

#[cfg(feature = "internals")]
#[deprecated = "this type is volatile and may change"]
pub use engine::{Imports, State as EvalState};

#[cfg(feature = "internals")]
#[cfg(not(feature = "unchecked"))]
pub use engine::Limits;

#[cfg(feature = "internals")]
#[deprecated = "this type is volatile and may change"]
pub use module::NamespaceRef;

/// _(INTERNALS)_ Alias to [`smallvec::SmallVec<[T; 4]>`](https://crates.io/crates/smallvec),
/// which is a specialized [`Vec`] backed by a small, fixed-size array when there are <= 4 items stored.
/// Exported under the `internals` feature only.
#[cfg(not(feature = "internals"))]
type StaticVec<T> = smallvec::SmallVec<[T; 4]>;

/// _(INTERNALS)_ Alias to [`smallvec::SmallVec<[T; 4]>`](https://crates.io/crates/smallvec),
/// which is a specialized [`Vec`] backed by a small, fixed-size array when there are <= 4 items stored.
/// Exported under the `internals` feature only.
#[cfg(feature = "internals")]
pub type StaticVec<T> = smallvec::SmallVec<[T; 4]>;

// Compiler guards against mutually-exclusive feature flags

#[cfg(feature = "no_float")]
#[cfg(feature = "f32_float")]
compile_error!("'f32_float' cannot be used with 'no_float'");

#[cfg(feature = "no_std")]
#[cfg(feature = "wasm-bindgen")]
compile_error!("'wasm-bindgen' cannot be used with 'no-std'");

#[cfg(feature = "no_std")]
#[cfg(feature = "stdweb")]
compile_error!("'stdweb' cannot be used with 'no-std'");

#[cfg(any(target_arch = "wasm32", target_arch = "wasm64"))]
#[cfg(feature = "no_std")]
compile_error!("'no_std' cannot be used for WASM target");

#[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
#[cfg(feature = "wasm-bindgen")]
compile_error!("'wasm-bindgen' should not be used non-WASM target");

#[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
#[cfg(feature = "stdweb")]
compile_error!("'stdweb' should not be used non-WASM target");
