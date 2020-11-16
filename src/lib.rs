//! # Rhai - embedded scripting for Rust
//!
//! Rhai is a tiny, simple and fast embedded scripting language for Rust
//! that gives you a safe and easy way to add scripting to your applications.
//! It provides a familiar syntax based on JavaScript and Rust and a simple Rust interface.
//! Here is a quick example.
//!
//! First, the contents of `my_script.rhai`:
//!
//! ```,ignore
//! // Brute force factorial function
//! fn factorial(x) {
//!     if x == 1 { return 1; }
//!     x * factorial(x - 1)
//! }
//!
//! // Calling an external function 'compute'
//! compute(factorial(10))
//! ```
//!
//! And the Rust part:
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
//! #   #[cfg(not(target_arch = "wasm32"))]
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
//! See [The Rhai Book](https://schungx.github.io/rhai) for details on the Rhai script engine and language.

#![cfg_attr(feature = "no_std", no_std)]

#[cfg(feature = "no_std")]
extern crate alloc;

mod ast;
mod dynamic;
mod engine;
mod engine_api;
mod engine_settings;
mod fn_args;
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
#[cfg(feature = "serde")]
mod serde_impl;
mod stdlib;
mod syntax;
mod token;
mod r#unsafe;
mod utils;

/// The system integer type. It is defined as `i64`.
///
/// If the `only_i32` feature is enabled, this will be `i32` instead.
#[cfg(not(feature = "only_i32"))]
pub type INT = i64;

/// The system integer type.
/// It is defined as `i32` since the `only_i32` feature is used.
///
/// If the `only_i32` feature is not enabled, this will be `i64` instead.
#[cfg(feature = "only_i32")]
pub type INT = i32;

/// The system floating-point type. It is defined as `f64`.
///
/// Not available under the `no_float` feature.
#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "f32_float"))]
pub type FLOAT = f64;

/// The system floating-point type.
/// It is defined as `f32` since the `f32_float` feature is used.
///
/// Not available under the `no_float` feature.
#[cfg(not(feature = "no_float"))]
#[cfg(feature = "f32_float")]
pub type FLOAT = f32;

pub use ast::AST;
pub use dynamic::Dynamic;
pub use engine::{Engine, EvalContext};
pub use fn_native::{FnPtr, NativeCallContext};
pub use fn_register::{RegisterFn, RegisterResultFn};
pub use module::Module;
pub use parse_error::{LexError, ParseError, ParseErrorType};
pub use result::EvalAltResult;
pub use scope::Scope;
pub use syntax::Expression;
pub use token::{Position, NO_POS};
pub use utils::ImmutableString;

#[allow(dead_code)]
use fn_native::{Locked, Shared};

#[cfg(feature = "internals")]
pub use utils::{calc_native_fn_hash, calc_script_fn_hash};

#[cfg(not(feature = "internals"))]
pub(crate) use utils::{calc_native_fn_hash, calc_script_fn_hash};

pub use rhai_codegen::*;

#[cfg(not(feature = "no_function"))]
pub use ast::FnAccess;

#[cfg(not(feature = "no_function"))]
pub use fn_func::Func;

#[cfg(not(feature = "no_index"))]
pub use engine::Array;

#[cfg(not(feature = "no_object"))]
pub use engine::Map;

#[cfg(not(feature = "no_module"))]
pub use module::ModuleResolver;

/// Module containing all built-in _module resolvers_ available to Rhai.
#[cfg(not(feature = "no_module"))]
pub use crate::module::resolvers as module_resolvers;

/// _[SERDE]_ Serialization and deserialization support for [`serde`](https://crates.io/crates/serde).
/// Exported under the `serde` feature.
#[cfg(feature = "serde")]
pub mod serde {
    pub use super::serde_impl::de::from_dynamic;
    pub use super::serde_impl::ser::to_dynamic;
}

#[cfg(not(feature = "no_optimize"))]
pub use optimize::OptimizationLevel;

// Expose internal data structures.
#[cfg(feature = "internals")]
#[deprecated(note = "this type is volatile and may change")]
pub use token::{get_next_token, parse_string_literal, InputStream, Token, TokenizeState};

#[cfg(feature = "internals")]
#[deprecated(note = "this type is volatile and may change")]
pub use ast::{
    BinaryExpr, CustomExpr, Expr, FnCallExpr, Ident, IdentX, ReturnType, ScriptFnDef, Stmt,
};

#[cfg(feature = "internals")]
#[deprecated(note = "this type is volatile and may change")]
pub use engine::{Imports, Limits, State as EvalState};

#[cfg(feature = "internals")]
#[deprecated(note = "this type is volatile and may change")]
pub use module::NamespaceRef;

/// _[INTERNALS]_ Alias to [`smallvec::SmallVec<[T; 4]>`](https://crates.io/crates/smallvec),
/// which is a specialized `Vec` backed by a small, fixed-size array when there are <= 4 items stored.
/// Exported under the `internals` feature only.
#[cfg(not(feature = "internals"))]
type StaticVec<T> = smallvec::SmallVec<[T; 4]>;

/// _[INTERNALS]_ Alias to [`smallvec::SmallVec<[T; 4]>`](https://crates.io/crates/smallvec),
/// which is a specialized `Vec` backed by a small, fixed-size array when there are <= 4 items stored.
/// Exported under the `internals` feature only.
#[cfg(feature = "internals")]
pub type StaticVec<T> = smallvec::SmallVec<[T; 4]>;
