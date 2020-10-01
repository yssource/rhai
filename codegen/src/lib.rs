//! This crate contains procedural macros to make creating Rhai plugin-modules much easier.
//!
//! # Export an Entire Rust Module to a Rhai `Module`
//!
//! ```
//! use rhai::{EvalAltResult, FLOAT};
//! use rhai::plugin::*;
//! use rhai::module_resolvers::*;
//!
//! #[export_module]
//! mod advanced_math {
//!     pub const MYSTIC_NUMBER: FLOAT = 42.0 as FLOAT;
//!
//!     pub fn euclidean_distance(x1: FLOAT, y1: FLOAT, x2: FLOAT, y2: FLOAT) -> FLOAT {
//!         ((y2 - y1).abs().powf(2.0) + (x2 -x1).abs().powf(2.0)).sqrt()
//!     }
//! }
//!
//! fn main() -> Result<(), Box<EvalAltResult>> {
//!     let mut engine = Engine::new();
//!     let m = rhai::exported_module!(advanced_math);
//!     let mut r = StaticModuleResolver::new();
//!     r.insert("Math::Advanced".to_string(), m);
//!     engine.set_module_resolver(Some(r));
//!
//!     assert_eq!(engine.eval::<FLOAT>(
//!         r#"import "Math::Advanced" as math;
//!            let m = math::MYSTIC_NUMBER;
//!            let x = math::euclidean_distance(0.0, 1.0, 0.0, m);
//!            x"#)?, 41.0);
//!     Ok(())
//! }
//! ```
//!
//! # Register a Rust Function with a Rhai `Module`
//!
//! ```
//! use rhai::{EvalAltResult, FLOAT, Module, RegisterFn};
//! use rhai::plugin::*;
//! use rhai::module_resolvers::*;
//!
//! #[export_fn]
//! fn distance_function(x1: FLOAT, y1: FLOAT, x2: FLOAT, y2: FLOAT) -> FLOAT {
//!     ((y2 - y1).abs().powf(2.0) + (x2 -x1).abs().powf(2.0)).sqrt()
//! }
//!
//! fn main() -> Result<(), Box<EvalAltResult>> {
//!
//!     let mut engine = Engine::new();
//!     engine.register_fn("get_mystic_number", || { 42 as FLOAT });
//!     let mut m = Module::new();
//!     rhai::set_exported_fn!(m, "euclidean_distance", distance_function);
//!     let mut r = StaticModuleResolver::new();
//!     r.insert("Math::Advanced".to_string(), m);
//!     engine.set_module_resolver(Some(r));
//!
//!     assert_eq!(engine.eval::<FLOAT>(
//!         r#"import "Math::Advanced" as math;
//!            let m = get_mystic_number();
//!            let x = math::euclidean_distance(0.0, 1.0, 0.0, m);
//!            x"#)?, 41.0);
//!     Ok(())
//! }
//! ```
//!
//! # Register a Plugin Function with an `Engine`
//!
//! ```
//! use rhai::{EvalAltResult, FLOAT, Module, RegisterFn};
//! use rhai::plugin::*;
//! use rhai::module_resolvers::*;
//!
//! #[rhai::export_fn]
//! pub fn distance_function(x1: FLOAT, y1: FLOAT, x2: FLOAT, y2: FLOAT) -> FLOAT {
//!     ((y2 - y1).abs().powf(2.0) + (x2 -x1).abs().powf(2.0)).sqrt()
//! }
//!
//! fn main() -> Result<(), Box<EvalAltResult>> {
//!
//!     let mut engine = Engine::new();
//!     engine.register_fn("get_mystic_number", || { 42 as FLOAT });
//!     rhai::register_exported_fn!(engine, "euclidean_distance", distance_function);
//!
//!     assert_eq!(engine.eval::<FLOAT>(
//!         r#"let m = get_mystic_number();
//!            let x = euclidean_distance(0.0, 1.0, 0.0, m);
//!            x"#)?, 41.0);
//!     Ok(())
//! }
//! ```
//!

use quote::quote;
use syn::parse_macro_input;

mod attrs;
mod function;
mod module;
mod register;
mod rhai_module;

#[cfg(test)]
mod test;

/// Attribute, when put on a Rust function, turns it into a _plugin function_.
///
/// # Usage
///
/// ```,ignore
/// use rhai::plugin::*;
///
/// #[export_fn]
/// fn my_plugin_function(...) {
///   ...
/// }
/// ```
#[proc_macro_attribute]
pub fn export_fn(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut output = proc_macro2::TokenStream::from(input.clone());

    let parsed_params = match crate::attrs::outer_item_attributes(args.into(), "export_fn") {
        Ok(args) => args,
        Err(err) => return proc_macro::TokenStream::from(err.to_compile_error()),
    };
    let mut function_def = parse_macro_input!(input as function::ExportedFn);
    if let Err(e) = function_def.set_params(parsed_params) {
        return e.to_compile_error().into();
    }

    output.extend(function_def.generate());
    proc_macro::TokenStream::from(output)
}

/// Attribute, when put on a Rust module, turns it into a _plugin module_.
///
/// # Usage
///
/// ```,ignore
/// use rhai::plugin::*;
///
/// #[export_module]
/// mod my_plugin_module {
///   ...
/// }
/// ```
#[proc_macro_attribute]
pub fn export_module(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let parsed_params = match crate::attrs::outer_item_attributes(args.into(), "export_module") {
        Ok(args) => args,
        Err(err) => return proc_macro::TokenStream::from(err.to_compile_error()),
    };
    let mut module_def = parse_macro_input!(input as module::Module);
    if let Err(e) = module_def.set_params(parsed_params) {
        return e.to_compile_error().into();
    }

    let tokens = module_def.generate();
    proc_macro::TokenStream::from(tokens)
}

/// Macro to generate a Rhai `Module` from a _plugin module_.
///
/// # Usage
///
/// ```,ignore
/// use rhai::plugin::*;
///
/// #[export_module]
/// mod my_plugin_module {
///   ...
/// }
///
/// let module = exported_module!(my_plugin_module);
/// ```
#[proc_macro]
pub fn exported_module(module_path: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module_path = parse_macro_input!(module_path as syn::Path);
    let tokens = quote::quote! {
        #module_path::rhai_module_generate()
    };
    proc_macro::TokenStream::from(tokens)
}

/// Macro to combine a _plugin module_ into an existing module.
///
/// Functions and variables in the plugin module overrides any existing similarly-named
/// functions and variables in the target module.
///
/// This call is intended to be used within the `def_package!` macro to define a custom
/// package based on a plugin module.
///
/// All sub-modules, if any, in the plugin module are _flattened_ and their functions/variables
/// registered at the top level because packages require so.
///
/// The text string name in the second parameter can be anything and is reserved for future use;
/// it is recommended to be an ID string that uniquely identifies the plugin module.
///
/// # Usage
///
/// ```,ignore
/// use rhai::plugin::*;
///
/// #[export_module]
/// mod my_plugin_module {
///   ...
/// }
///
/// let mut module = Module::new();
///
/// combine_with_exported_module!(&mut module, "my_plugin_module_ID", my_plugin_module);
/// ```
#[proc_macro]
pub fn combine_with_exported_module(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (module_expr, _export_name, module_path) = match crate::register::parse_register_macro(args)
    {
        Ok(triple) => triple,
        Err(e) => return e.to_compile_error().into(),
    };
    let tokens = quote! {
        #module_path::rhai_generate_into_module(#module_expr, true);
    };
    proc_macro::TokenStream::from(tokens)
}

/// Macro to register a _plugin function_ into an `Engine`.
///
/// # Usage
///
/// ```,ignore
/// use rhai::plugin::*;
///
/// #[export_fn]
/// fn my_plugin_function(...) {
///   ...
/// }
///
/// let mut engine = Engine::new();
///
/// register_exported_fn!(engine, "calc", my_plugin_function);
/// ```
#[proc_macro]
pub fn register_exported_fn(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (engine_expr, export_name, rust_modpath) = match crate::register::parse_register_macro(args)
    {
        Ok(triple) => triple,
        Err(e) => return e.to_compile_error().into(),
    };
    let gen_mod_path = crate::register::generated_module_path(&rust_modpath);
    let tokens = quote! {
        #engine_expr.register_result_fn(&(#export_name), #gen_mod_path::dynamic_result_fn);
    };
    proc_macro::TokenStream::from(tokens)
}

/// Macro to register a _plugin function_ into a Rhai `Module`.
///
/// # Usage
///
/// ```,ignore
/// use rhai::plugin::*;
///
/// #[export_fn]
/// fn my_plugin_function(...) {
///   ...
/// }
///
/// let mut module = Module::new();
///
/// set_exported_fn!(module, "calc", my_plugin_function);
/// ```
#[proc_macro]
pub fn set_exported_fn(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let (module_expr, export_name, rust_modpath) = match crate::register::parse_register_macro(args)
    {
        Ok(triple) => triple,
        Err(e) => return e.to_compile_error().into(),
    };
    let gen_mod_path = crate::register::generated_module_path(&rust_modpath);
    let tokens = quote! {
        #module_expr.set_fn(#export_name, FnAccess::Public,
                            #gen_mod_path::token_input_types().as_ref(),
                            #gen_mod_path::token_callable());
    };
    proc_macro::TokenStream::from(tokens)
}
