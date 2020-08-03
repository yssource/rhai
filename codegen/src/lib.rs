//!
//! This crate contains procedural macros to make creating Rhai modules much easier.
//!
//! # Exporting a Macro to Rhai
//!
//! ```
//! use rhai::{EvalAltResult, FLOAT};
//! use rhai::plugin::*;
//! use rhai::module_resolvers::*;
//!
//! #[rhai::export_module]
//! pub mod advanced_math {
//!     use rhai::FLOAT;
//!
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
//! # Exporting a Function to Rhai
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
//!     let mut m = Module::new();
//!     rhai::register_exported_fn!(m, "euclidean_distance", distance_function);
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

use quote::{quote, quote_spanned};
use syn::{parse::Parser, parse_macro_input, spanned::Spanned};

mod function;
mod module;
mod rhai_module;

#[proc_macro_attribute]
pub fn export_fn(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut output = proc_macro2::TokenStream::from(input.clone());
    let function_def = parse_macro_input!(input as function::ExportedFn);
    output.extend(function_def.generate());
    proc_macro::TokenStream::from(output)
}

#[proc_macro_attribute]
pub fn export_module(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let module_def = parse_macro_input!(input as module::Module);
    let tokens = module_def.generate();
    proc_macro::TokenStream::from(tokens)
}

#[proc_macro]
pub fn exported_module(module_path: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let module_path = parse_macro_input!(module_path as syn::Path);
    let tokens = quote::quote! {
        #module_path::rhai_module__generate()
    };
    proc_macro::TokenStream::from(tokens)
}

#[proc_macro]
pub fn register_exported_fn(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parser = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_separated_nonempty;
    let args = parser.parse(args).unwrap();
    let arg_span = args.span();
    let items: Vec<syn::Expr> = args.into_iter().collect();
    if items.len() != 3 {
        return proc_macro::TokenStream::from(
            syn::Error::new(arg_span, "this macro requires three arguments").to_compile_error(),
        );
    }
    let rhai_module = &items[0];
    let export_name = match &items[1] {
        syn::Expr::Lit(litstr) => quote_spanned!(items[1].span()=>
                                                 #litstr.to_string()),
        expr => quote! { #expr },
    };
    let rust_modpath = if let syn::Expr::Path(ref path) = &items[2] {
        &path.path
    } else {
        return proc_macro::TokenStream::from(
            syn::Error::new(items[2].span(), "third argument must be a function name")
                .to_compile_error(),
        );
    };
    let gen_mod_path: syn::punctuated::Punctuated<syn::PathSegment, _> = {
        let mut g = rust_modpath.clone().segments;
        g.pop();
        let ident = syn::Ident::new(
            &format!("rhai_fn__{}", rust_modpath.segments.last().unwrap().ident),
            items[2].span(),
        );
        g.push_value(syn::PathSegment {
            ident,
            arguments: syn::PathArguments::None,
        });
        g
    };
    let tokens = quote! {
        #rhai_module.set_fn(#export_name, rhai::FnAccess::Public,
                            #gen_mod_path::Token__input_types().as_ref(),
                            #gen_mod_path::Token__callable());

    };
    proc_macro::TokenStream::from(tokens)
}
