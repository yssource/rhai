use quote::{quote, ToTokens};
use syn::{parse::Parse, parse::ParseStream, spanned::Spanned};

use crate::function::{ExportedFn, ExportedFnParams};
use crate::rhai_module::ExportedConst;

#[cfg(no_std)]
use alloc::vec as new_vec;
#[cfg(not(no_std))]
use std::vec as new_vec;

#[cfg(no_std)]
use core::mem;
#[cfg(not(no_std))]
use std::mem;

use std::borrow::Cow;
use std::collections::HashMap;

fn inner_fn_attributes(f: &mut syn::ItemFn) -> syn::Result<ExportedFnParams> {
    // #[cfg] attributes are not allowed on objects
    if let Some(cfg_attr) = f.attrs.iter().find(|a| {
        a.path
            .get_ident()
            .map(|i| i.to_string() == "cfg")
            .unwrap_or(false)
    }) {
        return Err(syn::Error::new(cfg_attr.span(), "cfg attributes not allowed on this item"));
    }

    // Find the #[rhai_fn] attribute which will turn be read for the function parameters.
    if let Some(rhai_fn_idx) = f.attrs.iter().position(|a| {
        a.path
            .get_ident()
            .map(|i| i.to_string() == "rhai_fn")
            .unwrap_or(false)
    }) {
        let rhai_fn_attr = f.attrs.remove(rhai_fn_idx);
        rhai_fn_attr.parse_args()
    } else if let syn::Visibility::Public(_) = f.vis {
        Ok(ExportedFnParams::default())
    } else {
        Ok(ExportedFnParams::skip())
    }
}

fn check_rename_collisions(fns: &Vec<ExportedFn>) -> Result<(), syn::Error> {
    let mut renames = HashMap::<String, proc_macro2::Span>::new();
    let mut names = HashMap::<String, proc_macro2::Span>::new();
    for itemfn in fns.iter() {
        if let Some(ref name) = itemfn.params.name {
            let current_span = itemfn.params.span.as_ref().unwrap();
            let key = itemfn.arg_list().fold(name.clone(), |mut argstr, fnarg| {
                let type_string: String = match fnarg {
                    syn::FnArg::Receiver(_) => unimplemented!("receiver rhai_fns not implemented"),
                    syn::FnArg::Typed(syn::PatType { ref ty, .. }) =>
                        ty.as_ref().to_token_stream().to_string(),
                };
                argstr.push('.');
                argstr.extend(type_string.chars());
                argstr
            });
            if let Some(other_span) = renames.insert(key,
                                                     current_span.clone()) {
                let mut err = syn::Error::new(current_span.clone(),
                                              format!("duplicate Rhai signature for '{}'", &name));
                err.combine(syn::Error::new(other_span,
                                            format!("duplicated function renamed '{}'", &name)));
                return Err(err);
            }
        } else {
            let ident = itemfn.name();
            names.insert(ident.to_string(), ident.span());
        }
    }
    for (new_name, attr_span) in renames.drain() {
        let new_name = new_name.split('.').next().unwrap();
        if let Some(fn_span) = names.get(new_name) {
            let mut err = syn::Error::new(attr_span,
                                          format!("duplicate Rhai signature for '{}'", &new_name));
            err.combine(syn::Error::new(fn_span.clone(),
                                        format!("duplicated function '{}'", &new_name)));
            return Err(err);
        }
    }
    Ok(())
}

fn inner_mod_attributes(f: &mut syn::ItemMod) -> syn::Result<ExportedModParams> {
    if let Some(rhai_mod_idx) = f.attrs.iter().position(|a| {
        a.path
            .get_ident()
            .map(|i| i.to_string() == "rhai_mod")
            .unwrap_or(false)
    }) {
        let rhai_mod_attr = f.attrs.remove(rhai_mod_idx);
        rhai_mod_attr.parse_args()
    } else if let syn::Visibility::Public(_) = f.vis {
        Ok(ExportedModParams::default())
    } else {
        Ok(ExportedModParams::skip())
    }
}

#[derive(Debug, Default)]
pub(crate) struct ExportedModParams {
    pub name: Option<String>,
    pub skip: bool,
}

impl ExportedModParams {
    pub fn skip() -> ExportedModParams {
        let mut skip = ExportedModParams::default();
        skip.skip = true;
        skip
    }
}

impl Parse for ExportedModParams {
    fn parse(args: ParseStream) -> syn::Result<Self> {
        if args.is_empty() {
            return Ok(ExportedModParams::default());
        }

        let arg_list = args.call(
            syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_separated_nonempty,
        )?;

        let mut attrs: HashMap<syn::Ident, Option<syn::LitStr>> = HashMap::new();
        for arg in arg_list {
            let (left, right) = match arg {
                syn::Expr::Assign(syn::ExprAssign {
                    ref left,
                    ref right,
                    ..
                }) => {
                    let attr_name: syn::Ident = match left.as_ref() {
                        syn::Expr::Path(syn::ExprPath {
                            path: attr_path, ..
                        }) => attr_path.get_ident().cloned().ok_or_else(|| {
                            syn::Error::new(attr_path.span(), "expecting attribute name")
                        })?,
                        x => return Err(syn::Error::new(x.span(), "expecting attribute name")),
                    };
                    let attr_value = match right.as_ref() {
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(string),
                            ..
                        }) => string.clone(),
                        x => return Err(syn::Error::new(x.span(), "expecting string literal")),
                    };
                    (attr_name, Some(attr_value))
                }
                syn::Expr::Path(syn::ExprPath {
                    path: attr_path, ..
                }) => attr_path
                    .get_ident()
                    .cloned()
                    .map(|a| (a, None))
                    .ok_or_else(|| syn::Error::new(attr_path.span(), "expecting attribute name"))?,
                x => return Err(syn::Error::new(x.span(), "expecting identifier")),
            };
            attrs.insert(left, right);
        }

        let mut name = None;
        let mut skip = false;
        for (ident, value) in attrs.drain() {
            match (ident.to_string().as_ref(), value) {
                ("name", Some(s)) => name = Some(s.value()),
                ("name", None) => return Err(syn::Error::new(ident.span(), "requires value")),
                ("skip", None) => skip = true,
                ("skip", Some(s)) => {
                    return Err(syn::Error::new(s.span(), "extraneous value"))
                }
                (attr, _) => {
                    return Err(syn::Error::new(
                        ident.span(),
                        format!("unknown attribute '{}'", attr),
                    ))
                }
            }
        }

        Ok(ExportedModParams { name, skip, ..Default::default() })
    }
}

#[derive(Debug)]
pub(crate) struct Module {
    mod_all: Option<syn::ItemMod>,
    fns: Vec<ExportedFn>,
    consts: Vec<ExportedConst>,
    submodules: Vec<Module>,
    params: ExportedModParams,
}

impl Parse for Module {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut mod_all: syn::ItemMod = input.parse()?;
        let fns: Vec<_>;
        let mut consts: Vec<_> = new_vec![];
        let mut submodules: Vec<_> = Vec::new();
        if let Some((_, ref mut content)) = mod_all.content {
            // Gather and parse functions.
            fns = content
                .iter_mut()
                .filter_map(|item| match item {
                    syn::Item::Fn(f) => Some(f),
                    _ => None,
                })
                .try_fold(Vec::new(), |mut vec, mut itemfn| {
                    let params = match inner_fn_attributes(&mut itemfn) {
                        Ok(p) => p,
                        Err(e) => return Err(e),
                    };
                    syn::parse2::<ExportedFn>(itemfn.to_token_stream())
                        .map(|mut f| {
                            f.params = params;
                            f
                        })
                        .map(|f| if !f.params.skip { vec.push(f) })
                        .map(|_| vec)
                })?;
            // Gather and parse constants definitions.
            for item in content.iter() {
                match item {
                    syn::Item::Const(syn::ItemConst {
                        vis,
                        ref expr,
                        ident,
                        attrs,
                        ..
                    }) => {
                        // #[cfg] attributes are not allowed on const declarations
                        if let Some(cfg_attr) = attrs.iter().find(|a| {
                            a.path
                                .get_ident()
                                .map(|i| i.to_string() == "cfg")
                                .unwrap_or(false)
                        }) {
                            return Err(syn::Error::new(
                                    cfg_attr.span(),
                                    "cfg attributes not allowed on this item"));
                        }
                        if let syn::Visibility::Public(_) = vis {
                            consts.push((ident.to_string(), expr.as_ref().clone()));
                        }
                    },
                    _ => {},
                }
            };
            // Gather and parse submodule definitions.
            //
            // They are actually removed from the module's body, because they will need
            // re-generating later when generated code is added.
            submodules.reserve(content.len() - fns.len() - consts.len());
            let mut i = 0;
            while i < content.len() {
                if  let syn::Item::Mod(_) = &content[i] {
                    let mut itemmod = match content.remove(i) {
                        syn::Item::Mod(m) => m,
                        _ => unreachable!(),
                    };
                    let params = match inner_mod_attributes(&mut itemmod) {
                        Ok(p) => p,
                        Err(e) => return Err(e),
                    };
                    let module = syn::parse2::<Module>(itemmod.to_token_stream())
                        .map(|mut f| {
                            f.params = params;
                            f
                        })?;
                    if !module.params.skip {
                        submodules.push(module);
                    }
                } else {
                    i += 1;
                }
            }
        } else {
            fns = new_vec![];
        }
        Ok(Module {
            mod_all: Some(mod_all),
            fns,
            consts,
            submodules,
            params: ExportedModParams::default(),
        })
    }
}

impl Module {
    pub fn attrs(&self) -> Option<&Vec<syn::Attribute>> {
        self.mod_all.as_ref().map(|m| &m.attrs)
    }

    pub fn module_name(&self) -> Option<&syn::Ident> {
        self.mod_all.as_ref().map(|m| &m.ident)
    }

    pub fn exported_name(&self) -> Option<Cow<str>> {
        if let Some(ref s) = self.params.name {
            Some(Cow::Borrowed(s))
        } else {
            self.module_name().map(|m| Cow::Owned(m.to_string()))
        }
    }

    pub fn generate(self) -> proc_macro2::TokenStream {
        match self.generate_inner() {
            Ok(tokens) => tokens,
            Err(e) => e.to_compile_error(),
        }
    }

    fn generate_inner(mut self) -> Result<proc_macro2::TokenStream, syn::Error> {
        // Check for collisions if the "name" attribute was used on inner functions.
        check_rename_collisions(&self.fns)?;

        // Generate new module items.
        //
        // This is done before inner module recursive generation, because that is destructive.
        let mod_gen = crate::rhai_module::generate_body(&self.fns, &self.consts, &self.submodules);

        // NB: submodules must have their new items for exporting generated in depth-first order to
        // avoid issues with reparsing them.
        let inner_modules: Vec<proc_macro2::TokenStream> = self.submodules.drain(..)
            .try_fold::<Vec<proc_macro2::TokenStream>, _,
                        Result<Vec<proc_macro2::TokenStream>, syn::Error>>(
                Vec::new(), |mut acc, m| { acc.push(m.generate_inner()?); Ok(acc) })?;

        // Generate new module items for exporting functions and constant.

        // Rebuild the structure of the module, with the new content added.
        let Module { mod_all, .. } = self;
        let mut mod_all = mod_all.unwrap();
        let mod_name = mod_all.ident.clone();
        let (_, orig_content) = mod_all.content.take().unwrap();
        let mod_attrs = mem::replace(&mut mod_all.attrs, Vec::with_capacity(0));

        Ok(quote! {
            #(#mod_attrs)*
            pub mod #mod_name {
                #(#orig_content)*
                #(#inner_modules)*
                #mod_gen
            }
        })
    }

    pub fn name(&self) -> Option<&syn::Ident> {
        self.mod_all.as_ref().map(|m| &m.ident)
    }

    pub fn content(&self) -> Option<&Vec<syn::Item>> {
        match self.mod_all {
            Some(syn::ItemMod { content: Some((_, ref vec)), .. }) => Some(vec),
            _ => None,
        }
    }
}

#[cfg(test)]
mod module_tests {
    use super::Module;

    use proc_macro2::TokenStream;
    use quote::quote;

    #[test]
    fn empty_module() {
        let input_tokens: TokenStream = quote! {
            pub mod empty { }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
    }

    #[test]
    fn one_factory_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn get_mystic_number() -> INT {
                    42
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.consts.is_empty());
        assert_eq!(item_mod.fns.len(), 1);
        assert_eq!(item_mod.fns[0].name().to_string(), "get_mystic_number");
        assert_eq!(item_mod.fns[0].arg_count(), 0);
        assert_eq!(
            item_mod.fns[0].return_type().unwrap(),
            &syn::parse2::<syn::Type>(quote! { INT }).unwrap()
        );
    }

    #[test]
    fn one_single_arg_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn add_one_to(x: INT) -> INT {
                    x + 1
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.consts.is_empty());
        assert_eq!(item_mod.fns.len(), 1);
        assert_eq!(item_mod.fns[0].name().to_string(), "add_one_to");
        assert_eq!(item_mod.fns[0].arg_count(), 1);
        assert_eq!(
            item_mod.fns[0].arg_list().next().unwrap(),
            &syn::parse2::<syn::FnArg>(quote! { x: INT }).unwrap()
        );
        assert_eq!(
            item_mod.fns[0].return_type().unwrap(),
            &syn::parse2::<syn::Type>(quote! { INT }).unwrap()
        );
    }

    #[test]
    fn one_double_arg_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn add_together(x: INT, y: INT) -> INT {
                    x + y
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        let mut args = item_mod.fns[0].arg_list();
        assert!(item_mod.consts.is_empty());
        assert_eq!(item_mod.fns.len(), 1);
        assert_eq!(item_mod.fns[0].name().to_string(), "add_together");
        assert_eq!(item_mod.fns[0].arg_count(), 2);
        assert_eq!(
            args.next().unwrap(),
            &syn::parse2::<syn::FnArg>(quote! { x: INT }).unwrap()
        );
        assert_eq!(
            args.next().unwrap(),
            &syn::parse2::<syn::FnArg>(quote! { y: INT }).unwrap()
        );
        assert!(args.next().is_none());
        assert_eq!(
            item_mod.fns[0].return_type().unwrap(),
            &syn::parse2::<syn::Type>(quote! { INT }).unwrap()
        );
    }

    #[test]
    fn one_constant_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                pub mod it_is {
                    pub const MYSTIC_NUMBER: INT = 42;
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
        assert_eq!(item_mod.submodules.len(), 1);
        assert_eq!(&item_mod.submodules[0].consts[0].0, "MYSTIC_NUMBER");
        assert_eq!(
            item_mod.submodules[0].consts[0].1,
            syn::parse2::<syn::Expr>(quote! { 42 }).unwrap()
        );
    }

    #[test]
    fn one_skipped_fn_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub mod skip_this {
                    #[rhai_fn(skip)]
                    pub fn get_mystic_number() -> INT {
                        42
                    }
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
        assert_eq!(item_mod.submodules.len(), 1);
        assert!(item_mod.submodules[0].fns.is_empty());
        assert!(item_mod.submodules[0].consts.is_empty());
        assert!(item_mod.submodules[0].submodules.is_empty());
    }

    #[test]
    fn one_skipped_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_mod(skip)]
                pub mod skip_this {
                    pub fn get_mystic_number() -> INT {
                        42
                    }
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
        assert!(item_mod.submodules.is_empty());
    }

    #[test]
    fn one_constant_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                pub const MYSTIC_NUMBER: INT = 42;
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert_eq!(item_mod.consts.len(), 1);
        assert_eq!(&item_mod.consts[0].0, "MYSTIC_NUMBER");
        assert_eq!(
            item_mod.consts[0].1,
            syn::parse2::<syn::Expr>(quote! { 42 }).unwrap()
        );
    }

    #[test]
    fn one_private_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                fn get_mystic_number() -> INT {
                    42
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
    }

    #[test]
    fn one_skipped_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(skip)]
                pub fn get_mystic_number() -> INT {
                    42
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
    }

    #[test]
    fn one_private_constant_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                const MYSTIC_NUMBER: INT = 42;
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns.is_empty());
        assert!(item_mod.consts.is_empty());
    }
}

#[cfg(test)]
mod generate_tests {
    use super::Module;

    use proc_macro2::TokenStream;
    use quote::quote;

    fn assert_streams_eq(actual: TokenStream, expected: TokenStream) {
        let actual = actual.to_string();
        let expected = expected.to_string();
        if &actual != &expected {
            let mut counter = 0;
            let iter = actual
                .chars()
                .zip(expected.chars())
                .inspect(|_| counter += 1)
                .skip_while(|(a, e)| *a == *e);
            let (actual_diff, expected_diff) = {
                let mut actual_diff = String::new();
                let mut expected_diff = String::new();
                for (a, e) in iter.take(50) {
                    actual_diff.push(a);
                    expected_diff.push(e);
                }
                (actual_diff, expected_diff)
            };
            eprintln!("actual != expected, diverge at char {}", counter);
        }
        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_module() {
        let input_tokens: TokenStream = quote! {
            pub mod empty { }
        };

        let expected_tokens = quote! {
            pub mod empty {
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_factory_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn get_mystic_number() -> INT {
                    42
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn get_mystic_number() -> INT {
                    42
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("get_mystic_number", FnAccess::Public, &[],
                             CallableFunction::from_plugin(get_mystic_number_token()));
                    m
                }
                #[allow(non_camel_case_types)]
                struct get_mystic_number_token();
                impl PluginFunction for get_mystic_number_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 0usize,
                                            "wrong arg count: {} != {}", args.len(), 0usize);
                        Ok(Dynamic::from(get_mystic_number()))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(get_mystic_number_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![].into_boxed_slice()
                    }
                }
                pub fn get_mystic_number_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(get_mystic_number_token())
                }
                pub fn get_mystic_number_token_input_types() -> Box<[TypeId]> {
                    get_mystic_number_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_single_arg_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn add_one_to(x: INT) -> INT {
                    x + 1
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn add_one_to(x: INT) -> INT {
                    x + 1
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("add_one_to", FnAccess::Public, &[core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_one_to_token()));
                    m
                }
                #[allow(non_camel_case_types)]
                struct add_one_to_token();
                impl PluginFunction for add_one_to_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 1usize,
                                            "wrong arg count: {} != {}", args.len(), 1usize);
                        let arg0 = mem::take(args[0usize]).clone().cast::<INT>();
                        Ok(Dynamic::from(add_one_to(arg0)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(add_one_to_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<INT>()].into_boxed_slice()
                    }
                }
                pub fn add_one_to_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(add_one_to_token())
                }
                pub fn add_one_to_token_input_types() -> Box<[TypeId]> {
                    add_one_to_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_double_arg_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn add_together(x: INT, y: INT) -> INT {
                    x + y
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn add_together(x: INT, y: INT) -> INT {
                    x + y
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("add_together", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                                 core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_together_token()));
                    m
                }
                #[allow(non_camel_case_types)]
                struct add_together_token();
                impl PluginFunction for add_together_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 2usize,
                                            "wrong arg count: {} != {}", args.len(), 2usize);
                        let arg0 = mem::take(args[0usize]).clone().cast::<INT>();
                        let arg1 = mem::take(args[1usize]).clone().cast::<INT>();
                        Ok(Dynamic::from(add_together(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(add_together_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<INT>(),
                             TypeId::of::<INT>()].into_boxed_slice()
                    }
                }
                pub fn add_together_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(add_together_token())
                }
                pub fn add_together_token_input_types() -> Box<[TypeId]> {
                    add_together_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_constant_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                pub const MYSTIC_NUMBER: INT = 42;
            }
        };

        let expected_tokens = quote! {
            pub mod one_constant {
                pub const MYSTIC_NUMBER: INT = 42;
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_var("MYSTIC_NUMBER", 42);
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_constant_module_imports_preserved() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                pub use rhai::INT;
                pub const MYSTIC_NUMBER: INT = 42;
            }
        };

        let expected_tokens = quote! {
            pub mod one_constant {
                pub use rhai::INT;
                pub const MYSTIC_NUMBER: INT = 42;
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_var("MYSTIC_NUMBER", 42);
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_private_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                fn get_mystic_number() -> INT {
                    42
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                fn get_mystic_number() -> INT {
                    42
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_skipped_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(skip)]
                pub fn get_mystic_number() -> INT {
                    42
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn get_mystic_number() -> INT {
                    42
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_skipped_submodule() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub fn get_mystic_number() -> INT {
                    42
                }
                #[rhai_mod(skip)]
                pub mod inner_secrets {
                    pub const SECRET_NUMBER: INT = 86;
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn get_mystic_number() -> INT {
                    42
                }
                pub mod inner_secrets {
                    pub const SECRET_NUMBER: INT = 86;
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("get_mystic_number", FnAccess::Public, &[],
                             CallableFunction::from_plugin(get_mystic_number_token()));
                    m
                }
                #[allow(non_camel_case_types)]
                struct get_mystic_number_token();
                impl PluginFunction for get_mystic_number_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 0usize,
                                            "wrong arg count: {} != {}", args.len(), 0usize);
                        Ok(Dynamic::from(get_mystic_number()))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(get_mystic_number_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![].into_boxed_slice()
                    }
                }
                pub fn get_mystic_number_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(get_mystic_number_token())
                }
                pub fn get_mystic_number_token_input_types() -> Box<[TypeId]> {
                    get_mystic_number_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_private_constant_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                const MYSTIC_NUMBER: INT = 42;
            }
        };

        let expected_tokens = quote! {
            pub mod one_constant {
                const MYSTIC_NUMBER: INT = 42;
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_str_arg_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod str_fn {
                pub fn print_out_to(x: &str) {
                    x + 1
                }
            }
        };

        let expected_tokens = quote! {
            pub mod str_fn {
                pub fn print_out_to(x: &str) {
                    x + 1
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("print_out_to", FnAccess::Public,
                             &[core::any::TypeId::of::<ImmutableString>()],
                             CallableFunction::from_plugin(print_out_to_token()));
                    m
                }
                #[allow(non_camel_case_types)]
                struct print_out_to_token();
                impl PluginFunction for print_out_to_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 1usize,
                                            "wrong arg count: {} != {}", args.len(), 1usize);
                        let arg0 = mem::take(args[0usize]).clone().cast::<ImmutableString>();
                        Ok(Dynamic::from(print_out_to(&arg0)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(print_out_to_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<ImmutableString>()].into_boxed_slice()
                    }
                }
                pub fn print_out_to_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(print_out_to_token())
                }
                pub fn print_out_to_token_input_types() -> Box<[TypeId]> {
                    print_out_to_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_mut_ref_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod ref_fn {
                pub fn increment(x: &mut FLOAT) {
                    *x += 1.0 as FLOAT;
                }
            }
        };

        let expected_tokens = quote! {
            pub mod ref_fn {
                pub fn increment(x: &mut FLOAT) {
                    *x += 1.0 as FLOAT;
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("increment", FnAccess::Public,
                             &[core::any::TypeId::of::<FLOAT>()],
                             CallableFunction::from_plugin(increment_token()));
                    m
                }
                #[allow(non_camel_case_types)]
                struct increment_token();
                impl PluginFunction for increment_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 1usize,
                                            "wrong arg count: {} != {}", args.len(), 1usize);
                        let arg0: &mut _ = &mut args[0usize].write_lock::<FLOAT>().unwrap();
                        Ok(Dynamic::from(increment(arg0)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(increment_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<FLOAT>()].into_boxed_slice()
                    }
                }
                pub fn increment_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(increment_token())
                }
                pub fn increment_token_input_types() -> Box<[TypeId]> {
                    increment_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_fn_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                pub mod it_is {
                    pub fn increment(x: &mut FLOAT) {
                        *x += 1.0 as FLOAT;
                    }
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub mod it_is {
                    pub fn increment(x: &mut FLOAT) {
                        *x += 1.0 as FLOAT;
                    }
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_fn("increment", FnAccess::Public,
                                 &[core::any::TypeId::of::<FLOAT>()],
                                 CallableFunction::from_plugin(increment_token()));
                        m
                    }
                    #[allow(non_camel_case_types)]
                    struct increment_token();
                    impl PluginFunction for increment_token {
                        fn call(&self,
                                args: &mut [&mut Dynamic], pos: Position
                        ) -> Result<Dynamic, Box<EvalAltResult>> {
                            debug_assert_eq!(args.len(), 1usize,
                                                "wrong arg count: {} != {}", args.len(), 1usize);
                            let arg0: &mut _ = &mut args[0usize].write_lock::<FLOAT>().unwrap();
                            Ok(Dynamic::from(increment(arg0)))
                        }

                        fn is_method_call(&self) -> bool { true }
                        fn is_varadic(&self) -> bool { false }
                        fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                            Box::new(increment_token())
                        }
                        fn input_types(&self) -> Box<[TypeId]> {
                            new_vec![TypeId::of::<FLOAT>()].into_boxed_slice()
                        }
                    }
                    pub fn increment_token_callable() -> CallableFunction {
                        CallableFunction::from_plugin(increment_token())
                    }
                    pub fn increment_token_input_types() -> Box<[TypeId]> {
                        increment_token().input_types()
                    }
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    { m.set_sub_module("it_is", self::it_is::rhai_module_generate()); }
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_fn_with_cfg_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[cfg(not(feature = "no_float"))]
                pub mod it_is {
                    pub fn increment(x: &mut FLOAT) {
                        *x += 1.0 as FLOAT;
                    }
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                #[cfg(not(feature = "no_float"))]
                pub mod it_is {
                    pub fn increment(x: &mut FLOAT) {
                        *x += 1.0 as FLOAT;
                    }
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_fn("increment", FnAccess::Public,
                                 &[core::any::TypeId::of::<FLOAT>()],
                                 CallableFunction::from_plugin(increment_token()));
                        m
                    }
                    #[allow(non_camel_case_types)]
                    struct increment_token();
                    impl PluginFunction for increment_token {
                        fn call(&self,
                                args: &mut [&mut Dynamic], pos: Position
                        ) -> Result<Dynamic, Box<EvalAltResult>> {
                            debug_assert_eq!(args.len(), 1usize,
                                                "wrong arg count: {} != {}", args.len(), 1usize);
                            let arg0: &mut _ = &mut args[0usize].write_lock::<FLOAT>().unwrap();
                            Ok(Dynamic::from(increment(arg0)))
                        }

                        fn is_method_call(&self) -> bool { true }
                        fn is_varadic(&self) -> bool { false }
                        fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                            Box::new(increment_token())
                        }
                        fn input_types(&self) -> Box<[TypeId]> {
                            new_vec![TypeId::of::<FLOAT>()].into_boxed_slice()
                        }
                    }
                    pub fn increment_token_callable() -> CallableFunction {
                        CallableFunction::from_plugin(increment_token())
                    }
                    pub fn increment_token_input_types() -> Box<[TypeId]> {
                        increment_token().input_types()
                    }
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    #[cfg(not(feature = "no_float"))] {
                        m.set_sub_module("it_is", self::it_is::rhai_module_generate());
                    }
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_constant_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                pub mod it_is {
                    pub const MYSTIC_NUMBER: INT = 42;
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_constant {
                pub mod it_is {
                    pub const MYSTIC_NUMBER: INT = 42;
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_var("MYSTIC_NUMBER", 42);
                        m
                    }
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    { m.set_sub_module("it_is", self::it_is::rhai_module_generate()); }
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn dual_constant_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod two_constants {
                pub mod first_is {
                    pub const MYSTIC_NUMBER: INT = 42;
                }
                pub mod second_is {
                    pub const SPECIAL_CPU_NUMBER: INT = 68000;
                }
            }
        };

        let expected_tokens = quote! {
            pub mod two_constants {
                pub mod first_is {
                    pub const MYSTIC_NUMBER: INT = 42;
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_var("MYSTIC_NUMBER", 42);
                        m
                    }
                }
                pub mod second_is {
                    pub const SPECIAL_CPU_NUMBER: INT = 68000;
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_var("SPECIAL_CPU_NUMBER", 68000);
                        m
                    }
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    { m.set_sub_module("first_is", self::first_is::rhai_module_generate()); }
                    { m.set_sub_module("second_is", self::second_is::rhai_module_generate()); }
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn deep_tree_nested_module() {
        let input_tokens: TokenStream = quote! {
            pub mod heap_root {
                pub const VALUE: INT = 100;
                pub mod left {
                    pub const VALUE: INT = 19;
                    pub mod left {
                        pub const VALUE: INT = 17;
                        pub mod left {
                            pub const VALUE: INT = 2;
                        }
                        pub mod right {
                            pub const VALUE: INT = 7;
                        }
                    }
                    pub mod right {
                        pub const VALUE: INT = 3;
                    }
                }
                pub mod right {
                    pub const VALUE: INT = 36;
                    pub mod left {
                        pub const VALUE: INT = 25;
                    }
                    pub mod right {
                        pub const VALUE: INT = 1;
                    }
                }
            }
        };

        let expected_tokens = quote! {
            pub mod heap_root {
                pub const VALUE: INT = 100;
                pub mod left {
                    pub const VALUE: INT = 19;
                    pub mod left {
                        pub const VALUE: INT = 17;
                        pub mod left {
                            pub const VALUE: INT = 2;
                            #[allow(unused_imports)]
                            use super::*;
                            #[allow(unused_mut)]
                            pub fn rhai_module_generate() -> Module {
                                let mut m = Module::new();
                                m.set_var("VALUE", 2);
                                m
                            }
                        }
                        pub mod right {
                            pub const VALUE: INT = 7;
                            #[allow(unused_imports)]
                            use super::*;
                            #[allow(unused_mut)]
                            pub fn rhai_module_generate() -> Module {
                                let mut m = Module::new();
                                m.set_var("VALUE", 7);
                                m
                            }
                        }
                        #[allow(unused_imports)]
                        use super::*;
                        #[allow(unused_mut)]
                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            m.set_var("VALUE", 17);
                            { m.set_sub_module("left", self::left::rhai_module_generate()); }
                            { m.set_sub_module("right", self::right::rhai_module_generate()); }
                            m
                        }
                    }
                    pub mod right {
                        pub const VALUE: INT = 3;
                        #[allow(unused_imports)]
                        use super::*;
                        #[allow(unused_mut)]
                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            m.set_var("VALUE", 3);
                            m
                        }
                    }
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_var("VALUE", 19);
                        { m.set_sub_module("left", self::left::rhai_module_generate()); }
                        { m.set_sub_module("right", self::right::rhai_module_generate()); }
                        m
                    }
                }
                pub mod right {
                    pub const VALUE: INT = 36;
                    pub mod left {
                        pub const VALUE: INT = 25;
                        #[allow(unused_imports)]
                        use super::*;
                        #[allow(unused_mut)]
                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            m.set_var("VALUE", 25);
                            m
                        }
                    }
                    pub mod right {
                        pub const VALUE: INT = 1;
                        #[allow(unused_imports)]
                        use super::*;
                        #[allow(unused_mut)]
                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            m.set_var("VALUE", 1);
                            m
                        }
                    }
                    #[allow(unused_imports)]
                    use super::*;
                    #[allow(unused_mut)]
                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        m.set_var("VALUE", 36);
                        { m.set_sub_module("left", self::left::rhai_module_generate()); }
                        { m.set_sub_module("right", self::right::rhai_module_generate()); }
                        m
                    }
                }
                #[allow(unused_imports)]
                use super::*;
                #[allow(unused_mut)]
                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    m.set_var("VALUE", 100);
                    { m.set_sub_module("left", self::left::rhai_module_generate()); }
                    { m.set_sub_module("right", self::right::rhai_module_generate()); }
                    m
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }
}
