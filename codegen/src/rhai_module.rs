use std::collections::HashMap;

use quote::{quote, ToTokens};

use crate::attrs::ExportScope;
use crate::function::flatten_type_groups;
use crate::function::{ExportedFn, FnSpecialAccess};
use crate::module::Module;

pub(crate) type ExportedConst = (String, Box<syn::Type>, syn::Expr);

pub(crate) fn generate_body(
    fns: &mut [ExportedFn],
    consts: &[ExportedConst],
    submodules: &mut [Module],
    parent_scope: &ExportScope,
) -> proc_macro2::TokenStream {
    let mut set_fn_stmts: Vec<syn::Stmt> = Vec::new();
    let mut set_const_stmts: Vec<syn::Stmt> = Vec::new();
    let mut add_mod_blocks: Vec<syn::ExprBlock> = Vec::new();
    let mut set_flattened_mod_blocks: Vec<syn::ExprBlock> = Vec::new();
    let str_type_path = syn::parse2::<syn::Path>(quote! { str }).unwrap();
    let string_type_path = syn::parse2::<syn::Path>(quote! { String }).unwrap();

    for (const_name, _, _) in consts {
        let const_literal = syn::LitStr::new(&const_name, proc_macro2::Span::call_site());
        let const_ref = syn::Ident::new(&const_name, proc_macro2::Span::call_site());
        set_const_stmts.push(
            syn::parse2::<syn::Stmt>(quote! {
                m.set_var(#const_literal, #const_ref);
            })
            .unwrap(),
        );
    }

    for itemmod in submodules {
        itemmod.update_scope(&parent_scope);
        if itemmod.skipped() {
            continue;
        }
        let module_name = itemmod.module_name().unwrap();
        let exported_name: syn::LitStr = if let Some(name) = itemmod.exported_name() {
            syn::LitStr::new(&name, proc_macro2::Span::call_site())
        } else {
            syn::LitStr::new(&module_name.to_string(), proc_macro2::Span::call_site())
        };
        let cfg_attrs: Vec<&syn::Attribute> = itemmod
            .attrs()
            .unwrap()
            .iter()
            .filter(|&a| a.path.get_ident().map(|i| *i == "cfg").unwrap_or(false))
            .collect();
        add_mod_blocks.push(
            syn::parse2::<syn::ExprBlock>(quote! {
                #(#cfg_attrs)* {
                    m.set_sub_module(#exported_name, self::#module_name::rhai_module_generate());
                }
            })
            .unwrap(),
        );
        set_flattened_mod_blocks.push(
            syn::parse2::<syn::ExprBlock>(quote! {
                #(#cfg_attrs)* {
                    self::#module_name::rhai_generate_into_module(m, flatten);
                }
            })
            .unwrap(),
        );
    }

    // NB: these are token streams, because re-parsing messes up "> >" vs ">>"
    let mut gen_fn_tokens: Vec<proc_macro2::TokenStream> = Vec::new();
    for function in fns {
        function.update_scope(&parent_scope);
        if function.skipped() {
            continue;
        }
        let fn_token_name = syn::Ident::new(
            &format!("{}_token", function.name().to_string()),
            function.name().span(),
        );
        let reg_names = function.exported_names();

        let fn_input_types: Vec<syn::Expr> = function
            .arg_list()
            .map(|fnarg| match fnarg {
                syn::FnArg::Receiver(_) => panic!("internal error: receiver fn outside impl!?"),
                syn::FnArg::Typed(syn::PatType { ref ty, .. }) => {
                    let arg_type = match flatten_type_groups(ty.as_ref()) {
                        syn::Type::Reference(syn::TypeReference {
                            mutability: None,
                            ref elem,
                            ..
                        }) => match flatten_type_groups(elem.as_ref()) {
                            syn::Type::Path(ref p) if p.path == str_type_path => {
                                syn::parse2::<syn::Type>(quote! {
                                ImmutableString })
                                .unwrap()
                            }
                            _ => panic!("internal error: non-string shared reference!?"),
                        },
                        syn::Type::Path(ref p) if p.path == string_type_path => {
                            syn::parse2::<syn::Type>(quote! {
                            ImmutableString })
                            .unwrap()
                        }
                        syn::Type::Reference(syn::TypeReference {
                            mutability: Some(_),
                            ref elem,
                            ..
                        }) => match flatten_type_groups(elem.as_ref()) {
                            syn::Type::Path(ref p) => syn::parse2::<syn::Type>(quote! {
                            #p })
                            .unwrap(),
                            _ => panic!("internal error: invalid mutable reference!?"),
                        },
                        t => t.clone(),
                    };
                    syn::parse2::<syn::Expr>(quote! {
                    core::any::TypeId::of::<#arg_type>()})
                    .unwrap()
                }
            })
            .collect();

        for fn_literal in reg_names {
            set_fn_stmts.push(
                syn::parse2::<syn::Stmt>(quote! {
                    m.set_fn(#fn_literal, FnAccess::Public, &[#(#fn_input_types),*],
                             #fn_token_name().into());
                })
                .unwrap(),
            );
        }

        gen_fn_tokens.push(quote! {
            #[allow(non_camel_case_types)]
            struct #fn_token_name();
        });
        gen_fn_tokens.push(function.generate_impl(&fn_token_name.to_string()));
        gen_fn_tokens.push(function.generate_callable(&fn_token_name.to_string()));
        gen_fn_tokens.push(function.generate_input_types(&fn_token_name.to_string()));
    }

    let mut generate_fncall = syn::parse2::<syn::ItemMod>(quote! {
        pub mod generate_info {
            #[allow(unused_imports)]
            use super::*;

            pub fn rhai_module_generate() -> Module {
                let mut m = Module::new();
                rhai_generate_into_module(&mut m, false);
                m
            }
            #[allow(unused_mut)]
            pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                #(#set_fn_stmts)*
                #(#set_const_stmts)*

                if flatten {
                    #(#set_flattened_mod_blocks)*
                } else {
                    #(#add_mod_blocks)*
                }
            }
        }
    })
    .unwrap();

    let (_, generate_call_content) = generate_fncall.content.take().unwrap();

    quote! {
        #(#generate_call_content)*
        #(#gen_fn_tokens)*
    }
}

pub(crate) fn check_rename_collisions(fns: &Vec<ExportedFn>) -> Result<(), syn::Error> {
    fn make_key(name: impl ToString, itemfn: &ExportedFn) -> String {
        itemfn
            .arg_list()
            .fold(name.to_string(), |mut argstr, fnarg| {
                let type_string: String = match fnarg {
                    syn::FnArg::Receiver(_) => unimplemented!("receiver rhai_fns not implemented"),
                    syn::FnArg::Typed(syn::PatType { ref ty, .. }) => {
                        ty.as_ref().to_token_stream().to_string()
                    }
                };
                argstr.push('.');
                argstr.push_str(&type_string);
                argstr
            })
    }

    let mut renames = HashMap::<String, proc_macro2::Span>::new();
    let mut fn_defs = HashMap::<String, proc_macro2::Span>::new();

    for itemfn in fns.iter() {
        if itemfn.params().name.is_some() || itemfn.params().special != FnSpecialAccess::None {
            let mut names = itemfn
                .params()
                .name
                .as_ref()
                .map(|v| v.iter().map(|n| (n.clone(), n.clone())).collect())
                .unwrap_or_else(|| Vec::new());

            if let Some((s, n, _)) = itemfn.params().special.get_fn_name() {
                names.push((s, n));
            }

            for (name, fn_name) in names {
                let current_span = itemfn.params().span.as_ref().unwrap();
                let key = make_key(&name, itemfn);
                if let Some(other_span) = renames.insert(key, *current_span) {
                    let mut err = syn::Error::new(
                        *current_span,
                        format!("duplicate Rhai signature for '{}'", &fn_name),
                    );
                    err.combine(syn::Error::new(
                        other_span,
                        format!("duplicated function renamed '{}'", &fn_name),
                    ));
                    return Err(err);
                }
            }
        } else {
            let ident = itemfn.name();
            if let Some(other_span) = fn_defs.insert(ident.to_string(), ident.span()) {
                let mut err = syn::Error::new(
                    ident.span(),
                    format!("duplicate function '{}'", ident.to_string()),
                );
                err.combine(syn::Error::new(
                    other_span,
                    format!("duplicated function '{}'", ident.to_string()),
                ));
                return Err(err);
            }
            let key = make_key(ident, itemfn);
            if let Some(fn_span) = renames.get(&key) {
                let mut err = syn::Error::new(
                    ident.span(),
                    format!("duplicate Rhai signature for '{}'", &ident),
                );
                err.combine(syn::Error::new(
                    *fn_span,
                    format!("duplicated function '{}'", &ident),
                ));
                return Err(err);
            }
        }
    }

    Ok(())
}
