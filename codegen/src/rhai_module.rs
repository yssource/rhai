use quote::quote;

use crate::function::ExportedFn;

pub(crate) type ExportedConst = (String, syn::Expr);

pub(crate) fn generate_body(
    fns: &Vec<ExportedFn>,
    consts: &Vec<ExportedConst>,
) -> proc_macro2::TokenStream {
    let mut set_fn_stmts: Vec<syn::Stmt> = Vec::new();
    let mut set_const_stmts: Vec<syn::Stmt> = Vec::new();
    let str_type_path = syn::parse2::<syn::Path>(quote! { str }).unwrap();

    for (const_name, const_expr) in consts {
        let const_literal = syn::LitStr::new(&const_name, proc_macro2::Span::call_site());
        set_const_stmts.push(
            syn::parse2::<syn::Stmt>(quote! {
                m.set_var(#const_literal, #const_expr);
            })
            .unwrap(),
        );
    }

    // NB: these are token streams, because reparsing messes up "> >" vs ">>"
    let mut gen_fn_tokens: Vec<proc_macro2::TokenStream> = Vec::new();
    for function in fns {
        let fn_token_name = syn::Ident::new(
            &format!("{}_token", function.name().to_string()),
            function.name().span(),
        );
        let reg_name = function
            .params
            .name
            .clone()
            .unwrap_or_else(|| function.name().to_string());
        let fn_literal = syn::LitStr::new(&reg_name, proc_macro2::Span::call_site());
        let fn_input_types: Vec<syn::Expr> = function
            .arg_list()
            .map(|fnarg| match fnarg {
                syn::FnArg::Receiver(_) => panic!("internal error: receiver fn outside impl!?"),
                syn::FnArg::Typed(syn::PatType { ref ty, .. }) => {
                    let arg_type = match ty.as_ref() {
                        &syn::Type::Reference(syn::TypeReference {
                            mutability: None,
                            ref elem,
                            ..
                        }) => match elem.as_ref() {
                            &syn::Type::Path(ref p) if p.path == str_type_path => {
                                syn::parse2::<syn::Type>(quote! {
                                ImmutableString })
                                .unwrap()
                            }
                            _ => panic!("internal error: non-string shared reference!?"),
                        },
                        &syn::Type::Reference(syn::TypeReference {
                            mutability: Some(_),
                            ref elem,
                            ..
                        }) => match elem.as_ref() {
                            &syn::Type::Path(ref p) => syn::parse2::<syn::Type>(quote! {
                            #p })
                            .unwrap(),
                            _ => panic!("internal error: non-string shared reference!?"),
                        },
                        t => t.clone(),
                    };
                    syn::parse2::<syn::Expr>(quote! {
                    core::any::TypeId::of::<#arg_type>()})
                    .unwrap()
                }
            })
            .collect();

        set_fn_stmts.push(
            syn::parse2::<syn::Stmt>(quote! {
                m.set_fn(#fn_literal, FnAccess::Public, &[#(#fn_input_types),*],
                         CallableFunction::from_plugin(#fn_token_name()));
            })
            .unwrap(),
        );

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
            #[allow(unused_mut)]
            pub fn rhai_module_generate() -> Module {
                let mut m = Module::new();
                #(#set_fn_stmts)*
                #(#set_const_stmts)*
                m
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
