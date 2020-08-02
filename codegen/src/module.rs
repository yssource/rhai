use quote::{quote, ToTokens};
use syn::{parse::Parse, parse::ParseStream};

use crate::function::ExportedFn;
use crate::rhai_module::ExportedConst;

#[derive(Debug)]
pub(crate) struct Module {
    mod_all: Option<syn::ItemMod>,
    fns: Vec<ExportedFn>,
    consts: Vec<ExportedConst>,
}

impl Parse for Module {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mod_all: syn::ItemMod = input.parse()?;
        let fns: Vec<_>;
        let consts: Vec<_>;
        if let Some((_, ref content)) = mod_all.content {
            fns = content
                .iter()
                .filter_map(|item| match item {
                    syn::Item::Fn(f) => {
                        if let syn::Visibility::Public(_) = f.vis {
                            Some(f)
                        } else {
                            None
                        }
                    }
                    _ => None,
                })
                .try_fold(Vec::new(), |mut vec, itemfn| {
                    syn::parse2::<ExportedFn>(itemfn.to_token_stream())
                        .map(|f| vec.push(f))
                        .map(|_| vec)
                })?;
            consts = content
                .iter()
                .filter_map(|item| match item {
                    syn::Item::Const(syn::ItemConst {
                        vis,
                        ref expr,
                        ident,
                        ..
                    }) => {
                        if let syn::Visibility::Public(_) = vis {
                            Some((ident.to_string(), expr.as_ref().clone()))
                        } else {
                            None
                        }
                    }
                    _ => None,
                })
                .collect();
        } else {
            consts = vec![];
            fns = vec![];
        }
        Ok(Module {
            mod_all: Some(mod_all),
            fns,
            consts,
        })
    }
}

impl Module {
    pub fn generate(self) -> proc_macro2::TokenStream {
        let mod_gen = crate::rhai_module::generate_body(&self.fns, &self.consts);
        let Module { mod_all, .. } = self;
        let mut mod_all = mod_all.unwrap();
        let mod_name = mod_all.ident.clone();
        let (_, orig_content) = mod_all.content.take().unwrap();

        quote! {
            pub mod #mod_name {
                #(#orig_content)*
                #mod_gen
            }
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("get_mystic_number", FnAccess::Public, &[],
                             rhai::plugin::CallableFunction::from_plugin(get_mystic_number__Token()));
                    m
                }
                #[allow(non_camel_case_types)]
                pub struct get_mystic_number__Token();
                impl rhai::plugin::PluginFunction for get_mystic_number__Token {
                    fn call(&self,
                            args: &mut [&mut rhai::Dynamic], pos: rhai::Position
                    ) -> Result<rhai::Dynamic, Box<rhai::EvalAltResult>> {
                        if args.len() != 0usize {
                            return Err(Box::new(rhai::EvalAltResult::ErrorRuntime(
                                    format!("wrong arg count: {} != {}",
                                            args.len(), 0usize), rhai::Position::none())));
                        }
                        Ok(rhai::Dynamic::from(get_mystic_number()))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn rhai::plugin::PluginFunction> {
                        Box::new(get_mystic_number__Token())
                    }
                    fn input_types(&self) -> Box<[std::any::TypeId]> {
                        vec![].into_boxed_slice()
                    }
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("add_one_to", FnAccess::Public, &[core::any::TypeId::of::<INT>()],
                             rhai::plugin::CallableFunction::from_plugin(add_one_to__Token()));
                    m
                }
                #[allow(non_camel_case_types)]
                pub struct add_one_to__Token();
                impl rhai::plugin::PluginFunction for add_one_to__Token {
                    fn call(&self,
                            args: &mut [&mut rhai::Dynamic], pos: rhai::Position
                    ) -> Result<rhai::Dynamic, Box<rhai::EvalAltResult>> {
                        if args.len() != 1usize {
                            return Err(Box::new(rhai::EvalAltResult::ErrorRuntime(
                                    format!("wrong arg count: {} != {}",
                                            args.len(), 1usize), rhai::Position::none())));
                        }
                        let arg0 = args[0usize].downcast_clone::<INT>().unwrap();
                        Ok(rhai::Dynamic::from(add_one_to(arg0)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn rhai::plugin::PluginFunction> {
                        Box::new(add_one_to__Token())
                    }
                    fn input_types(&self) -> Box<[std::any::TypeId]> {
                        vec![std::any::TypeId::of::<INT>()].into_boxed_slice()
                    }
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("add_together", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                                 core::any::TypeId::of::<INT>()],
                             rhai::plugin::CallableFunction::from_plugin(add_together__Token()));
                    m
                }
                #[allow(non_camel_case_types)]
                pub struct add_together__Token();
                impl rhai::plugin::PluginFunction for add_together__Token {
                    fn call(&self,
                            args: &mut [&mut rhai::Dynamic], pos: rhai::Position
                    ) -> Result<rhai::Dynamic, Box<rhai::EvalAltResult>> {
                        if args.len() != 2usize {
                            return Err(Box::new(rhai::EvalAltResult::ErrorRuntime(
                                    format!("wrong arg count: {} != {}",
                                            args.len(), 2usize), rhai::Position::none())));
                        }
                        let arg0 = args[0usize].downcast_clone::<INT>().unwrap();
                        let arg1 = args[1usize].downcast_clone::<INT>().unwrap();
                        Ok(rhai::Dynamic::from(add_together(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn rhai::plugin::PluginFunction> {
                        Box::new(add_together__Token())
                    }
                    fn input_types(&self) -> Box<[std::any::TypeId]> {
                        vec![std::any::TypeId::of::<INT>(),
                             std::any::TypeId::of::<INT>()].into_boxed_slice()
                    }
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
                    let mut m = Module::new();
                    m
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("print_out_to", FnAccess::Public,
                             &[core::any::TypeId::of::<rhai::ImmutableString>()],
                             rhai::plugin::CallableFunction::from_plugin(print_out_to__Token()));
                    m
                }
                #[allow(non_camel_case_types)]
                pub struct print_out_to__Token();
                impl rhai::plugin::PluginFunction for print_out_to__Token {
                    fn call(&self,
                            args: &mut [&mut rhai::Dynamic], pos: rhai::Position
                    ) -> Result<rhai::Dynamic, Box<rhai::EvalAltResult>> {
                        if args.len() != 1usize {
                            return Err(Box::new(rhai::EvalAltResult::ErrorRuntime(
                                    format!("wrong arg count: {} != {}",
                                            args.len(), 1usize), rhai::Position::none())));
                        }
                        let arg0 = args[0usize].downcast_clone::<rhai::ImmutableString>().unwrap();
                        Ok(rhai::Dynamic::from(print_out_to(&arg0)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn rhai::plugin::PluginFunction> {
                        Box::new(print_out_to__Token())
                    }
                    fn input_types(&self) -> Box<[std::any::TypeId]> {
                        vec![std::any::TypeId::of::<rhai::ImmutableString>()].into_boxed_slice()
                    }
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
                use rhai::{Module, FnAccess};
                #[allow(unused_mut)]
                pub fn rhai_module__generate() -> Module {
                    let mut m = Module::new();
                    m.set_fn("increment", FnAccess::Public,
                             &[core::any::TypeId::of::<FLOAT>()],
                             rhai::plugin::CallableFunction::from_plugin(increment__Token()));
                    m
                }
                #[allow(non_camel_case_types)]
                pub struct increment__Token();
                impl rhai::plugin::PluginFunction for increment__Token {
                    fn call(&self,
                            args: &mut [&mut rhai::Dynamic], pos: rhai::Position
                    ) -> Result<rhai::Dynamic, Box<rhai::EvalAltResult>> {
                        if args.len() != 1usize {
                            return Err(Box::new(rhai::EvalAltResult::ErrorRuntime(
                                    format!("wrong arg count: {} != {}",
                                            args.len(), 1usize), rhai::Position::none())));
                        }
                        let arg0: &mut _ = args[0usize].downcast_mut::<FLOAT>().unwrap();
                        Ok(rhai::Dynamic::from(increment(arg0)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn rhai::plugin::PluginFunction> {
                        Box::new(increment__Token())
                    }
                    fn input_types(&self) -> Box<[std::any::TypeId]> {
                        vec![std::any::TypeId::of::<FLOAT>()].into_boxed_slice()
                    }
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }
}
