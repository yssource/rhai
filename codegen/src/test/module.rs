#[cfg(test)]
mod module_tests {
    use crate::module::Module;

    use proc_macro2::TokenStream;
    use quote::quote;

    #[test]
    fn empty_module() {
        let input_tokens: TokenStream = quote! {
            pub mod empty { }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns().is_empty());
        assert!(item_mod.consts().is_empty());
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
        assert!(item_mod.consts().is_empty());
        assert_eq!(item_mod.fns().len(), 1);
        assert_eq!(item_mod.fns()[0].name().to_string(), "get_mystic_number");
        assert_eq!(item_mod.fns()[0].arg_count(), 0);
        assert_eq!(
            item_mod.fns()[0].return_type().unwrap(),
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
        assert!(item_mod.consts().is_empty());
        assert_eq!(item_mod.fns().len(), 1);
        assert_eq!(item_mod.fns()[0].name().to_string(), "add_one_to");
        assert_eq!(item_mod.fns()[0].arg_count(), 1);
        assert_eq!(
            item_mod.fns()[0].arg_list().next().unwrap(),
            &syn::parse2::<syn::FnArg>(quote! { x: INT }).unwrap()
        );
        assert_eq!(
            item_mod.fns()[0].return_type().unwrap(),
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
        let mut args = item_mod.fns()[0].arg_list();
        assert!(item_mod.consts().is_empty());
        assert_eq!(item_mod.fns().len(), 1);
        assert_eq!(item_mod.fns()[0].name().to_string(), "add_together");
        assert_eq!(item_mod.fns()[0].arg_count(), 2);
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
            item_mod.fns()[0].return_type().unwrap(),
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
        assert!(item_mod.fns().is_empty());
        assert!(item_mod.consts().is_empty());
        assert_eq!(item_mod.submodules().len(), 1);
        assert_eq!(&item_mod.submodules()[0].consts()[0].0, "MYSTIC_NUMBER");
        assert_eq!(
            item_mod.submodules()[0].consts()[0].2,
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
        assert!(item_mod.fns().is_empty());
        assert!(item_mod.consts().is_empty());
        assert_eq!(item_mod.submodules().len(), 1);
        assert_eq!(item_mod.submodules()[0].fns().len(), 1);
        assert!(item_mod.submodules()[0].fns()[0].skipped());
        assert!(item_mod.submodules()[0].consts().is_empty());
        assert!(item_mod.submodules()[0].submodules().is_empty());
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
        assert!(item_mod.fns().is_empty());
        assert!(item_mod.consts().is_empty());
        assert_eq!(item_mod.submodules().len(), 1);
        assert!(item_mod.submodules()[0].skipped());
    }

    #[test]
    fn one_constant_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                pub const MYSTIC_NUMBER: INT = 42;
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns().is_empty());
        assert_eq!(item_mod.consts().len(), 1);
        assert_eq!(&item_mod.consts()[0].0, "MYSTIC_NUMBER");
        assert_eq!(
            item_mod.consts()[0].2,
            syn::parse2::<syn::Expr>(quote! { 42 }).unwrap()
        );
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
        assert_eq!(item_mod.fns().len(), 1);
        assert!(item_mod.fns()[0].skipped());
        assert!(item_mod.consts().is_empty());
    }

    #[test]
    fn one_private_constant_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                const MYSTIC_NUMBER: INT = 42;
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert!(item_mod.fns().is_empty());
        assert!(item_mod.consts().is_empty());
    }
}

#[cfg(test)]
mod generate_tests {
    use crate::module::Module;

    use proc_macro2::TokenStream;
    use quote::quote;

    fn assert_streams_eq(actual: TokenStream, expected: TokenStream) {
        let actual = actual.to_string();
        let expected = expected.to_string();
        if &actual != &expected {
            let mut counter = 0;
            let _iter = actual.chars().zip(expected.chars()).skip_while(|(a, e)| {
                if *a == *e {
                    counter += 1;
                    true
                } else {
                    false
                }
            });
            /*
            let (actual_diff, expected_diff) = {
                let mut actual_diff = String::new();
                let mut expected_diff = String::new();
                for (a, e) in _iter.take(50) {
                    actual_diff.push(a);
                    expected_diff.push(e);
                }
                (actual_diff, expected_diff)
            };
            */
            eprintln!("actual != expected, diverge at char {}", counter);
            /*
            eprintln!("  actual: {}", actual_diff);
            eprintln!("expected: {}", expected_diff);
            assert!(false);
            */
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("get_mystic_number", FnAccess::Public, &[],
                             CallableFunction::from_plugin(get_mystic_number_token()));
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("add_one_to", FnAccess::Public, &[core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_one_to_token()));
                    if flatten {} else {}
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
    fn two_fn_overload_module() {
        let input_tokens: TokenStream = quote! {
            pub mod two_fns {
                #[rhai_fn(name = "add_n")]
                pub fn add_one_to(x: INT) -> INT {
                    x + 1
                }

                #[rhai_fn(name = "add_n")]
                pub fn add_n_to(x: INT, y: INT) -> INT {
                    x + y
                }
            }
        };

        let expected_tokens = quote! {
            pub mod two_fns {
                pub fn add_one_to(x: INT) -> INT {
                    x + 1
                }

                pub fn add_n_to(x: INT, y: INT) -> INT {
                    x + y
                }

                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("add_n", FnAccess::Public, &[core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_one_to_token()));
                    m.set_fn("add_n", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                          core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_n_to_token()));
                    if flatten {} else {}
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

                #[allow(non_camel_case_types)]
                struct add_n_to_token();
                impl PluginFunction for add_n_to_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 2usize,
                                            "wrong arg count: {} != {}", args.len(), 2usize);
                        let arg0 = mem::take(args[0usize]).clone().cast::<INT>();
                        let arg1 = mem::take(args[1usize]).clone().cast::<INT>();
                        Ok(Dynamic::from(add_n_to(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { false }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(add_n_to_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<INT>(),
                                 TypeId::of::<INT>()].into_boxed_slice()
                    }
                }
                pub fn add_n_to_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(add_n_to_token())
                }
                pub fn add_n_to_token_input_types() -> Box<[TypeId]> {
                    add_n_to_token().input_types()
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("add_together", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                                 core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_together_token()));
                    if flatten {} else {}
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
    fn one_double_rename_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(name = "add", name = "+", name = "add_together")]
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("add", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                                 core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_together_token()));
                    m.set_fn("+", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                                 core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_together_token()));
                    m.set_fn("add_together", FnAccess::Public, &[core::any::TypeId::of::<INT>(),
                                                                 core::any::TypeId::of::<INT>()],
                             CallableFunction::from_plugin(add_together_token()));
                    if flatten {} else {}
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
    fn one_constant_type_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_constant {
                #[derive(Debug, Clone)]
                pub struct Foo(pub INT);

                pub const MYSTIC_NUMBER: Foo = Foo(42);
            }
        };

        let expected_tokens = quote! {
            pub mod one_constant {
                #[derive(Debug, Clone)]
                pub struct Foo(pub INT);

                pub const MYSTIC_NUMBER: Foo = Foo(42);
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_var("MYSTIC_NUMBER", MYSTIC_NUMBER);
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_var("MYSTIC_NUMBER", MYSTIC_NUMBER);
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_var("MYSTIC_NUMBER", MYSTIC_NUMBER);
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("get_mystic_number", FnAccess::Public, &[],
                             CallableFunction::from_plugin(get_mystic_number_token()));
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("print_out_to", FnAccess::Public,
                             &[core::any::TypeId::of::<ImmutableString>()],
                             CallableFunction::from_plugin(print_out_to_token()));
                    if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("increment", FnAccess::Public,
                             &[core::any::TypeId::of::<FLOAT>()],
                             CallableFunction::from_plugin(increment_token()));
                    if flatten {} else {}
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

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_fn("increment", FnAccess::Public,
                                 &[core::any::TypeId::of::<FLOAT>()],
                                 CallableFunction::from_plugin(increment_token()));
                        if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {
                        { self::it_is::rhai_generate_into_module(m, flatten); }
                    } else {
                        { m.set_sub_module("it_is", self::it_is::rhai_module_generate()); }
                    }
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

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_fn("increment", FnAccess::Public,
                                 &[core::any::TypeId::of::<FLOAT>()],
                                 CallableFunction::from_plugin(increment_token()));
                        if flatten {} else {}
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

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {
                        #[cfg(not(feature = "no_float"))] {
                            self::it_is::rhai_generate_into_module(m, flatten);
                        }
                    } else {
                        #[cfg(not(feature = "no_float"))] {
                            m.set_sub_module("it_is", self::it_is::rhai_module_generate());
                        }
                    }
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_getter_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(get = "square")]
                pub fn int_foo(x: &mut u64) -> u64 {
                    (*x) * (*x)
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn int_foo(x: &mut u64) -> u64 {
                    (*x) * (*x)
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("get$square", FnAccess::Public, &[core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(int_foo_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct int_foo_token();
                impl PluginFunction for int_foo_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 1usize,
                                            "wrong arg count: {} != {}", args.len(), 1usize);
                        let arg0: &mut _ = &mut args[0usize].write_lock::<u64>().unwrap();
                        Ok(Dynamic::from(int_foo(arg0)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(int_foo_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<u64>()].into_boxed_slice()
                    }
                }
                pub fn int_foo_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(int_foo_token())
                }
                pub fn int_foo_token_input_types() -> Box<[TypeId]> {
                    int_foo_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_getter_and_rename_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(name = "square", get = "square")]
                pub fn int_foo(x: &mut u64) -> u64 {
                    (*x) * (*x)
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn int_foo(x: &mut u64) -> u64 {
                    (*x) * (*x)
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("square", FnAccess::Public, &[core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(int_foo_token()));
                    m.set_fn("get$square", FnAccess::Public, &[core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(int_foo_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct int_foo_token();
                impl PluginFunction for int_foo_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 1usize,
                                            "wrong arg count: {} != {}", args.len(), 1usize);
                        let arg0: &mut _ = &mut args[0usize].write_lock::<u64>().unwrap();
                        Ok(Dynamic::from(int_foo(arg0)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(int_foo_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<u64>()].into_boxed_slice()
                    }
                }
                pub fn int_foo_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(int_foo_token())
                }
                pub fn int_foo_token_input_types() -> Box<[TypeId]> {
                    int_foo_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_setter_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(set = "squared")]
                pub fn int_foo(x: &mut u64, y: u64) {
                    *x = y * y
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn int_foo(x: &mut u64, y: u64) {
                    *x = y * y
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("set$squared", FnAccess::Public,
                             &[core::any::TypeId::of::<u64>(),
                               core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(int_foo_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct int_foo_token();
                impl PluginFunction for int_foo_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 2usize,
                                            "wrong arg count: {} != {}", args.len(), 2usize);
                        let arg1 = mem::take(args[1usize]).clone().cast::<u64>();
                        let arg0: &mut _ = &mut args[0usize].write_lock::<u64>().unwrap();
                        Ok(Dynamic::from(int_foo(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(int_foo_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<u64>(), TypeId::of::<u64>()].into_boxed_slice()
                    }
                }
                pub fn int_foo_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(int_foo_token())
                }
                pub fn int_foo_token_input_types() -> Box<[TypeId]> {
                    int_foo_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_setter_and_rename_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_fn {
                #[rhai_fn(name = "set_sq", set = "squared")]
                pub fn int_foo(x: &mut u64, y: u64) {
                    *x = y * y
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_fn {
                pub fn int_foo(x: &mut u64, y: u64) {
                    *x = y * y
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("set_sq", FnAccess::Public,
                             &[core::any::TypeId::of::<u64>(),
                               core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(int_foo_token()));
                    m.set_fn("set$squared", FnAccess::Public,
                             &[core::any::TypeId::of::<u64>(),
                               core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(int_foo_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct int_foo_token();
                impl PluginFunction for int_foo_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 2usize,
                                            "wrong arg count: {} != {}", args.len(), 2usize);
                        let arg1 = mem::take(args[1usize]).clone().cast::<u64>();
                        let arg0: &mut _ = &mut args[0usize].write_lock::<u64>().unwrap();
                        Ok(Dynamic::from(int_foo(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(int_foo_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<u64>(), TypeId::of::<u64>()].into_boxed_slice()
                    }
                }
                pub fn int_foo_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(int_foo_token())
                }
                pub fn int_foo_token_input_types() -> Box<[TypeId]> {
                    int_foo_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_index_getter_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_index_fn {
                #[rhai_fn(index_get)]
                pub fn get_by_index(x: &mut MyCollection, i: u64) -> FLOAT {
                    x.get(i)
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_index_fn {
                pub fn get_by_index(x: &mut MyCollection, i: u64) -> FLOAT {
                    x.get(i)
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("index$get$", FnAccess::Public,
                             &[core::any::TypeId::of::<MyCollection>(),
                               core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(get_by_index_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct get_by_index_token();
                impl PluginFunction for get_by_index_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 2usize,
                                            "wrong arg count: {} != {}", args.len(), 2usize);
                        let arg1 = mem::take(args[1usize]).clone().cast::<u64>();
                        let arg0: &mut _ = &mut args[0usize].write_lock::<MyCollection>().unwrap();
                        Ok(Dynamic::from(get_by_index(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(get_by_index_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<MyCollection>(),
                                 TypeId::of::<u64>()].into_boxed_slice()
                    }
                }
                pub fn get_by_index_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(get_by_index_token())
                }
                pub fn get_by_index_token_input_types() -> Box<[TypeId]> {
                    get_by_index_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_index_getter_and_rename_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_index_fn {
                #[rhai_fn(name = "get", index_get)]
                pub fn get_by_index(x: &mut MyCollection, i: u64) -> FLOAT {
                    x.get(i)
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_index_fn {
                pub fn get_by_index(x: &mut MyCollection, i: u64) -> FLOAT {
                    x.get(i)
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("get", FnAccess::Public,
                             &[core::any::TypeId::of::<MyCollection>(),
                               core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(get_by_index_token()));
                    m.set_fn("index$get$", FnAccess::Public,
                             &[core::any::TypeId::of::<MyCollection>(),
                               core::any::TypeId::of::<u64>()],
                             CallableFunction::from_plugin(get_by_index_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct get_by_index_token();
                impl PluginFunction for get_by_index_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 2usize,
                                            "wrong arg count: {} != {}", args.len(), 2usize);
                        let arg1 = mem::take(args[1usize]).clone().cast::<u64>();
                        let arg0: &mut _ = &mut args[0usize].write_lock::<MyCollection>().unwrap();
                        Ok(Dynamic::from(get_by_index(arg0, arg1)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(get_by_index_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<MyCollection>(),
                                 TypeId::of::<u64>()].into_boxed_slice()
                    }
                }
                pub fn get_by_index_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(get_by_index_token())
                }
                pub fn get_by_index_token_input_types() -> Box<[TypeId]> {
                    get_by_index_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_index_setter_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_index_fn {
                #[rhai_fn(index_set)]
                pub fn set_by_index(x: &mut MyCollection, i: u64, item: FLOAT) {
                    x.entry(i).set(item)
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_index_fn {
                pub fn set_by_index(x: &mut MyCollection, i: u64, item: FLOAT) {
                    x.entry(i).set(item)
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("index$set$", FnAccess::Public,
                             &[core::any::TypeId::of::<MyCollection>(),
                               core::any::TypeId::of::<u64>(),
                               core::any::TypeId::of::<FLOAT>()],
                             CallableFunction::from_plugin(set_by_index_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct set_by_index_token();
                impl PluginFunction for set_by_index_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 3usize,
                                            "wrong arg count: {} != {}", args.len(), 3usize);
                        let arg1 = mem::take(args[1usize]).clone().cast::<u64>();
                        let arg2 = mem::take(args[2usize]).clone().cast::<FLOAT>();
                        let arg0: &mut _ = &mut args[0usize].write_lock::<MyCollection>().unwrap();
                        Ok(Dynamic::from(set_by_index(arg0, arg1, arg2)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(set_by_index_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<MyCollection>(),
                                 TypeId::of::<u64>(),
                                 TypeId::of::<FLOAT>()].into_boxed_slice()
                    }
                }
                pub fn set_by_index_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(set_by_index_token())
                }
                pub fn set_by_index_token_input_types() -> Box<[TypeId]> {
                    set_by_index_token().input_types()
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }

    #[test]
    fn one_index_setter_and_rename_fn_module() {
        let input_tokens: TokenStream = quote! {
            pub mod one_index_fn {
                #[rhai_fn(name = "set", index_set)]
                pub fn set_by_index(x: &mut MyCollection, i: u64, item: FLOAT) {
                    x.entry(i).set(item)
                }
            }
        };

        let expected_tokens = quote! {
            pub mod one_index_fn {
                pub fn set_by_index(x: &mut MyCollection, i: u64, item: FLOAT) {
                    x.entry(i).set(item)
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_fn("set", FnAccess::Public,
                             &[core::any::TypeId::of::<MyCollection>(),
                               core::any::TypeId::of::<u64>(),
                               core::any::TypeId::of::<FLOAT>()],
                             CallableFunction::from_plugin(set_by_index_token()));
                    m.set_fn("index$set$", FnAccess::Public,
                             &[core::any::TypeId::of::<MyCollection>(),
                               core::any::TypeId::of::<u64>(),
                               core::any::TypeId::of::<FLOAT>()],
                             CallableFunction::from_plugin(set_by_index_token()));
                    if flatten {} else {}
                }
                #[allow(non_camel_case_types)]
                struct set_by_index_token();
                impl PluginFunction for set_by_index_token {
                    fn call(&self,
                            args: &mut [&mut Dynamic], pos: Position
                    ) -> Result<Dynamic, Box<EvalAltResult>> {
                        debug_assert_eq!(args.len(), 3usize,
                                            "wrong arg count: {} != {}", args.len(), 3usize);
                        let arg1 = mem::take(args[1usize]).clone().cast::<u64>();
                        let arg2 = mem::take(args[2usize]).clone().cast::<FLOAT>();
                        let arg0: &mut _ = &mut args[0usize].write_lock::<MyCollection>().unwrap();
                        Ok(Dynamic::from(set_by_index(arg0, arg1, arg2)))
                    }

                    fn is_method_call(&self) -> bool { true }
                    fn is_varadic(&self) -> bool { false }
                    fn clone_boxed(&self) -> Box<dyn PluginFunction> {
                        Box::new(set_by_index_token())
                    }
                    fn input_types(&self) -> Box<[TypeId]> {
                        new_vec![TypeId::of::<MyCollection>(),
                                 TypeId::of::<u64>(),
                                 TypeId::of::<FLOAT>()].into_boxed_slice()
                    }
                }
                pub fn set_by_index_token_callable() -> CallableFunction {
                    CallableFunction::from_plugin(set_by_index_token())
                }
                pub fn set_by_index_token_input_types() -> Box<[TypeId]> {
                    set_by_index_token().input_types()
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

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_var("MYSTIC_NUMBER", MYSTIC_NUMBER);
                        if flatten {} else {}
                    }
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {
                        { self::it_is::rhai_generate_into_module(m, flatten); }
                    } else {
                        { m.set_sub_module("it_is", self::it_is::rhai_module_generate()); }
                    }
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

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_var("MYSTIC_NUMBER", MYSTIC_NUMBER);
                        if flatten {} else {}
                    }
                }
                pub mod second_is {
                    pub const SPECIAL_CPU_NUMBER: INT = 68000;
                    #[allow(unused_imports)]
                    use super::*;

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_var("SPECIAL_CPU_NUMBER", SPECIAL_CPU_NUMBER);
                        if flatten {} else {}
                    }
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    if flatten {
                        { self::first_is::rhai_generate_into_module(m, flatten); }
                        { self::second_is::rhai_generate_into_module(m, flatten); }
                    } else {
                        { m.set_sub_module("first_is", self::first_is::rhai_module_generate()); }
                        { m.set_sub_module("second_is", self::second_is::rhai_module_generate()); }
                    }
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

                            pub fn rhai_module_generate() -> Module {
                                let mut m = Module::new();
                                rhai_generate_into_module(&mut m, false);
                                m
                            }
                            #[allow(unused_mut)]
                            pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                                m.set_var("VALUE", VALUE);
                                if flatten {} else {}
                            }
                        }
                        pub mod right {
                            pub const VALUE: INT = 7;
                            #[allow(unused_imports)]
                            use super::*;

                            pub fn rhai_module_generate() -> Module {
                                let mut m = Module::new();
                                rhai_generate_into_module(&mut m, false);
                                m
                            }
                            #[allow(unused_mut)]
                            pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                                m.set_var("VALUE", VALUE);
                                if flatten {} else {}
                            }
                        }
                        #[allow(unused_imports)]
                        use super::*;

                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            rhai_generate_into_module(&mut m, false);
                            m
                        }
                        #[allow(unused_mut)]
                        pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                            m.set_var("VALUE", VALUE);

                            if flatten {
                                { self::left::rhai_generate_into_module(m, flatten); }
                                { self::right::rhai_generate_into_module(m, flatten); }
                            } else {
                                { m.set_sub_module("left", self::left::rhai_module_generate()); }
                                { m.set_sub_module("right", self::right::rhai_module_generate()); }
                            }
                        }
                    }
                    pub mod right {
                        pub const VALUE: INT = 3;
                        #[allow(unused_imports)]
                        use super::*;

                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            rhai_generate_into_module(&mut m, false);
                            m
                        }
                        #[allow(unused_mut)]
                        pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                            m.set_var("VALUE", VALUE);
                            if flatten {} else {}
                        }
                    }
                    #[allow(unused_imports)]
                    use super::*;

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_var("VALUE", VALUE);

                        if flatten {
                            { self::left::rhai_generate_into_module(m, flatten); }
                            { self::right::rhai_generate_into_module(m, flatten); }
                        } else {
                            { m.set_sub_module("left", self::left::rhai_module_generate()); }
                            { m.set_sub_module("right", self::right::rhai_module_generate()); }
                        }
                    }
                }
                pub mod right {
                    pub const VALUE: INT = 36;
                    pub mod left {
                        pub const VALUE: INT = 25;
                        #[allow(unused_imports)]
                        use super::*;

                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            rhai_generate_into_module(&mut m, false);
                            m
                        }
                        #[allow(unused_mut)]
                        pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                            m.set_var("VALUE", VALUE);
                            if flatten {} else {}
                        }
                    }
                    pub mod right {
                        pub const VALUE: INT = 1;
                        #[allow(unused_imports)]
                        use super::*;

                        pub fn rhai_module_generate() -> Module {
                            let mut m = Module::new();
                            rhai_generate_into_module(&mut m, false);
                            m
                        }
                        #[allow(unused_mut)]
                        pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                            m.set_var("VALUE", VALUE);
                            if flatten {} else {}
                        }
                    }
                    #[allow(unused_imports)]
                    use super::*;

                    pub fn rhai_module_generate() -> Module {
                        let mut m = Module::new();
                        rhai_generate_into_module(&mut m, false);
                        m
                    }
                    #[allow(unused_mut)]
                    pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                        m.set_var("VALUE", VALUE);

                        if flatten {
                            { self::left::rhai_generate_into_module(m, flatten); }
                            { self::right::rhai_generate_into_module(m, flatten); }
                        } else {
                            { m.set_sub_module("left", self::left::rhai_module_generate()); }
                            { m.set_sub_module("right", self::right::rhai_module_generate()); }
                        }
                    }
                }
                #[allow(unused_imports)]
                use super::*;

                pub fn rhai_module_generate() -> Module {
                    let mut m = Module::new();
                    rhai_generate_into_module(&mut m, false);
                    m
                }
                #[allow(unused_mut)]
                pub fn rhai_generate_into_module(m: &mut Module, flatten: bool) {
                    m.set_var("VALUE", VALUE);

                    if flatten {
                        { self::left::rhai_generate_into_module(m, flatten); }
                        { self::right::rhai_generate_into_module(m, flatten); }
                    } else {
                        { m.set_sub_module("left", self::left::rhai_module_generate()); }
                        { m.set_sub_module("right", self::right::rhai_module_generate()); }
                    }
                }
            }
        };

        let item_mod = syn::parse2::<Module>(input_tokens).unwrap();
        assert_streams_eq(item_mod.generate(), expected_tokens);
    }
}
