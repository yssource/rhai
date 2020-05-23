#![cfg(not(feature = "no_module"))]
use rhai::{module_resolvers, Engine, EvalAltResult, Module, Scope, INT};

#[test]
fn test_module() {
    let mut module = Module::new();
    module.set_var("answer", 42 as INT);

    assert!(module.contains_var("answer"));
    assert_eq!(module.get_var_value::<INT>("answer").unwrap(), 42);
}

#[test]
fn test_module_sub_module() -> Result<(), Box<EvalAltResult>> {
    let mut module = Module::new();

    let mut sub_module = Module::new();

    let mut sub_module2 = Module::new();
    sub_module2.set_var("answer", 41 as INT);
    let hash_inc = sub_module2.set_fn_1("inc", |x: INT| Ok(x + 1));

    sub_module.set_sub_module("universe", sub_module2);
    module.set_sub_module("life", sub_module);

    assert!(module.contains_sub_module("life"));
    let m = module.get_sub_module("life").unwrap();

    assert!(m.contains_sub_module("universe"));
    let m2 = m.get_sub_module("universe").unwrap();

    assert!(m2.contains_var("answer"));
    assert!(m2.contains_fn(hash_inc));

    assert_eq!(m2.get_var_value::<INT>("answer").unwrap(), 41);

    let engine = Engine::new();
    let mut scope = Scope::new();

    scope.push_module("question", module);

    assert_eq!(
        engine.eval_expression_with_scope::<INT>(
            &mut scope,
            "question::life::universe::answer + 1"
        )?,
        42
    );
    assert_eq!(
        engine.eval_expression_with_scope::<INT>(
            &mut scope,
            "question::life::universe::inc(question::life::universe::answer)"
        )?,
        42
    );

    Ok(())
}

#[test]
fn test_module_resolver() -> Result<(), Box<EvalAltResult>> {
    let mut resolver = module_resolvers::StaticModuleResolver::new();

    let mut module = Module::new();
    module.set_var("answer", 42 as INT);

    resolver.insert("hello".to_string(), module);

    let mut engine = Engine::new();
    engine.set_module_resolver(Some(resolver));

    assert_eq!(
        engine.eval::<INT>(
            r#"
                import "hello" as h1;
                import "hello" as h2;
                h2::answer
    "#
        )?,
        42
    );

    #[cfg(not(feature = "unchecked"))]
    {
        engine.set_max_modules(5);

        assert!(matches!(
            *engine
                .eval::<INT>(
                    r#"
                        let sum = 0;

                        for x in range(0, 10) {
                            import "hello" as h;
                            sum += h::answer;
                        }

                        sum
            "#
                )
                .expect_err("should error"),
            EvalAltResult::ErrorTooManyModules(_)
        ));

        #[cfg(not(feature = "no_function"))]
        assert!(matches!(
            *engine
                .eval::<INT>(
                    r#"
                        let sum = 0;

                        fn foo() {
                            import "hello" as h;
                            sum += h::answer;
                        }

                        for x in range(0, 10) {
                            foo();
                        }

                        sum
            "#
                )
                .expect_err("should error"),
            EvalAltResult::ErrorInFunctionCall(fn_name, _, _) if fn_name == "foo"
        ));

        engine.set_max_modules(0);

        #[cfg(not(feature = "no_function"))]
        engine.eval::<()>(
            r#"
                fn foo() {
                    import "hello" as h;
                }

                for x in range(0, 10) {
                    foo();
                }
    "#,
        )?;
    }

    Ok(())
}

#[test]
#[cfg(not(feature = "no_function"))]
fn test_module_from_ast() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    let mut resolver = rhai::module_resolvers::StaticModuleResolver::new();
    let mut sub_module = Module::new();
    sub_module.set_var("foo", true);
    resolver.insert("another module".to_string(), sub_module);

    engine.set_module_resolver(Some(resolver));

    let ast = engine.compile(
        r#"
        // Functions become module functions
        fn calc(x) {
            x + 1
        }
        fn add_len(x, y) {
            x + len(y)
        }
        private fn hidden() {
            throw "you shouldn't see me!";
        }
    
        // Imported modules become sub-modules
        import "another module" as extra;
    
        // Variables defined at global level become module variables
        const x = 123;
        let foo = 41;
        let hello;
    
        // Final variable values become constant module variable values
        foo = calc(foo);
        hello = "hello, " + foo + " worlds!";

        export
            x as abc,
            foo,
            hello,
            extra as foobar;
    "#,
    )?;

    let module = Module::eval_ast_as_new(Scope::new(), &ast, &engine)?;

    let mut scope = Scope::new();
    scope.push_module("testing", module);

    assert_eq!(
        engine.eval_expression_with_scope::<INT>(&mut scope, "testing::abc")?,
        123
    );
    assert_eq!(
        engine.eval_expression_with_scope::<INT>(&mut scope, "testing::foo")?,
        42
    );
    assert!(engine.eval_expression_with_scope::<bool>(&mut scope, "testing::foobar::foo")?);
    assert_eq!(
        engine.eval_expression_with_scope::<String>(&mut scope, "testing::hello")?,
        "hello, 42 worlds!"
    );
    assert_eq!(
        engine.eval_expression_with_scope::<INT>(&mut scope, "testing::calc(999)")?,
        1000
    );
    assert_eq!(
        engine.eval_expression_with_scope::<INT>(
            &mut scope,
            "testing::add_len(testing::foo, testing::hello)"
        )?,
        59
    );
    assert!(matches!(
        *engine
            .eval_expression_with_scope::<()>(&mut scope, "testing::hidden()")
            .expect_err("should error"),
        EvalAltResult::ErrorFunctionNotFound(fn_name, _) if fn_name == "hidden"
    ));

    Ok(())
}
