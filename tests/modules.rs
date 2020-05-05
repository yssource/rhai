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
    let hash = sub_module2.set_fn_1("inc", |x: INT| Ok(x + 1));

    sub_module.set_sub_module("universe", sub_module2);
    module.set_sub_module("life", sub_module);

    assert!(module.contains_sub_module("life"));
    let m = module.get_sub_module("life").unwrap();

    assert!(m.contains_sub_module("universe"));
    let m2 = m.get_sub_module("universe").unwrap();

    assert!(m2.contains_var("answer"));
    assert!(m2.contains_fn(hash));

    assert_eq!(m2.get_var_value::<INT>("answer").unwrap(), 41);

    let mut engine = Engine::new();
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

    resolver.add_module("hello", module);

    let mut engine = Engine::new();
    engine.set_module_resolver(resolver);

    assert_eq!(
        engine.eval::<INT>(
            r#"
                import "hello" as h;
                h::answer
    "#
        )?,
        42
    );

    Ok(())
}
