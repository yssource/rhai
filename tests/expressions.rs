use rhai::{Engine, EvalAltResult, Scope, INT};

#[test]
fn test_expressions() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    scope.push("x", 10 as INT);

    assert_eq!(engine.eval_expression::<INT>("2 + (10 + 10) * 2")?, 42);
    assert_eq!(
        engine.eval_expression_with_scope::<INT>(&mut scope, "2 + (x + 10) * 2")?,
        42
    );

    assert!(engine.eval_expression::<()>("40 + 2;").is_err());
    assert!(engine.eval_expression::<()>("40 + { 2 }").is_err());
    assert!(engine.eval_expression::<()>("x = 42").is_err());
    assert!(engine.compile_expression("let x = 42").is_err());
    assert!(engine
        .eval_expression_with_scope::<INT>(&mut scope, "if x > 0 { 42 } else { 123 }")
        .is_err());

    Ok(())
}
