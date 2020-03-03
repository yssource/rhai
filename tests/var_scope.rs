use rhai::{Engine, EvalAltResult, Scope};

#[test]
fn test_var_scope() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    engine.eval_with_scope::<()>(&mut scope, "let x = 4 + 5")?;
    assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x")?, 9);
    engine.eval_with_scope::<()>(&mut scope, "x = x + 1; x = x + 2;")?;
    assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x")?, 12);
    assert_eq!(engine.eval_with_scope::<()>(&mut scope, "{let x = 3}")?, ());
    assert_eq!(engine.eval_with_scope::<i64>(&mut scope, "x")?, 12);

    Ok(())
}
