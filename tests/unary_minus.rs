use rhai::{Engine, EvalAltResult};

#[test]
fn test_unary_minus() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = -5; x")?, -5);
    assert_eq!(engine.eval::<i64>("fn n(x) { -x } n(5)")?, -5);
    assert_eq!(engine.eval::<i64>("5 - -(-5)")?, 0);

    Ok(())
}
