use rhai::{Engine, EvalAltResult};

#[test]
fn test_ops() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("60 + 5")?, 65);
    assert_eq!(engine.eval::<i64>("(1 + 2) * (6 - 4) / 2")?, 3);

    Ok(())
}

#[test]
fn test_op_prec() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<i64>("let x = 0; if x == 10 || true { x = 1} x")?,
        1
    );

    Ok(())
}
