use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_ops() -> Result<(), EvalAltResult> {
    let engine = Engine::new();

    assert_eq!(engine.eval::<INT>("60 + 5")?, 65);
    assert_eq!(engine.eval::<INT>("(1 + 2) * (6 - 4) / 2")?, 3);

    Ok(())
}

#[test]
fn test_op_precedence() -> Result<(), EvalAltResult> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>("let x = 0; if x == 10 || true { x = 1} x")?,
        1
    );

    Ok(())
}
