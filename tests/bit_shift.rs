use rhai::{Engine, EvalAltResult};

#[test]
fn test_left_shift() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    assert_eq!(engine.eval::<i64>("4 << 2")?, 16);
    Ok(())
}

#[test]
fn test_right_shift() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    assert_eq!(engine.eval::<i64>("9 >> 1")?, 4);
    Ok(())
}
