 use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_left_shift() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();
    assert_eq!(engine.eval::<INT>("4 << 2")?, 16);
    Ok(())
}

#[test]
fn test_right_shift() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();
    assert_eq!(engine.eval::<INT>("9 >> 1")?, 4);
    Ok(())
}
