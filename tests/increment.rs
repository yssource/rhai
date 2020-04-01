use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_increment() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<INT>("let x = 1; x += 2; x")?, 3);
    assert_eq!(
        engine.eval::<String>("let s = \"test\"; s += \"ing\"; s")?,
        "testing"
    );

    Ok(())
}
