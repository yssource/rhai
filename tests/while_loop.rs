use rhai::{Engine, EvalAltResult};

#[test]
fn test_while() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<i64>(
            "let x = 0; while x < 10 { x = x + 1; if x > 5 { \
             break } } x",
        )?,
        6
    );

    Ok(())
}
