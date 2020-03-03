use rhai::{Engine, EvalAltResult};

#[test]
fn test_string() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<String>("\"Test string: \\u2764\"")?,
        "Test string: ❤".to_string()
    );
    assert_eq!(
        engine.eval::<String>("\"foo\" + \"bar\"")?,
        "foobar".to_string()
    );

    Ok(())
}
