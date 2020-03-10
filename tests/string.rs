use rhai::{Engine, EvalAltResult};

#[test]
fn test_string() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<String>(r#""Test string: \u2764""#)?,
        "Test string: ❤".to_string()
    );
    assert_eq!(
        engine.eval::<String>(r#""Test string: \x58""#)?,
        "Test string: X".to_string()
    );

    assert_eq!(
        engine.eval::<String>(r#""foo" + "bar""#)?,
        "foobar".to_string()
    );

    #[cfg(not(feature = "no_stdlib"))]
    assert_eq!(
        engine.eval::<String>(r#""foo" + 123"#)?,
        "foo123".to_string()
    );

    #[cfg(not(feature = "no_float"))]
    #[cfg(not(feature = "no_stdlib"))]
    assert_eq!(
        engine.eval::<String>(r#""foo" + 123.4556"#)?,
        "foo123.4556".to_string()
    );

    Ok(())
}
