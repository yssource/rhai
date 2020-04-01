use rhai::{Engine, EvalAltResult};

#[test]
fn test_string() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<String>(r#""Test string: \u2764""#)?,
        "Test string: ❤"
    );
    assert_eq!(
        engine.eval::<String>(r#""Test string: \x58""#)?,
        "Test string: X"
    );

    assert_eq!(engine.eval::<String>(r#""foo" + "bar""#)?, "foobar");

    #[cfg(not(feature = "no_stdlib"))]
    assert_eq!(engine.eval::<String>(r#""foo" + 123"#)?, "foo123");

    #[cfg(not(feature = "no_float"))]
    #[cfg(not(feature = "no_stdlib"))]
    assert_eq!(engine.eval::<String>(r#""foo" + 123.4556"#)?, "foo123.4556");

    #[cfg(not(feature = "no_stdlib"))]
    assert_eq!(engine.eval::<String>("(42).to_string()")?, "42");

    Ok(())
}
