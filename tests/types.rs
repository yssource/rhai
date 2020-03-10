use rhai::{Engine, EvalAltResult};

#[test]
fn test_type_of() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<String>("type_of(60 + 5)")?, "i64");

    #[cfg(not(feature = "no_float"))]
    assert_eq!(engine.eval::<String>("type_of(1.0 + 2.0)")?, "f64");

    #[cfg(not(feature = "no_index"))]
    #[cfg(not(feature = "no_float"))]
    assert_eq!(
        engine.eval::<String>(r#"type_of([1.0, 2, "hello"])"#)?,
        "array"
    );

    assert_eq!(engine.eval::<String>(r#"type_of("hello")"#)?, "string");
    assert_eq!(engine.eval::<String>("let x = 123; x.type_of()")?, "i64");

    Ok(())
}
