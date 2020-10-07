use rhai::{Engine, EvalAltResult, ParseErrorType, INT};

#[test]
fn test_constant() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(engine.eval::<INT>("const x = 123; x")?, 123);

    assert!(matches!(
        *engine
            .eval::<INT>("const x = 123; x = 42;")
            .expect_err("expects error"),
        EvalAltResult::ErrorParsing(ParseErrorType::AssignmentToConstant(x), _) if x == "x"
    ));

    #[cfg(not(feature = "no_index"))]
    assert!(matches!(
        *engine.eval::<INT>("const x = [1, 2, 3, 4, 5]; x[2] = 42;").expect_err("expects error"),
        EvalAltResult::ErrorParsing(ParseErrorType::AssignmentToConstant(x), _) if x == "x"
    ));

    Ok(())
}

#[test]
fn test_var_is_def() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert!(engine.eval::<bool>(
        r#"
            let x = 42;
            is_def_var("x")
    "#
    )?);
    assert!(!engine.eval::<bool>(
        r#"
            let x = 42;
            is_def_var("y")
    "#
    )?);
    assert!(engine.eval::<bool>(
        r#"
            const x = 42;
            is_def_var("x")
    "#
    )?);

    Ok(())
}
