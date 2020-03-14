use rhai::{Engine, EvalAltResult};

#[test]
fn test_constant() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("const x = 123; x")?, 123);

    assert!(
        matches!(engine.eval::<i64>("const x = 123; x = 42;").expect_err("expects error"),
        EvalAltResult::ErrorAssignmentToConstant(var, _) if var == "x")
    );

    #[cfg(not(feature = "no_index"))]
    assert!(
        matches!(engine.eval::<i64>("const x = [1, 2, 3, 4, 5]; x[2] = 42;").expect_err("expects error"),
        EvalAltResult::ErrorAssignmentToConstant(var, _) if var == "x")
    );

    Ok(())
}
