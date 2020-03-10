use rhai::{Engine, EvalAltResult};

#[test]
fn test_power_of() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("2 ~ 3")?, 8);
    assert_eq!(engine.eval::<i64>("(-2 ~ 3)")?, -8);

    #[cfg(not(feature = "no_float"))]
    {
        assert_eq!(engine.eval::<f64>("2.2 ~ 3.3")?, 13.489468760533386_f64);
        assert_eq!(engine.eval::<f64>("2.0~-2.0")?, 0.25_f64);
        assert_eq!(engine.eval::<f64>("(-2.0~-2.0)")?, 0.25_f64);
        assert_eq!(engine.eval::<f64>("(-2.0~-2)")?, 0.25_f64);
        assert_eq!(engine.eval::<i64>("4~3")?, 64);
    }

    Ok(())
}

#[test]
fn test_power_of_equals() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 2; x ~= 3; x")?, 8);
    assert_eq!(engine.eval::<i64>("let x = -2; x ~= 3; x")?, -8);

    #[cfg(not(feature = "no_float"))]
    {
        assert_eq!(
            engine.eval::<f64>("let x = 2.2; x ~= 3.3; x")?,
            13.489468760533386_f64
        );
        assert_eq!(engine.eval::<f64>("let x = 2.0; x ~= -2.0; x")?, 0.25_f64);
        assert_eq!(engine.eval::<f64>("let x = -2.0; x ~= -2.0; x")?, 0.25_f64);
        assert_eq!(engine.eval::<f64>("let x = -2.0; x ~= -2; x")?, 0.25_f64);
        assert_eq!(engine.eval::<i64>("let x =4; x ~= 3; x")?, 64);
    }

    Ok(())
}
