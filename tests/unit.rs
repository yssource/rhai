use rhai::{Engine, EvalAltResult};

#[test]
fn test_unit() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    engine.eval::<()>("let x = (); x")?;
    Ok(())
}

#[test]
fn test_unit_eq() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    assert_eq!(engine.eval::<bool>("let x = (); let y = (); x == y")?, true);
    Ok(())
}

#[test]
fn test_unit_with_spaces() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    engine.eval::<()>("let x = ( ); x")?;
    Ok(())
}
