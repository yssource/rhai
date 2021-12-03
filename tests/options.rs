use rhai::{Engine, EvalAltResult};

#[test]
fn test_options() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine.compile("let x = if y { z } else { w };")?;

    engine.set_allow_if_expression(false);

    assert!(engine.compile("let x = if y { z } else { w };").is_err());

    engine.compile("let x = { let z = 0; z + 1 };")?;

    engine.set_allow_statement_expression(false);

    assert!(engine.compile("let x = { let z = 0; z + 1 };").is_err());

    engine.compile("let x = || 42;")?;

    engine.set_allow_anonymous_fn(false);

    assert!(engine.compile("let x = || 42;").is_err());

    Ok(())
}
