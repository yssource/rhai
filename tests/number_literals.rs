use rhai::{Engine, EvalAltResult};

#[test]
fn test_number_literal() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("65")?, 65);

    Ok(())
}

#[test]
fn test_hex_literal() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 0xf; x")?, 15);
    assert_eq!(engine.eval::<i64>("let x = 0xff; x")?, 255);

    Ok(())
}

#[test]
fn test_octal_literal() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 0o77; x")?, 63);
    assert_eq!(engine.eval::<i64>("let x = 0o1234; x")?, 668);

    Ok(())
}

#[test]
fn test_binary_literal() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 0b1111; x")?, 15);
    assert_eq!(
        engine.eval::<i64>("let x = 0b0011_1100_1010_0101; x")?,
        15525
    );

    Ok(())
}
