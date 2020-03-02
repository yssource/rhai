use rhai::{Engine, EvalAltResult};

#[test]
fn test_decrement() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("let x = 10; x -= 7; x")?, 3);

    let r = engine.eval::<String>("let s = \"test\"; s -= \"ing\"; s");

    match r {
        Err(EvalAltResult::ErrorFunctionNotFound(err, _)) if err == "- (string, string)" => (),
        _ => panic!(),
    }

    Ok(())
}
