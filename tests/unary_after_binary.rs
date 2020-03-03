use rhai::{Engine, EvalAltResult};

#[test]
// TODO also add test case for unary after compound
// Hah, turns out unary + has a good use after all!
fn test_unary_after_binary() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("10 % +4")?, 2);
    assert_eq!(engine.eval::<i64>("10 << +4")?, 160);
    assert_eq!(engine.eval::<i64>("10 >> +4")?, 0);
    assert_eq!(engine.eval::<i64>("10 & +4")?, 0);
    assert_eq!(engine.eval::<i64>("10 | +4")?, 14);
    assert_eq!(engine.eval::<i64>("10 ^ +4")?, 14);

    Ok(())
}
