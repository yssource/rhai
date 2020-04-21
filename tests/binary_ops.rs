use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_binary_ops() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(engine.eval::<INT>("10 % 4")?, 2);
    assert_eq!(engine.eval::<INT>("10 << 4")?, 160);
    assert_eq!(engine.eval::<INT>("10 >> 4")?, 0);
    assert_eq!(engine.eval::<INT>("10 & 4")?, 0);
    assert_eq!(engine.eval::<INT>("10 | 4")?, 14);
    assert_eq!(engine.eval::<INT>("10 ^ 4")?, 14);

    assert_eq!(engine.eval::<bool>("42 == 42")?, true);
    assert_eq!(engine.eval::<bool>("42 > 42")?, false);

    // Incompatible types compare to false
    assert_eq!(engine.eval::<bool>("true == 42")?, false);
    assert_eq!(engine.eval::<bool>(r#""42" == 42"#)?, false);

    Ok(())
}
