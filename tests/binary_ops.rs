use rhai::Engine;

#[test]
fn test_binary_ops() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("10 % 4"), Ok(2));
    assert_eq!(engine.eval::<i64>("10 << 4"), Ok(160));
    assert_eq!(engine.eval::<i64>("10 >> 4"), Ok(0));
    assert_eq!(engine.eval::<i64>("10 & 4"), Ok(0));
    assert_eq!(engine.eval::<i64>("10 | 4"), Ok(14));
    assert_eq!(engine.eval::<i64>("10 ^ 4"), Ok(14));

    assert_eq!(engine.eval::<bool>("42 == 42"), Ok(true));
    assert_eq!(engine.eval::<bool>("42 > 42"), Ok(false));

    // Incompatible types compare to false
    assert_eq!(engine.eval::<bool>("true == 42"), Ok(false));
    assert_eq!(engine.eval::<bool>(r#""42" == 42"#), Ok(false));
}
