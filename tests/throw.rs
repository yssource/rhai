use rhai::{Engine, EvalAltResult};

#[test]
fn test_throw() {
    let engine = Engine::new();

    assert!(matches!(
        *engine.eval::<()>("if true { throw 42 }").expect_err("expects error"),
        EvalAltResult::ErrorRuntime(s, _) if s.as_int().unwrap() == 42
    ));

    assert!(matches!(
        *engine.eval::<()>(r#"throw"#).expect_err("expects error"),
        EvalAltResult::ErrorRuntime(s, _) if s.is::<()>()
    ));
}
