use rhai::{Engine, EvalAltResult};

#[test]
fn test_throw() {
    let engine = Engine::new();

    assert!(matches!(
        *engine.eval::<()>(r#"if true { throw "hello" }"#).expect_err("expects error"),
        EvalAltResult::ErrorRuntime(s, _) if s == "hello"
    ));

    assert!(matches!(
        *engine.eval::<()>(r#"throw"#).expect_err("expects error"),
        EvalAltResult::ErrorRuntime(s, _) if s == ""
    ));
}
