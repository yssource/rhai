use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_throw() {
    let mut engine = Engine::new();

    match engine.eval::<INT>(r#"if true { throw "hello" }"#) {
        Ok(_) => panic!("not an error"),
        Err(EvalAltResult::ErrorRuntime(s, _)) if s == "hello" => (),
        Err(err) => panic!("wrong error: {}", err),
    }

    match engine.eval::<INT>(r#"throw;"#) {
        Ok(_) => panic!("not an error"),
        Err(EvalAltResult::ErrorRuntime(s, _)) if s == "" => (),
        Err(err) => panic!("wrong error: {}", err),
    }
}
