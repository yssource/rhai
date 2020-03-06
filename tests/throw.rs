use rhai::{Engine, EvalAltResult};

#[test]
fn test_throw() {
    let mut engine = Engine::new();

    match engine.eval::<i64>(r#"if true { throw "hello" }"#) {
        Ok(_) => panic!("not an error"),
        Err(EvalAltResult::ErrorRuntime(s, _)) if s == "hello" => (),
        Err(err) => panic!("wrong error: {}", err),
    }

    match engine.eval::<i64>(r#"throw;"#) {
        Ok(_) => panic!("not an error"),
        Err(EvalAltResult::ErrorRuntime(s, _)) if s == "" => (),
        Err(err) => panic!("wrong error: {}", err),
    }
}
