use rhai::{Engine, EvalAltResult};

#[test]
fn test_engine_call_fn() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let ast = Engine::compile("fn hello(x, y) { x.len() + y }")?;

    let result: i64 = engine.call_fn("hello", ast, (&mut String::from("abc"), &mut 123_i64))?;

    assert_eq!(result, 126);

    Ok(())
}
