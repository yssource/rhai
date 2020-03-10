use rhai::{Engine, EvalAltResult};

#[test]
fn test_engine_call_fn() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let ast = engine.compile(
        r"
            fn hello(x, y) {
                x.len() + y
            }
                ",
    )?;

    let result: i64 = engine.call_fn("hello", &ast, (String::from("abc"), 123_i64))?;

    assert_eq!(result, 126);

    Ok(())
}
