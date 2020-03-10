#![cfg(not(feature = "no_stdlib"))]
use rhai::{Engine, EvalAltResult, INT};

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

    let result: INT = engine.call_fn("hello", &ast, (String::from("abc"), 123 as INT))?;

    assert_eq!(result, 126);

    Ok(())
}
