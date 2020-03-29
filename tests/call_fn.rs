#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult, ParseErrorType, INT};

#[test]
fn test_fn() -> Result<(), EvalAltResult> {
    let engine = Engine::new();

    // Expect duplicated parameters error
    match engine
        .compile("fn hello(x, x) { x }")
        .expect_err("should be error")
        .error_type()
    {
        ParseErrorType::FnDuplicatedParam(f, p) if f == "hello" && p == "x" => (),
        _ => assert!(false, "wrong error"),
    }

    Ok(())
}

#[test]
fn test_call_fn() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    engine.consume(
        true,
        r"
            fn hello(x, y) {
                x + y
            }
            fn hello(x) {
                x * 2
            }
                ",
    )?;

    let r: i64 = engine.call_fn("hello", (42 as INT, 123 as INT))?;
    assert_eq!(r, 165);

    let r: i64 = engine.call_fn("hello", 123 as INT)?;
    assert_eq!(r, 246);

    Ok(())
}
