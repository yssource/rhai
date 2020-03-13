#![cfg(not(feature = "no_stdlib"))]
#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_engine_call_fn() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    engine.consume(
        r"
            fn hello(x, y) {
                x.len() + y
            }
            fn hello(x) {
                x * 2
            }
                ",
        true,
    )?;

    let r: i64 = engine.call_fn("hello", (String::from("abc"), 123 as INT))?;
    assert_eq!(r, 126);

    let r: i64 = engine.call_fn("hello", 123 as INT)?;
    assert_eq!(r, 246);

    Ok(())
}
