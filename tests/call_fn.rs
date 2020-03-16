#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult, INT};

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
