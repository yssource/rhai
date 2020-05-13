#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult, Func, ParseErrorType, Scope, INT};

#[test]
fn test_functions() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(engine.eval::<INT>("fn add(x, n) { x + n } add(40, 2)")?, 42);

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>("fn add(x, n) { x + n } let x = 40; x.add(2)")?,
        42
    );

    assert_eq!(engine.eval::<INT>("fn mul2(x) { x * 2 } mul2(21)")?, 42);

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>("fn mul2(x) { x * 2 } let x = 21; x.mul2()")?,
        42
    );

    Ok(())
}
