#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult};

#[test]
fn test_stack_overflow() -> Result<(), EvalAltResult> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<i64>(
            r"
                fn foo(n) { if n == 0 { 0 } else { n + foo(n-1) } }
                foo(30)
    ",
        )?,
        465
    );

    match engine.eval::<()>(
        r"
            fn foo(n) { if n == 0 { 0 } else { n + foo(n-1) } }
            foo(1000)
    ",
    ) {
        Ok(_) => panic!("should be stack overflow"),
        Err(EvalAltResult::ErrorStackOverflow(_)) => (),
        Err(_) => panic!("should be stack overflow"),
    }

    Ok(())
}
