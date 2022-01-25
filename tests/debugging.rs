#![cfg(feature = "debugging")]
use rhai::{Engine, EvalAltResult, INT};

#[cfg(not(feature = "no_index"))]
use rhai::Array;

#[test]
fn test_debugging() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_index"))]
    {
        let r = engine.eval::<Array>(
            "
                fn foo(x) {
                    if x >= 5 {
                        stack_trace()
                    } else {
                        foo(x+1)
                    }
                }

                foo(0)
            ",
        )?;

        assert_eq!(r.len(), 6);

        assert_eq!(engine.eval::<INT>("len(stack_trace())")?, 0);
    }

    Ok(())
}
