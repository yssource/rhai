#![cfg(not(feature = "no_function"))]
use rhai::{Dynamic, Engine, EvalAltResult, FnPtr, Module, INT};
use std::any::TypeId;

#[test]
fn test_fn_ptr_curry_call() -> Result<(), Box<EvalAltResult>> {
    let mut module = Module::new();

    module.set_raw_fn(
        "call_with_arg",
        &[TypeId::of::<FnPtr>(), TypeId::of::<INT>()],
        |engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]| {
            let fn_ptr = std::mem::take(args[0]).cast::<FnPtr>();
            fn_ptr.call_dynamic(engine, lib, None, [std::mem::take(args[1])])
        },
    );

    let mut engine = Engine::new();
    engine.load_package(module.into());

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                let addition = |x, y| { x + y };
                let curried = addition.curry(2);

                call_with_arg(curried, 40)
            "#
        )?,
        42
    );

    Ok(())
}

#[test]
#[cfg(not(feature = "no_capture"))]
fn test_closures() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r#"
                let x = 8;

                let res = |y, z| {
                    let w = 12;

                    return (|| x + y + z + w).call();
                }.curry(15).call(2);

                res + (|| x - 3).call()
            "#
        )?,
        42
    );

    Ok(())
}
