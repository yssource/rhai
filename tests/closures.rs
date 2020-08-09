#![cfg(not(feature = "no_function"))]
use rhai::{Dynamic, Engine, EvalAltResult, FnPtr, Module, RegisterFn, INT};
use std::any::TypeId;
use std::mem::take;

#[test]
fn test_fn_ptr_curry_call() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    #[allow(deprecated)]
    engine.register_raw_fn(
        "call_with_arg",
        &[TypeId::of::<FnPtr>(), TypeId::of::<INT>()],
        |engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]| {
            let fn_ptr = std::mem::take(args[0]).cast::<FnPtr>();
            fn_ptr.call_dynamic(engine, lib, None, [std::mem::take(args[1])])
        },
    );

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
#[cfg(not(feature = "no_closure"))]
#[cfg(not(feature = "no_object"))]
fn test_closures() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

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

    assert_eq!(
        engine.eval::<INT>(
            r#"
                let a = 41;
                let foo = |x| { a += x };
                foo.call(1);
                a
            "#
        )?,
        42
    );

    assert!(engine.eval::<bool>(
        r#"
            let a = 41;
            let foo = |x| { a += x };
            a.is_shared()
        "#
    )?);

    engine.register_fn("plus_one", |x: INT| x + 1);

    assert_eq!(
        engine.eval::<INT>(
            r#"
                let a = 41;
                let f = || plus_one(a);
                f.call()
            "#
        )?,
        42
    );

    engine.register_raw_fn(
        "custom_call",
        &[TypeId::of::<INT>(), TypeId::of::<FnPtr>()],
        |engine: &Engine, module: &Module, args: &mut [&mut Dynamic]| {
            let func = take(args[1]).cast::<FnPtr>();

            func.call_dynamic(engine, module, None, [])
        },
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"
                let a = 41;
                let b = 0;
                let f = || b.custom_call(|| a + 1);
                
                f.call()
            "#
        )?,
        42
    );

    Ok(())
}

#[test]
#[cfg(not(feature = "no_closure"))]
#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "sync"))]
fn test_closures_data_race() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r#"
                    let a = 1;
                    let b = 40;
                    let foo = |x| { this += a + x };
                    b.call(foo, 1);
                    b
                "#
        )?,
        42
    );

    assert!(matches!(
        *engine
            .eval::<INT>(
                r#"
                        let a = 20;
                        let foo = |x| { this += a + x };
                        a.call(foo, 1);
                        a
                    "#
            )
            .expect_err("should error"),
        EvalAltResult::ErrorDataRace(_, _)
    ));

    Ok(())
}
