#![cfg(not(feature = "no_function"))]
use rhai::{Dynamic, Engine, EvalAltResult, FnPtr, Module, INT, Array};
use std::any::{TypeId, Any};

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

#[test]
#[cfg(not(feature = "no_shared"))]
fn test_shared() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    // assert_eq!(
    //     engine.eval::<INT>(
    //         r#"
    //             shared(42)
    //         "#
    //     )?,
    //     42
    // );
    //
    // assert_eq!(
    //     engine.eval::<bool>(
    //         r#"
    //             shared(true)
    //         "#
    //     )?,
    //     true
    // );
    //
    // #[cfg(not(feature = "no_float"))]
    // assert_eq!(
    //     engine.eval::<f64>(
    //         r#"
    //             shared(4.2)
    //         "#
    //     )?,
    //     4.2
    // );
    //
    // assert_eq!(
    //     engine.eval::<String>(
    //         r#"
    //             shared("test")
    //         "#
    //     )?,
    //     "test"
    // );
    //
    // #[cfg(not(feature = "no_index"))]
    // {
    //     assert_eq!(
    //         engine.eval::<Array>(
    //             r#"
    //                 let x = shared([1, 2, 3]);
    //                 let y = shared([4, 5]);
    //                 x + y
    //             "#
    //         )?.len(),
    //         5
    //     );
    //
    //     assert_eq!(
    //         engine.eval::<INT>(
    //             r"
    //                 let x = shared([2, 9]);
    //                 x.insert(-1, 1);
    //                 x.insert(999, 3);
    //
    //                 let r = x.remove(2);
    //
    //                 let y = shared([4, 5]);
    //                 x.append(y);
    //
    //                 x.len + r
    //            "
    //         )?,
    //         14
    //     );
    //
    //     assert_eq!(
    //         engine.eval::<bool>(
    //             r#"
    //                 let x = shared([1, 2, 3]);
    //
    //                 if x[0] + x[2] == 4 {
    //                     true
    //                 } else {
    //                     false
    //                 }
    //             "#
    //         )?,
    //         true
    //     );
    // }
    //
    // #[cfg(not(feature = "no_object"))]
    // assert_eq!(
    //     engine.eval::<INT>(r#"
    //         let y = shared(#{a: 1, b: 2, c: 3});
    //         y.c = shared(5);
    //         y.c
    //     "#)?,
    //     5
    // );
    //
    // #[cfg(not(feature = "no_object"))]
    // assert_eq!(
    //     engine.eval::<INT>(r#"
    //         let y = shared(#{a: 1, b: 2, c: shared(3)});
    //         let c = y.c;
    //         c = 5;// "c" still holds Dynamic Shared
    //         y.c
    //     "#)?,
    //     5
    // );
    //
    // #[cfg(not(feature = "no_capture"))]
    // assert_eq!(
    //     engine.eval::<INT>(r#"
    //         let x = shared(1);
    //         (|| x = x + 41).call();
    //         x
    //     "#)?,
    //     42
    // );

    #[cfg(all(not(feature = "no_object"), not(feature = "no_capture")))]
    assert_eq!(
        engine.eval::<INT>(r#"
            // let x = shared(#{a: 1, b: shared(2), c: 3});
            // let a = x.a;
            // let b = x.b;
            // a = 100;
            // b = 20;
            //
            // let f = |a| {
            //     x.c = x.a + x.b;// + a;
            // };
            //
            // f.call(20);
            //
            // x.c

            let x = #{a: 1, b: 2};

            x.a + x.b
        "#)?,
        42
    );

    Ok(())
}
