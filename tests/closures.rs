#![cfg(not(feature = "no_function"))]
use rhai::{Dynamic, Engine, EvalAltResult, RegisterFn, FnPtr, Module, INT, Array};
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
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r#"
                shared(42)
            "#
        )?,
        42
    );

    assert_eq!(
        engine.eval::<bool>(
            r#"
                shared(true)
            "#
        )?,
        true
    );

    #[cfg(not(feature = "no_float"))]
    assert_eq!(
        engine.eval::<f64>(
            r#"
                shared(4.2)
            "#
        )?,
        4.2
    );

    assert_eq!(
        engine.eval::<String>(
            r#"
                shared("test")
            "#
        )?,
        "test"
    );

    assert_eq!(
        engine.eval::<char>(
            r#"
                shared('x')
            "#
        )?,
        'x'
    );

    assert_eq!(
        engine.eval::<String>(
            r#"
                let s = shared("test");
                let i = shared(0);
                i = 2;
                s[i] = 'S';

                s
            "#
        )?,
        "teSt"
    );

    #[cfg(not(feature = "no_index"))]
    {
        assert_eq!(
            engine.eval::<Array>(
                r#"
                    let x = shared([1, 2, 3]);
                    let y = shared([4, 5]);
                    x + y
                "#
            )?.len(),
            5
        );

        assert_eq!(
            engine.eval::<INT>(
                r"
                    let x = shared([2, 9]);
                    x.insert(-1, 1);
                    x.insert(999, 3);

                    let r = x.remove(2);

                    let y = shared([4, 5]);
                    x.append(y);

                    x.len + r
               "
            )?,
            14
        );

        assert_eq!(
            engine.eval::<bool>(
                r#"
                    let x = shared([1, 2, 3]);

                    if x[0] + x[2] == 4 {
                        true
                    } else {
                        false
                    }
                "#
            )?,
            true
        );

        assert_eq!(
            engine.eval::<INT>(
                r#"
                    let x = shared([1, 2, 3]);
                    let y = shared(());

                    (|| {
                        for i in x {
                            y = i * 10;
                        }
                    }).call();

                    y
                "#
            )?,
            30
        );
    }

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(r#"
            let y = shared(#{a: 1, b: 2, c: 3});
            y.c = shared(5);
            y.c
        "#)?,
        5
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(r#"
            let y = shared(#{a: 1, b: 2, c: shared(3)});
            let c = y.c;
            c = 5;// "c" holds Dynamic Shared
            y.c
        "#)?,
        5
    );

    #[cfg(not(feature = "no_capture"))]
    assert_eq!(
        engine.eval::<INT>(r#"
            let x = shared(1);
            (|| x = x + 41).call();
            x
        "#)?,
        42
    );

    #[cfg(all(not(feature = "no_object"), not(feature = "no_capture")))]
    assert_eq!(
        engine.eval::<INT>(r#"
            let x = shared(#{a: 1, b: shared(2), c: 3});
            let a = x.a;
            let b = x.b;
            a = 100; // does not hold reference to x.a
            b = 20; // does hold reference to x.b

            let f = |a| {
                x.c = x.a + x.b + a;
            };

            f.call(21);

            x.c
        "#)?,
        42
    );

    // Register a binary function named `foo`
    engine.register_fn("custom_addition", |x: INT, y: INT| x + y);

    assert_eq!(
        engine.eval::<INT>(r#"
            custom_addition(shared(20), shared(22))
        "#)?,
        42
    );

    #[derive(Clone)]
    struct TestStruct {
        x: INT,
    }

    impl TestStruct {
        fn update(&mut self) {
            self.x += 1000;
        }

        fn merge(&mut self, other: Self) {
            self.x += other.x;
        }

        fn get_x(&mut self) -> INT {
            self.x
        }

        fn set_x(&mut self, new_x: INT) {
            self.x = new_x;
        }

        fn new() -> Self {
            TestStruct { x: 1 }
        }
    }

    engine.register_type::<TestStruct>();

    engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
    engine.register_fn("update", TestStruct::update);
    engine.register_fn("merge", TestStruct::merge);
    engine.register_fn("new_ts", TestStruct::new);
    engine.
        register_raw_fn(
            "mutate_with_cb",
            &[
                TypeId::of::<TestStruct>(),
                TypeId::of::<INT>(),
                TypeId::of::<FnPtr>(),
            ],
            move |engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]| {
                let fp = std::mem::take(args[2]).cast::<FnPtr>();
                let mut value = args[1].clone();
                {
                    let mut lock = value.write_lock::<INT>().unwrap();
                    *lock = *lock + 1;
                }
                let this_ptr = args.get_mut(0).unwrap();

                fp.call_dynamic(engine, lib, Some(this_ptr), [value])
            },
        );

    assert_eq!(
        engine.eval::<INT>(
            r"
                let a = shared(new_ts());

                a.x = 100;
                a.update();
                a.merge(a.take()); // take is important to prevent a deadlock

                a.x
            "
        )?,
        2200
    );

    assert_eq!(
        engine.eval::<INT>(
            r"
                let a = shared(new_ts());
                let b = shared(100);

                a.mutate_with_cb(b, |param| {
                    this.x = param;
                    param = 50;
                    this.update();
                });

                a.update();
                a.x += b;

                a.x
            "
        )?,
        2151
    );

    Ok(())
}
