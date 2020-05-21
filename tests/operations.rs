#![cfg(not(feature = "unchecked"))]
use rhai::{Engine, EvalAltResult};

#[test]
fn test_max_operations() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.set_max_operations(500);

    engine.on_progress(|count| {
        if count % 100 == 0 {
            println!("{}", count);
        }
        true
    });

    engine.eval::<()>("let x = 0; while x < 20 { x += 1; }")?;

    assert!(matches!(
        *engine
            .eval::<()>("for x in range(0, 500) {}")
            .expect_err("should error"),
        EvalAltResult::ErrorTooManyOperations(_)
    ));

    engine.set_max_operations(0);

    engine.eval::<()>("for x in range(0, 10000) {}")?;

    Ok(())
}

#[test]
fn test_max_operations_functions() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.set_max_operations(500);

    engine.on_progress(|count| {
        if count % 100 == 0 {
            println!("{}", count);
        }
        true
    });

    engine.eval::<()>(
        r#"
                print("Test1");
                let x = 0;

                while x < 28 {
                    print(x);
                    x += 1;
                }
            "#,
    )?;

    #[cfg(not(feature = "no_function"))]
    engine.eval::<()>(
        r#"
                print("Test2");
                fn inc(x) { x + 1 }
                let x = 0;
                while x < 20 { x = inc(x); }
            "#,
    )?;

    #[cfg(not(feature = "no_function"))]
    assert!(matches!(
        *engine
            .eval::<()>(
                r#"
                    print("Test3");
                    fn inc(x) { x + 1 }
                    let x = 0;

                    while x < 28 {
                        print(x);
                        x = inc(x);
                    }
            "#,
            )
            .expect_err("should error"),
        EvalAltResult::ErrorTooManyOperations(_)
    ));

    Ok(())
}

#[test]
fn test_max_operations_eval() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.set_max_operations(500);

    engine.on_progress(|count| {
        if count % 100 == 0 {
            println!("{}", count);
        }
        true
    });

    assert!(matches!(
        *engine
            .eval::<()>(
                r#"
                    let script = "for x in range(0, 500) {}";
                    eval(script);
            "#
            )
            .expect_err("should error"),
        EvalAltResult::ErrorTooManyOperations(_)
    ));

    Ok(())
}
