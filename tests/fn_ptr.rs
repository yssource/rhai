use rhai::{Engine, EvalAltResult, RegisterFn, INT};

#[test]
fn test_fn_ptr() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine.register_fn("bar", |x: &mut INT, y: INT| *x += y);

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                let f = Fn("bar");
                let x = 40;
                f.call(x, 2);
                x
            "#
        )?,
        40
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                let f = Fn("bar");
                let x = 40;
                x.call(f, 2);
                x
            "#
        )?,
        42
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"
                let f = Fn("bar");
                let x = 40;
                call(f, x, 2);
                x
            "#
        )?,
        42
    );

    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn foo(x) { this += x; }

                let f = Fn("foo");
                let x = 40;
                x.call(f, 2);
                x
            "#
        )?,
        42
    );

    #[cfg(not(feature = "no_function"))]
    assert!(matches!(
        *engine
            .eval::<INT>(
                r#"
                fn foo(x) { this += x; }

                let f = Fn("foo");
                call(f, 2);
                x
            "#
            )
            .expect_err("should error"),
        EvalAltResult::ErrorInFunctionCall(fn_name, err, _) if fn_name == "foo" && matches!(*err, EvalAltResult::ErrorUnboundedThis(_))
    ));

    Ok(())
}
