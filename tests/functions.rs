#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult, FnNamespace, Module, ParseErrorType, RegisterFn, INT};

#[test]
fn test_functions() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(engine.eval::<INT>("fn add(x, n) { x + n } add(40, 2)")?, 42);

    assert_eq!(
        engine.eval::<INT>("fn add(x, n,) { x + n } add(40, 2,)")?,
        42
    );

    assert_eq!(
        engine.eval::<INT>("fn add(x, n) { x + n } let a = 40; add(a, 2); a")?,
        40
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>("fn add(n) { this + n } let x = 40; x.add(2)")?,
        42
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>("fn add(n) { this += n; } let x = 40; x.add(2); x")?,
        42
    );

    assert_eq!(engine.eval::<INT>("fn mul2(x) { x * 2 } mul2(21)")?, 42);

    assert_eq!(
        engine.eval::<INT>("fn mul2(x) { x *= 2 } let a = 21; mul2(a); a")?,
        21
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>("fn mul2() { this * 2 } let x = 21; x.mul2()")?,
        42
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>("fn mul2() { this *= 2; } let x = 21; x.mul2(); x")?,
        42
    );

    Ok(())
}

#[cfg(not(feature = "no_function"))]
#[test]
fn test_functions_namespaces() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    #[cfg(not(feature = "no_module"))]
    {
        let mut m = Module::new();
        let hash = m.set_fn_0("test", || Ok(999 as INT));
        m.update_fn_namespace(hash, FnNamespace::Global);

        engine.register_static_module("hello", m);

        assert_eq!(engine.eval::<INT>("test()")?, 999);
        assert_eq!(engine.eval::<INT>("fn test() { 123 } test()")?, 123);
    }

    engine.register_fn("test", || 42 as INT);

    assert_eq!(engine.eval::<INT>("test()")?, 42);
    assert_eq!(engine.eval::<INT>("fn test() { 123 } test()")?, 123);

    Ok(())
}

#[test]
fn test_function_pointers() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(engine.eval::<String>(r#"type_of(Fn("abc"))"#)?, "Fn");

    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn foo(x) { 40 + x }

                let f = Fn("foo");
                call(f, 2)
            "#
        )?,
        42
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn foo(x) { 40 + x }

                let fn_name = "f";
                fn_name += "oo";

                let f = Fn(fn_name);
                f.call(2)
            "#
        )?,
        42
    );

    #[cfg(not(feature = "no_object"))]
    assert!(matches!(
        *engine.eval::<INT>(r#"let f = Fn("abc"); f.call(0)"#).expect_err("should error"),
        EvalAltResult::ErrorFunctionNotFound(f, _) if f.starts_with("abc (")
    ));

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn foo(x) { 40 + x }

                let x = #{ action: Fn("foo") };
                x.action.call(2)
            "#
        )?,
        42
    );

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn foo(x) { this.data += x; }

                let x = #{ data: 40, action: Fn("foo") };
                x.action(2);
                x.data
            "#
        )?,
        42
    );

    Ok(())
}

#[test]
#[cfg(not(feature = "no_closure"))]
fn test_function_captures() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn foo(y) { x += y; x }

                let x = 41;
                let y = 999;

                foo!(1) + x
            "#
        )?,
        83
    );

    assert!(engine
        .eval::<INT>(
            r#"
                fn foo(y) { x += y; x }

                let x = 41;
                let y = 999;

                foo(1) + x
            "#
        )
        .is_err());

    #[cfg(not(feature = "no_object"))]
    assert!(matches!(
        *engine
            .compile(
                r#"
                    fn foo() { this += x; }

                    let x = 41;
                    let y = 999;

                    y.foo!();
                "#
            )
            .expect_err("should error")
            .0,
        ParseErrorType::MalformedCapture(_)
    ));

    Ok(())
}

#[test]
fn test_function_is_def() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert!(engine.eval::<bool>(
        r#"
            fn foo(x) { x + 1 }
            is_def_fn("foo", 1)
    "#
    )?);
    assert!(!engine.eval::<bool>(
        r#"
            fn foo(x) { x + 1 }
            is_def_fn("bar", 1)
    "#
    )?);
    assert!(!engine.eval::<bool>(
        r#"
            fn foo(x) { x + 1 }
            is_def_fn("foo", 0)
    "#
    )?);

    Ok(())
}
