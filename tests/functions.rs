#![cfg(not(feature = "no_function"))]
use rhai::{Engine, EvalAltResult, INT};

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
