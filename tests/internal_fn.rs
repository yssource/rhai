#![cfg(not(feature = "no_function"))]

use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_internal_fn() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<INT>("fn addme(a, b) { a+b } addme(3, 4)")?, 7);
    assert_eq!(engine.eval::<INT>("fn bob() { return 4; 5 } bob()")?, 4);

    Ok(())
}

#[test]
fn test_big_internal_fn() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r"
                fn mathme(a, b, c, d, e, f) {
                    a - b * c + d * e - f
                }
                mathme(100, 5, 2, 9, 6, 32)
            ",
        )?,
        112
    );

    Ok(())
}

#[test]
fn test_internal_fn_overloading() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r#"
                fn abc(x,y,z) { 2*x + 3*y + 4*z + 888 }
                fn abc(x) { x + 42 }
                fn abc(x,y) { x + 2*y + 88 }
                fn abc() { 42 }
                fn abc(x) { x - 42 }    // should override previous definition

                abc() + abc(1) + abc(1,2) + abc(1,2,3)
    "#
        )?,
        1002
    );

    Ok(())
}
