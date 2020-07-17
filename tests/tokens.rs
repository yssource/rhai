use rhai::{Engine, EvalAltResult, ParseErrorType, RegisterFn, INT};

#[test]
fn test_tokens_disabled() {
    let mut engine = Engine::new();

    engine.disable_symbol("if"); // disable the 'if' keyword

    assert!(matches!(
        *engine
            .compile("let x = if true { 42 } else { 0 };")
            .expect_err("should error")
            .0,
        ParseErrorType::Reserved(err) if err == "if"
    ));

    engine.disable_symbol("+="); // disable the '+=' operator

    assert!(matches!(
        *engine.compile("let x = 40 + 2; x += 1;").expect_err("should error").0,
        ParseErrorType::BadInput(ref s) if s == "Unexpected '+='"
    ));
}

#[test]
fn test_tokens_custom_operator() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    // Register a custom operator called `foo` and give it
    // a precedence of 160 (i.e. between +|- and *|/).
    engine.register_custom_operator("foo", 160).unwrap();

    // Register a binary function named `foo`
    engine.register_fn("foo", |x: INT, y: INT| (x * y) - (x + y));

    assert_eq!(
        engine.eval_expression::<INT>("1 + 2 * 3 foo 4 - 5 / 6")?,
        15
    );

    #[cfg(not(feature = "no_function"))]
    assert_eq!(
        engine.eval::<INT>(
            r"
                fn foo(x, y) { y - x }
                1 + 2 * 3 foo 4 - 5 / 6
            "
        )?,
        -1
    );

    Ok(())
}
