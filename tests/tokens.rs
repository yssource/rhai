use rhai::{Engine, ParseErrorType};

#[test]
fn test_tokens_disabled() {
    let mut engine = Engine::new();

    engine.disable_symbol("if"); // disable the 'if' keyword

    assert!(matches!(
        *engine.compile("let x = if true { 42 } else { 0 };").expect_err("should error").0,
        ParseErrorType::MissingToken(ref token, _) if token == ";"
    ));

    engine.disable_symbol("+="); // disable the '+=' operator

    assert!(matches!(
        *engine.compile("let x = 40 + 2; x += 1;").expect_err("should error").0,
        ParseErrorType::BadInput(ref s) if s == "Unexpected '+='"
    ));
}
