use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_comments() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>("let x = 42; x // I am a single line comment, yay!")?,
        42
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"
            let /* I am a
                multi-line
                    comment, yay!
                */ x = 42; x
            "#
        )?,
        42
    );

    Ok(())
}
