use rhai::{Engine, EvalAltResult};

#[test]
fn test_chars() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<char>("'y'")?, 'y');
    assert_eq!(engine.eval::<char>("'\\u2764'")?, '❤');

    #[cfg(not(feature = "no_index"))]
    {
        assert_eq!(engine.eval::<char>(r#"let x="hello"; x[2]"#)?, 'l');
        assert_eq!(
            engine.eval::<String>(r#"let x="hello"; x[2]='$'; x"#)?,
            "he$lo".to_string()
        );
    }

    assert!(engine.eval::<char>("'\\uhello'").is_err());
    assert!(engine.eval::<char>("''").is_err());

    Ok(())
}
