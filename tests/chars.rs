use rhai::Engine;

#[test]
fn test_chars() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<char>("'y'"), Ok('y'));
    assert_eq!(engine.eval::<char>("'\\u2764'"), Ok('❤'));
    assert_eq!(engine.eval::<char>(r#"let x="hello"; x[2]"#), Ok('l'));
    assert_eq!(
        engine.eval::<String>(r#"let x="hello"; x[2]='$'; x"#),
        Ok("he$lo".into())
    );

    match engine.eval::<char>("'\\uhello'") {
        Err(_) => (),
        _ => assert!(false),
    }

    match engine.eval::<char>("''") {
        Err(_) => (),
        _ => assert!(false),
    }
}
