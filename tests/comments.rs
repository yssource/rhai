use rhai::{Engine, INT};

#[test]
fn test_comments() {
    let mut engine = Engine::new();

    assert!(engine
        .eval::<INT>("let x = 5; x // I am a single line comment, yay!")
        .is_ok());

    assert!(engine
        .eval::<INT>("let /* I am a multiline comment, yay! */ x = 5; x")
        .is_ok());
}
