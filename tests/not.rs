use rhai::{Engine, EvalAltResult};

#[test]
fn test_not() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>("let not_true = !true; not_true")?,
        false
    );

    assert_eq!(engine.eval::<bool>("fn not(x) { !x } not(false)")?, true);

    // TODO - do we allow stacking unary operators directly? e.g '!!!!!!!true'
    assert_eq!(engine.eval::<bool>("!(!(!(!(true))))")?, true);

    Ok(())
}
