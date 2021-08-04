use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_while() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            "
                let x = 0;

                while x < 10 {
                    x += 1;
                    if x > 5 { break; }
                    if x > 3 { continue; }
                    x += 3;
                }
                
                x
            ",
        )?,
        6
    );

    Ok(())
}

#[test]
fn test_do() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            "
                let x = 0;

                do {
                    x += 1;
                    if x > 5 { break; }
                    if x > 3 { continue; }
                    x += 3;
                } while x < 10;
                
                x
            ",
        )?,
        6
    );

    Ok(())
}

#[test]
fn test_infinite_loops() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine.set_max_operations(1024);

    assert!(engine.consume("loop {}").is_err());
    assert!(engine.consume("while true {}").is_err());
    assert!(engine.consume("do {} while true").is_err());

    Ok(())
}
