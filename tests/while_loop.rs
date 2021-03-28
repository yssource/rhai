use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_while() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r"
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
            r"
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
