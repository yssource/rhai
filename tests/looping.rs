use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_loop() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r"
				let x = 0;
				let i = 0;

				loop {
					if i < 10 {
						i += 1;
						if x > 20 { continue; }
						x = x + i;
					} else {
						break;
					}
				}

				return x;
		"
        )?,
        21
    );

    Ok(())
}
