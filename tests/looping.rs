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
						x = x + i;
						i = i + 1;
					} else {
						break;
					}
				}

				return x;
		"
        )?,
        45
    );

    Ok(())
}
