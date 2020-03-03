use rhai::{Engine, EvalAltResult};

#[test]
fn test_loop() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert!(engine.eval::<bool>(
        r"
			let x = 0;
			let i = 0;

			loop {
				if i < 10 {
					x = x + i;
					i = i + 1;
				}
				else {
					break;
				}
			}

			x == 45
		"
    )?);

    Ok(())
}
