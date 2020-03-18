#![cfg_attr(feature = "no_std", no_std)]

use rhai::{Engine, EvalAltResult};

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let result = engine.eval::<i64>("40 + 2")?;

    assert_eq!(result, 42);

    Ok(())
}
