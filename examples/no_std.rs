#![cfg_attr(feature = "no_std", no_std)]

use rhai::{Engine, EvalAltResult, INT};

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let result = engine.eval::<INT>("40 + 2")?;

    println!("Answer: {}", 42);

    Ok(())
}
