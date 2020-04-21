#![cfg_attr(feature = "no_std", no_std)]

use rhai::{Engine, EvalAltResult, INT};

fn main() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    let result = engine.eval::<INT>("40 + 2")?;

    #[cfg(not(feature = "no_std"))]
    println!("Answer: {}", result);

    #[cfg(feature = "no_std")]
    assert_eq!(result, 42);

    Ok(())
}
