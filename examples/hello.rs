use rhai::{Engine, EvalAltResult};

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let result = engine.eval::<i64>("40 + 2")?;

    println!("Answer: {}", result); // prints 42

    Ok(())
}
