use rhai::{Engine, EvalAltResult, RegisterFn};

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    fn add(x: i64, y: i64) -> i64 {
        x + y
    }

    engine.register_fn("add", add);

    let result = engine.eval::<i64>("add(40, 2)")?;

    println!("Answer: {}", result); // prints 42

    Ok(())
}
