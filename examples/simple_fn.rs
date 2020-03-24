use rhai::{Engine, EvalAltResult, RegisterFn, INT};

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    fn add(x: INT, y: INT) -> INT {
        x + y
    }

    engine.register_fn("add", add);

    let result = engine.eval::<INT>("add(40, 2)")?;

    println!("Answer: {}", result); // prints 42

    Ok(())
}
