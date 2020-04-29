use rhai::{Engine, EvalAltResult, Scope, INT};

fn main() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();
    let mut scope = Scope::new();

    engine.eval_with_scope::<()>(&mut scope, "let x = 4 + 5")?;

    let result = engine.eval_with_scope::<INT>(&mut scope, "x")?;

    println!("result: {}", result);

    Ok(())
}
