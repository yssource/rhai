use rhai::{Engine, EvalAltResult, Scope};

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    engine.eval_with_scope::<()>(&mut scope, false, "let x = 4 + 5")?;

    let result = engine.eval_with_scope::<i64>(&mut scope, false, "x")?;

    println!("result: {}", result);

    Ok(())
}
