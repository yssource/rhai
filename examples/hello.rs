use rhai::{packages::*, Engine, EvalAltResult, INT};
use std::rc::Rc;

fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new_raw();
    engine.load_package(ArithmeticPackage::new().get());

    let result = engine.eval::<INT>("40 + 2")?;

    println!("Answer: {}", result); // prints 42

    Ok(())
}
