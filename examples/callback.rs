//! This example stores a Rhai closure for later use as a callback.

use rhai::{Engine, EvalAltResult, FnPtr};

fn main() -> Result<(), Box<EvalAltResult>> {
    // This script creates a closure which may capture variables.
    let script = "
        let x = 20;

        // The following closure captures 'x'
        return |a, b| (x + a) * b;
    ";

    // To call a Rhai closure at a later time, you'd need three things:
    // 1) an `Engine` (with all needed functions registered),
    // 2) a compiled `AST`,
    // 3) the closure (of type `FnPtr`).
    let engine = Engine::new();

    let ast = engine.compile(script)?;

    let closure = engine.eval_ast::<FnPtr>(&ast)?;

    // Create a closure that we can call any time, encapsulating the
    // `Engine`, `AST` and `FnPtr`.
    let func = move |x: i64, y: i64| -> Result<i64, _> { closure.call(&engine, &ast, (x, y)) };

    // Now we can call `func` anywhere just like a normal function!
    let result = func(1, 2)?;

    println!("The Answer: {}", result); // prints 42

    Ok(())
}
