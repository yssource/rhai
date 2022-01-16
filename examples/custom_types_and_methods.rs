#![cfg(not(feature = "no_object"))]

use rhai::{Engine, EvalAltResult};

#[derive(Debug, Clone)]
struct TestStruct {
    x: i64,
}

impl TestStruct {
    pub fn update(&mut self) {
        self.x += 1000;
    }

    pub fn new() -> Self {
        Self { x: 1 }
    }
}

fn main() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine
        .register_type::<TestStruct>()
        .register_fn("new_ts", TestStruct::new)
        .register_fn("update", TestStruct::update);

    let result = engine.eval::<TestStruct>(
        "
            let x = new_ts();
            x.update();
            x
        ",
    )?;

    println!("result: {}", result.x); // prints 1001

    Ok(())
}
