use rhai::{Engine, EvalAltResult, RegisterFn, INT};

#[derive(Clone)]
struct TestStruct {
    x: INT,
}

impl TestStruct {
    fn update(&mut self) {
        self.x += 1000;
    }

    fn new() -> Self {
        TestStruct { x: 1 }
    }
}

#[cfg(not(feature = "no_object"))]
fn main() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

    println!("result: {}", result.x); // prints 1001

    Ok(())
}

#[cfg(feature = "no_object")]
fn main() {}
