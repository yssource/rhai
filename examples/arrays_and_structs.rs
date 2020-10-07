use rhai::{Engine, RegisterFn, INT};

#[derive(Clone, Debug)]
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

#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
fn main() {
    let mut engine = Engine::new();

    engine
        .register_type::<TestStruct>()
        .register_fn("update", TestStruct::update)
        .register_fn("new_ts", TestStruct::new);

    println!(
        "{:?}",
        engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")
    );
    println!(
        "{:?}",
        engine.eval::<TestStruct>("let x = [new_ts()]; x[0].update(); x[0]")
    );
}

#[cfg(any(feature = "no_index", feature = "no_object"))]
fn main() {}
