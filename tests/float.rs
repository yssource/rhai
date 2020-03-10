#![cfg(not(feature = "no_float"))]
use rhai::{Engine, EvalAltResult, RegisterFn};

#[test]
fn test_float() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>("let x = 0.0; let y = 1.0; x < y")?,
        true
    );
    assert_eq!(
        engine.eval::<bool>("let x = 0.0; let y = 1.0; x > y")?,
        false
    );
    assert_eq!(engine.eval::<f64>("let x = 9.9999; x")?, 9.9999);

    Ok(())
}

#[test]
fn struct_with_float() -> Result<(), EvalAltResult> {
    #[derive(Clone)]
    struct TestStruct {
        x: f64,
    }

    impl TestStruct {
        fn update(&mut self) {
            self.x += 5.789_f64;
        }

        fn get_x(&mut self) -> f64 {
            self.x
        }

        fn set_x(&mut self, new_x: f64) {
            self.x = new_x;
        }

        fn new() -> TestStruct {
            TestStruct { x: 1.0 }
        }
    }

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    assert_eq!(
        engine.eval::<f64>("let ts = new_ts(); ts.update(); ts.x")?,
        6.789
    );
    assert_eq!(
        engine.eval::<f64>("let ts = new_ts(); ts.x = 10.1001; ts.x")?,
        10.1001
    );

    Ok(())
}
