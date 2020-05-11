#![cfg(not(feature = "no_object"))]

use rhai::{Engine, EvalAltResult, RegisterFn, INT};

#[test]
fn test_method_call() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, Clone, Eq, PartialEq)]
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

    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    assert_eq!(
        engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?,
        TestStruct { x: 1001 }
    );

    assert_eq!(
        engine.eval::<TestStruct>("let x = new_ts(); update(x); x")?,
        TestStruct { x: 1 }
    );

    Ok(())
}

#[test]
fn test_method_call_style() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<INT>("let x = -123; x.abs(); x")?, -123);

    Ok(())
}
