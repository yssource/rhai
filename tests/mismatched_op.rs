use rhai::{Engine, EvalAltResult, RegisterFn, INT};

#[test]
#[cfg(not(feature = "no_stdlib"))]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    let r = engine.eval::<INT>("60 + \"hello\"");

    match r {
        Err(EvalAltResult::ErrorMismatchOutputType(err, _)) if err == "string" => (),
        _ => panic!(),
    }
}

#[test]
fn test_mismatched_op_custom_type() {
    #[derive(Clone)]
    struct TestStruct {
        x: INT,
    }

    impl TestStruct {
        fn new() -> TestStruct {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();
    engine.register_type_with_name::<TestStruct>("TestStruct");
    engine.register_fn("new_ts", TestStruct::new);

    let r = engine.eval::<INT>("60 + new_ts()");

    match r {
        #[cfg(feature = "only_i32")]
        Err(EvalAltResult::ErrorFunctionNotFound(err, _)) if err == "+ (i32, TestStruct)" => (),

        #[cfg(not(feature = "only_i32"))]
        Err(EvalAltResult::ErrorFunctionNotFound(err, _)) if err == "+ (i64, TestStruct)" => (),

        _ => panic!(),
    }
}
