use rhai::{Engine, EvalAltResult, RegisterFn, INT};

#[test]
fn test_mismatched_op() {
    let engine = Engine::new();

    assert!(matches!(
        *engine.eval::<INT>(r#""hello, " + "world!""#).expect_err("expects error"),
        EvalAltResult::ErrorMismatchOutputType(need, actual, _) if need == std::any::type_name::<INT>() && actual == "string"
    ));
}

#[test]
#[cfg(not(feature = "no_object"))]
fn test_mismatched_op_custom_type() {
    #[derive(Debug, Clone)]
    struct TestStruct {
        x: INT,
    }

    impl TestStruct {
        fn new() -> Self {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();
    engine.register_type_with_name::<TestStruct>("TestStruct");
    engine.register_fn("new_ts", TestStruct::new);

    assert!(matches!(
        *engine.eval::<INT>("60 + new_ts()").expect_err("should error"),
        EvalAltResult::ErrorFunctionNotFound(err, _) if err == format!("+ ({}, TestStruct)", std::any::type_name::<INT>())
    ));

    assert!(matches!(
        *engine.eval::<TestStruct>("42").expect_err("should error"),
        EvalAltResult::ErrorMismatchOutputType(need, actual, _)
            if need == "TestStruct" && actual == std::any::type_name::<INT>()
    ));
}
