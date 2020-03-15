use rhai::{Engine, EvalAltResult, RegisterFn, INT};

#[test]
#[cfg(not(feature = "no_stdlib"))]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    assert!(
        matches!(engine.eval::<INT>(r#"60 + "hello""#).expect_err("expects error"),
        EvalAltResult::ErrorMismatchOutputType(err, _) if err == "string")
    );
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

    let r = engine
        .eval::<INT>("60 + new_ts()")
        .expect_err("expects error");

    #[cfg(feature = "only_i32")]
    assert!(
        matches!(r, EvalAltResult::ErrorFunctionNotFound(err, _) if err == "+ (i32, TestStruct)")
    );

    #[cfg(not(feature = "only_i32"))]
    assert!(
        matches!(r, EvalAltResult::ErrorFunctionNotFound(err, _) if err == "+ (i64, TestStruct)")
    );
}
