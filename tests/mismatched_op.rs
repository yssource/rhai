use rhai::{Engine, EvalAltResult, RegisterFn};

#[test]
fn test_mismatched_op() {
    let mut engine = Engine::new();

    let r = engine.eval::<i64>("60 + \"hello\"");

    match r {
        Err(EvalAltResult::ErrorMismatchOutputType(err, _)) if err == "alloc::string::String" => (),
        _ => panic!(),
    }
}

#[test]
fn test_mismatched_op_custom_type() {
    #[derive(Clone)]
    struct TestStruct {
        x: i64,
    }

    impl TestStruct {
        fn new() -> TestStruct {
            TestStruct { x: 1 }
        }
    }

    let mut engine = Engine::new();
    engine.register_type::<TestStruct>();
    engine.register_fn("new_ts", TestStruct::new);

    let r = engine.eval::<i64>("60 + new_ts()");

    match r {
        Err(EvalAltResult::ErrorFunctionNotFound(err, _))
            if err == "+ (i64, mismatched_op::test_mismatched_op_custom_type::TestStruct)" =>
        {
            ()
        }
        _ => panic!(),
    }
}
