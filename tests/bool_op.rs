use rhai::{Engine, EvalAltResult};

#[test]
fn test_bool_op1() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<bool>("true && (false || true)"), Ok(true));
    assert_eq!(engine.eval::<bool>("true & (false | true)"), Ok(true));
}

#[test]
fn test_bool_op2() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<bool>("false && (false || true)"), Ok(false));
    assert_eq!(engine.eval::<bool>("false & (false | true)"), Ok(false));
}

#[test]
fn test_bool_op3() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>("true && (false || 123)"),
        Err(EvalAltResult::ErrorBooleanArgMismatch("OR".into()))
    );

    assert_eq!(engine.eval::<bool>("true && (true || 123)"), Ok(true));

    assert_eq!(
        engine.eval::<bool>("123 && (false || true)"),
        Err(EvalAltResult::ErrorBooleanArgMismatch("AND".into()))
    );

    assert_eq!(engine.eval::<bool>("false && (true || 123)"), Ok(false));
}

#[test]
fn test_bool_op_short_circuit() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>(
            r"
            fn this() { true }
            fn that() { 9/0 }

            this() || that();
        "
        ),
        Ok(true)
    );

    assert_eq!(
        engine.eval::<bool>(
            r"
            fn this() { false }
            fn that() { 9/0 }

            this() && that();
        "
        ),
        Ok(false)
    );
}

#[test]
#[should_panic]
fn test_bool_op_no_short_circuit1() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>(
            r"
            fn this() { false }
            fn that() { 9/0 }

            this() | that();
        "
        ),
        Ok(false)
    );
}

#[test]
#[should_panic]
fn test_bool_op_no_short_circuit2() {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<bool>(
            r"
            fn this() { false }
            fn that() { 9/0 }

            this() & that();
        "
        ),
        Ok(false)
    );
}
