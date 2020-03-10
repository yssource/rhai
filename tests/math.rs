use rhai::{Engine, EvalAltResult};

#[test]
fn test_math() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("1 + 2")?, 3);
    assert_eq!(engine.eval::<i64>("1 - 2")?, -1);
    assert_eq!(engine.eval::<i64>("2 * 3")?, 6);
    assert_eq!(engine.eval::<i64>("1 / 2")?, 0);
    assert_eq!(engine.eval::<i64>("3 % 2")?, 1);
    assert_eq!(
        engine.eval::<i64>("(-9223372036854775807).abs()")?,
        9223372036854775807
    );

    // Overflow/underflow/division-by-zero errors
    #[cfg(not(feature = "unchecked"))]
    {
        match engine.eval::<i64>("9223372036854775807 + 1") {
            Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
            r => panic!("should return overflow error: {:?}", r),
        }
        match engine.eval::<i64>("-9223372036854775808 - 1") {
            Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
            r => panic!("should return underflow error: {:?}", r),
        }
        match engine.eval::<i64>("9223372036854775807 * 9223372036854775807") {
            Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
            r => panic!("should return overflow error: {:?}", r),
        }
        match engine.eval::<i64>("9223372036854775807 / 0") {
            Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
            r => panic!("should return division by zero error: {:?}", r),
        }
        match engine.eval::<i64>("9223372036854775807 % 0") {
            Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
            r => panic!("should return division by zero error: {:?}", r),
        }
    }

    Ok(())
}
