use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_math() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<INT>("1 + 2")?, 3);
    assert_eq!(engine.eval::<INT>("1 - 2")?, -1);
    assert_eq!(engine.eval::<INT>("2 * 3")?, 6);
    assert_eq!(engine.eval::<INT>("1 / 2")?, 0);
    assert_eq!(engine.eval::<INT>("3 % 2")?, 1);

    #[cfg(not(feature = "only_i32"))]
    assert_eq!(
        engine.eval::<INT>("(-9223372036854775807).abs()")?,
        9223372036854775807
    );

    #[cfg(feature = "only_i32")]
    assert_eq!(engine.eval::<INT>("(-2147483647).abs()")?, 2147483647);

    // Overflow/underflow/division-by-zero errors
    #[cfg(not(feature = "unchecked"))]
    {
        #[cfg(not(feature = "only_i32"))]
        {
            match engine.eval::<INT>("(-9223372036854775808).abs()") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return overflow error: {:?}", r),
            }
            match engine.eval::<INT>("9223372036854775807 + 1") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return overflow error: {:?}", r),
            }
            match engine.eval::<INT>("-9223372036854775808 - 1") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return underflow error: {:?}", r),
            }
            match engine.eval::<INT>("9223372036854775807 * 9223372036854775807") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return overflow error: {:?}", r),
            }
            match engine.eval::<INT>("9223372036854775807 / 0") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return division by zero error: {:?}", r),
            }
            match engine.eval::<INT>("9223372036854775807 % 0") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return division by zero error: {:?}", r),
            }
        }

        #[cfg(feature = "only_i32")]
        {
            match engine.eval::<INT>("2147483647 + 1") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return overflow error: {:?}", r),
            }
            match engine.eval::<INT>("-2147483648 - 1") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return underflow error: {:?}", r),
            }
            match engine.eval::<INT>("2147483647 * 2147483647") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return overflow error: {:?}", r),
            }
            match engine.eval::<INT>("2147483647 / 0") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return division by zero error: {:?}", r),
            }
            match engine.eval::<INT>("2147483647 % 0") {
                Err(EvalAltResult::ErrorArithmetic(_, _)) => (),
                r => panic!("should return division by zero error: {:?}", r),
            }
        }
    }

    Ok(())
}
