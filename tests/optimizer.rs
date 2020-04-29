#![cfg(not(feature = "no_optimize"))]

use rhai::{Engine, EvalAltResult, OptimizationLevel, INT};

#[test]
fn test_optimizer() -> Result<(), Box<EvalAltResult>> {
    fn run_test(engine: &mut Engine) -> Result<(), Box<EvalAltResult>> {
        assert_eq!(engine.eval::<INT>(r"if true { 42 } else { 123 }")?, 42);
        assert_eq!(
            engine.eval::<INT>(r"if 1 == 1 || 2 > 3 { 42 } else { 123 }")?,
            42
        );
        assert_eq!(
            engine.eval::<INT>(r#"const abc = "hello"; if abc < "foo" { 42 } else { 123 }"#)?,
            123
        );
        Ok(())
    }

    let mut engine = Engine::new();

    engine.set_optimization_level(OptimizationLevel::None);
    run_test(&mut engine)?;

    engine.set_optimization_level(OptimizationLevel::Simple);
    run_test(&mut engine)?;

    engine.set_optimization_level(OptimizationLevel::Full);
    run_test(&mut engine)?;

    Ok(())
}
