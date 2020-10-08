#![cfg(not(feature = "no_optimize"))]

use rhai::{Engine, EvalAltResult, OptimizationLevel, INT, RegisterFn};

#[test]
fn test_optimizer_run() -> Result<(), Box<EvalAltResult>> {
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

    // Override == operator
    engine.register_fn("==", |_x: INT, _y: INT| false);

    engine.set_optimization_level(OptimizationLevel::Simple);

    assert_eq!(
        engine.eval::<INT>(r"if 1 == 1 || 2 > 3 { 42 } else { 123 }")?,
        123
    );

    engine.set_optimization_level(OptimizationLevel::Full);

    assert_eq!(
        engine.eval::<INT>(r"if 1 == 1 || 2 > 3 { 42 } else { 123 }")?,
        123
    );

    Ok(())
}

#[test]
fn test_optimizer_parse() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    engine.set_optimization_level(OptimizationLevel::Simple);

    let ast = engine.compile("{ const DECISION = false; if DECISION { 42 } }")?;

    assert!(format!("{:?}", ast).starts_with("AST([], Module("));

    engine.set_optimization_level(OptimizationLevel::Full);

    let ast = engine.compile("if 1 == 2 { 42 }")?;

    assert!(format!("{:?}", ast).starts_with("AST([], Module("));

    Ok(())
}
