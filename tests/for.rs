use rhai::{Engine, EvalAltResult};

#[test]
fn test_for() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let script = r"
        let sum1 = 0;
        let sum2 = 0;
        let inputs = [1, 2, 3, 4, 5];

        for x in inputs {
            sum1 += x;
        }

        for x in range(1, 6) {
            sum2 += x;
        }

        sum1 + sum2
    ";

    assert_eq!(engine.eval::<i64>(script)?, 30);

    Ok(())
}
