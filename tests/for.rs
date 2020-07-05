use rhai::{Engine, EvalAltResult, INT};

#[cfg(not(feature = "no_index"))]
#[test]
fn test_for_array() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

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

        for x in range(1, 6, 3) {
            sum2 += x;
        }

        sum1 + sum2
    ";

    assert_eq!(engine.eval::<INT>(script)?, 35);

    Ok(())
}

#[test]
fn test_for_string() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    let script = r#"
        let s = "hello";
        let sum = 0;

        for ch in s {
            sum += to_int(ch);
        }

        sum
    "#;

    assert_eq!(engine.eval::<INT>(script)?, 532);

    Ok(())
}

#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "no_index"))]
#[test]
fn test_for_object() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    let script = r#"
        let sum = 0;
        let keys = "";
        let map = #{a: 1, b: 2, c: 3};

        for key in keys(map) {
            keys += key;
        }
        for value in values(map) {
            sum += value;
        }

        keys.len + sum
    "#;

    assert_eq!(engine.eval::<INT>(script)?, 9);

    Ok(())
}
