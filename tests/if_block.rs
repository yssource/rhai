use rhai::{Engine, EvalAltResult, INT};

#[test]
fn test_if() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<INT>("if true { 55 }")?, 55);
    assert_eq!(engine.eval::<INT>("if false { 55 } else { 44 }")?, 44);
    assert_eq!(engine.eval::<INT>("if true { 55 } else { 44 }")?, 55);
    assert_eq!(
        engine.eval::<INT>("if false { 55 } else if true { 33 } else { 44 }")?,
        33
    );
    assert_eq!(
        engine.eval::<INT>(
            r"
                if false { 55 }
                else if false { 33 }
                else if false { 66 }
                else if false { 77 }
                else if false { 88 }
                else { 44 }
        "
        )?,
        44
    );

    Ok(())
}
