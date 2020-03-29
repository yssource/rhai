#![cfg(not(feature = "no_object"))]
#![cfg(not(feature = "no_index"))]

use rhai::{AnyExt, Dynamic, Engine, EvalAltResult, Map, RegisterFn, INT};

#[test]
fn test_map_indexing() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(r#"let x = ${a: 1, b: 2, c: 3}; x["b"]"#)?,
        2
    );
    assert_eq!(
        engine.eval::<INT>("let y = ${a: 1, b: 2, c: 3}; y.a = 5; y.a")?,
        5
    );
    assert_eq!(
        engine.eval::<char>(
            r#"
                let y = ${d: 1, e: ${a: 42, b: 88, c: "93"}, x: 9};
                y.e["c"][1]
            "#
        )?,
        '3'
    );

    engine.eval::<()>("let y = ${a: 1, b: 2, c: 3}; y.z")?;

    Ok(())
}

#[test]
fn test_map_assign() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let mut x = engine.eval::<Map>("let x = ${a: 1, b: true, c: \"3\"}; x")?;
    let box_a = x.remove("a").unwrap();
    let box_b = x.remove("b").unwrap();
    let box_c = x.remove("c").unwrap();

    assert_eq!(*box_a.downcast::<INT>().unwrap(), 1);
    assert_eq!(*box_b.downcast::<bool>().unwrap(), true);
    assert_eq!(*box_c.downcast::<String>().unwrap(), "3");

    Ok(())
}

#[test]
fn test_map_return() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let mut x = engine.eval::<Map>("${a: 1, b: true, c: \"3\"}")?;
    let box_a = x.remove("a").unwrap();
    let box_b = x.remove("b").unwrap();
    let box_c = x.remove("c").unwrap();

    assert_eq!(*box_a.downcast::<INT>().unwrap(), 1);
    assert_eq!(*box_b.downcast::<bool>().unwrap(), true);
    assert_eq!(*box_c.downcast::<String>().unwrap(), "3".to_string());

    Ok(())
}
