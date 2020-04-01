#![cfg(not(feature = "no_object"))]

use rhai::{AnyExt, Engine, EvalAltResult, Map, INT};

#[test]
fn test_map_indexing() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    #[cfg(not(feature = "no_index"))]
    {
        assert_eq!(
            engine.eval::<INT>(r#"let x = #{a: 1, b: 2, c: 3}; x["b"]"#)?,
            2
        );
        assert_eq!(
            engine.eval::<char>(
                r#"
                    let y = #{d: 1, "e": #{a: 42, b: 88, "": "hello"}, " 123 xyz": 9};
                    y.e[""][4]
            "#
            )?,
            'o'
        );
    }

    assert_eq!(
        engine.eval::<INT>("let y = #{a: 1, b: 2, c: 3}; y.a = 5; y.a")?,
        5
    );
    engine.eval::<()>("let y = #{a: 1, b: 2, c: 3}; y.z")?;

    #[cfg(not(feature = "no_stdlib"))]
    {
        assert_eq!(
            engine.eval::<INT>(
                r"
                    let x = #{a: 1, b: 2, c: 3};
                    let y = #{b: 42, d: 9};
                    x.mixin(y);
                    x.len() + x.b
           "
            )?,
            46
        );
        assert_eq!(
            engine.eval::<INT>(
                r"
                    let x = #{a: 1, b: 2, c: 3};
                    x += #{b: 42, d: 9};
                    x.len() + x.b
           "
            )?,
            46
        );
        assert_eq!(
            engine
                .eval::<Map>(
                    r"
                        let x = #{a: 1, b: 2, c: 3};
                        let y = #{b: 42, d: 9};
                        x + y
           "
                )?
                .len(),
            4
        );
    }

    Ok(())
}

#[test]
fn test_map_assign() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let x = engine.eval::<Map>(r#"let x = #{a: 1, b: true, "c#": "hello"}; x"#)?;
    let a = x.get("a").cloned().unwrap();
    let b = x.get("b").cloned().unwrap();
    let c = x.get("c#").cloned().unwrap();

    assert_eq!(*a.downcast::<INT>().unwrap(), 1);
    assert_eq!(*b.downcast::<bool>().unwrap(), true);
    assert_eq!(*c.downcast::<String>().unwrap(), "hello");

    Ok(())
}

#[test]
fn test_map_return() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();

    let x = engine.eval::<Map>(r#"#{a: 1, b: true, c: "hello"}"#)?;
    let a = x.get("a").cloned().unwrap();
    let b = x.get("b").cloned().unwrap();
    let c = x.get("c").cloned().unwrap();

    assert_eq!(*a.downcast::<INT>().unwrap(), 1);
    assert_eq!(*b.downcast::<bool>().unwrap(), true);
    assert_eq!(*c.downcast::<String>().unwrap(), "hello");

    Ok(())
}
