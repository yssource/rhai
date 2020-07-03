#![cfg(feature = "serde")]

use rhai::{de::from_dynamic, Dynamic, Engine, EvalAltResult, INT};
use serde::Deserialize;

#[cfg(not(feature = "no_index"))]
use rhai::Array;
#[cfg(not(feature = "no_object"))]
use rhai::Map;

#[test]
fn test_serde_de_primary_types() {
    assert_eq!(42_u16, from_dynamic(&Dynamic::from(42_u16)).unwrap());
    assert_eq!(42 as INT, from_dynamic(&(42 as INT).into()).unwrap());
    assert_eq!(true, from_dynamic(&true.into()).unwrap());
    assert_eq!((), from_dynamic(&().into()).unwrap());

    #[cfg(not(feature = "no_float"))]
    {
        assert_eq!(123.456_f64, from_dynamic(&123.456_f64.into()).unwrap());
        assert_eq!(
            123.456_f32,
            from_dynamic(&Dynamic::from(123.456_f32)).unwrap()
        );
    }

    assert_eq!(
        "hello",
        from_dynamic::<String>(&"hello".to_string().into()).unwrap()
    );
}

#[test]
#[cfg(not(feature = "no_index"))]
fn test_serde_de_array() {
    let arr: Vec<INT> = vec![123, 456, 42, 999];
    assert_eq!(arr, from_dynamic::<Vec<INT>>(&arr.clone().into()).unwrap());
}

#[test]
fn test_serde_de_struct() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Hello {
        a: INT,
        b: bool,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct Test {
        int: u32,
        seq: Vec<String>,
        obj: Hello,
    }

    let mut map = Map::new();
    map.insert("int".into(), Dynamic::from(42_u32));

    let mut map2 = Map::new();
    map2.insert("a".into(), (123 as INT).into());
    map2.insert("b".into(), true.into());

    map.insert("obj".into(), map2.into());

    let arr: Array = vec!["hello".into(), "kitty".into(), "world".into()];
    map.insert("seq".into(), arr.into());

    let expected = Test {
        int: 42,
        seq: vec!["hello".into(), "kitty".into(), "world".into()],
        obj: Hello { a: 123, b: true },
    };
    assert_eq!(expected, from_dynamic(&map.into()).unwrap());
}

#[test]
fn test_serde_de_script() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, Deserialize)]
    struct Point {
        x: f64,
        y: f64,
    }

    #[derive(Debug, Deserialize)]
    struct MyStruct {
        a: i64,
        b: Vec<String>,
        c: bool,
        d: Point,
    }

    let engine = Engine::new();

    let result: Dynamic = engine.eval(
        r#"
            #{
                a: 42,
                b: [ "hello", "world" ],
                c: true,
                d: #{ x: 123.456, y: 999.0 }
            }
        "#,
    )?;

    // Convert the 'Dynamic' object map into 'MyStruct'
    let x: MyStruct = from_dynamic(&result)?;

    Ok(())
}
