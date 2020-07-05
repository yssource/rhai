#![cfg(feature = "serde")]

use rhai::{de::from_dynamic, ser::to_dynamic, Dynamic, Engine, EvalAltResult, INT};
use serde::{Deserialize, Serialize};

#[cfg(not(feature = "no_index"))]
use rhai::Array;
#[cfg(not(feature = "no_object"))]
use rhai::Map;

#[test]
fn test_serde_ser_primary_types() -> Result<(), Box<EvalAltResult>> {
    assert_eq!(
        to_dynamic(42_u64)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(to_dynamic(u64::MAX)?.type_name(), "u64");
    assert_eq!(
        to_dynamic(42 as INT)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(to_dynamic(true)?.type_name(), "bool");
    assert_eq!(to_dynamic(())?.type_name(), "()");

    #[cfg(not(feature = "no_float"))]
    {
        assert_eq!(to_dynamic(123.456_f64)?.type_name(), "f64");
        assert_eq!(to_dynamic(123.456_f32)?.type_name(), "f32");
    }

    assert_eq!(to_dynamic("hello".to_string())?.type_name(), "string");

    Ok(())
}

#[test]
fn test_serde_ser_integer_types() -> Result<(), Box<EvalAltResult>> {
    assert_eq!(to_dynamic(42_i8)?.type_name(), std::any::type_name::<INT>());
    assert_eq!(
        to_dynamic(42_i16)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(
        to_dynamic(42_i32)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(
        to_dynamic(42_i64)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(to_dynamic(42_u8)?.type_name(), std::any::type_name::<INT>());
    assert_eq!(
        to_dynamic(42_u16)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(
        to_dynamic(42_u32)?.type_name(),
        std::any::type_name::<INT>()
    );
    assert_eq!(
        to_dynamic(42_u64)?.type_name(),
        std::any::type_name::<INT>()
    );

    Ok(())
}

#[test]
#[cfg(not(feature = "no_index"))]
fn test_serde_ser_array() -> Result<(), Box<EvalAltResult>> {
    let arr: Vec<INT> = vec![123, 456, 42, 999];

    let d = to_dynamic(arr)?;
    assert!(d.is::<Array>());
    assert_eq!(d.cast::<Array>().len(), 4);

    Ok(())
}

#[test]
#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
fn test_serde_ser_struct() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, Serialize, PartialEq)]
    struct Hello {
        a: INT,
        b: bool,
    }

    #[derive(Debug, Serialize, PartialEq)]
    struct Test {
        int: u32,
        seq: Vec<String>,
        obj: Hello,
    }

    let x = Test {
        int: 42,
        seq: vec!["hello".into(), "kitty".into(), "world".into()],
        obj: Hello { a: 123, b: true },
    };

    let d = to_dynamic(x)?;

    assert!(d.is::<Map>());

    let mut map = d.cast::<Map>();
    let mut obj = map.remove("obj").unwrap().cast::<Map>();
    let mut seq = map.remove("seq").unwrap().cast::<Array>();

    assert_eq!(obj.remove("a").unwrap().cast::<INT>(), 123);
    assert!(obj.remove("b").unwrap().cast::<bool>());
    assert_eq!(map.remove("int").unwrap().cast::<INT>(), 42);
    assert_eq!(seq.len(), 3);
    assert_eq!(seq.remove(1).cast::<String>(), "kitty");

    Ok(())
}

#[test]
fn test_serde_de_primary_types() -> Result<(), Box<EvalAltResult>> {
    assert_eq!(42_u16, from_dynamic(&Dynamic::from(42_u16))?);
    assert_eq!(42 as INT, from_dynamic(&(42 as INT).into())?);
    assert_eq!(true, from_dynamic(&true.into())?);
    assert_eq!((), from_dynamic(&().into())?);

    #[cfg(not(feature = "no_float"))]
    {
        assert_eq!(123.456_f64, from_dynamic(&123.456_f64.into())?);
        assert_eq!(123.456_f32, from_dynamic(&Dynamic::from(123.456_f32))?);
    }

    assert_eq!(
        "hello",
        from_dynamic::<String>(&"hello".to_string().into())?
    );

    Ok(())
}

#[test]
fn test_serde_de_integer_types() -> Result<(), Box<EvalAltResult>> {
    assert_eq!(42_i8, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_i16, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_i32, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_i64, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_u8, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_u16, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_u32, from_dynamic(&Dynamic::from(42 as INT))?);
    assert_eq!(42_u64, from_dynamic(&Dynamic::from(42 as INT))?);

    Ok(())
}

#[test]
#[cfg(not(feature = "no_index"))]
fn test_serde_de_array() -> Result<(), Box<EvalAltResult>> {
    let arr: Vec<INT> = vec![123, 456, 42, 999];
    assert_eq!(arr, from_dynamic::<Vec<INT>>(&arr.clone().into())?);
    Ok(())
}

#[test]
#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
fn test_serde_de_struct() -> Result<(), Box<EvalAltResult>> {
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
    assert_eq!(expected, from_dynamic(&map.into())?);

    Ok(())
}

#[test]
#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "no_float"))]
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
    let _: MyStruct = from_dynamic(&result)?;

    Ok(())
}

#[test]
fn test_serde_de_unit_enum() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, PartialEq, Deserialize)]
    enum MyEnum {
        VariantFoo,
        VariantBar,
    }

    {
        let d = Dynamic::from("VariantFoo".to_string());
        assert_eq!(MyEnum::VariantFoo, from_dynamic(&d)?);
    }

    {
        let d = Dynamic::from("VariantBar".to_string());
        assert_eq!(MyEnum::VariantBar, from_dynamic(&d)?);
    }

    Ok(())
}

#[test]
#[cfg(not(feature = "no_object"))]
fn test_serde_de_externally_tagged_enum() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(deny_unknown_fields)]
    enum MyEnum {
        VariantUnit,
        #[cfg(not(feature = "no_index"))]
        VariantUnitTuple(),
        VariantNewtype(i32),
        #[cfg(not(feature = "no_index"))]
        VariantTuple(i32, i32),
        VariantEmptyStruct {},
        VariantStruct {
            a: i32,
        },
    }

    {
        let d = Dynamic::from("VariantUnit".to_string());
        assert_eq!(MyEnum::VariantUnit, from_dynamic(&d).unwrap());
    }

    #[cfg(not(feature = "no_index"))]
    {
        let array: Array = vec![];
        let mut map_outer = Map::new();
        map_outer.insert("VariantUnitTuple".into(), array.into());
        assert_eq!(
            MyEnum::VariantUnitTuple(),
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    {
        let mut map_outer = Map::new();
        map_outer.insert("VariantNewtype".into(), (123 as INT).into());
        assert_eq!(
            MyEnum::VariantNewtype(123),
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    #[cfg(not(feature = "no_index"))]
    {
        let array: Array = vec![(123 as INT).into(), (456 as INT).into()];
        let mut map_outer = Map::new();
        map_outer.insert("VariantTuple".into(), array.into());
        assert_eq!(
            MyEnum::VariantTuple(123, 456),
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    {
        let map_inner = Map::new();
        let mut map_outer = Map::new();
        map_outer.insert("VariantEmptyStruct".into(), map_inner.into());
        assert_eq!(
            MyEnum::VariantEmptyStruct {},
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    {
        let mut map_inner = Map::new();
        map_inner.insert("a".into(), (123 as INT).into());
        let mut map_outer = Map::new();
        map_outer.insert("VariantStruct".into(), map_inner.into());
        assert_eq!(
            MyEnum::VariantStruct { a: 123 },
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    Ok(())
}

#[test]
#[cfg(not(feature = "no_object"))]
fn test_serde_de_internally_tagged_enum() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(tag = "tag", deny_unknown_fields)]
    enum MyEnum {
        VariantEmptyStruct {},
        VariantStruct { a: i32 },
    }

    {
        let mut map = Map::new();
        map.insert("tag".into(), "VariantStruct".into());
        map.insert("a".into(), (123 as INT).into());
        assert_eq!(
            MyEnum::VariantStruct { a: 123 },
            from_dynamic(&map.into()).unwrap()
        );
    }

    {
        let mut map = Map::new();
        map.insert("tag".into(), "VariantEmptyStruct".into());
        assert_eq!(
            MyEnum::VariantEmptyStruct {},
            from_dynamic(&map.into()).unwrap()
        );
    }

    Ok(())
}

#[test]
#[cfg(not(feature = "no_object"))]
fn test_serde_de_adjacently_tagged_enum() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(tag = "tag", content = "content", deny_unknown_fields)]
    enum MyEnum {
        VariantUnit,
        #[cfg(not(feature = "no_index"))]
        VariantUnitTuple(),
        VariantNewtype(i32),
        #[cfg(not(feature = "no_index"))]
        VariantTuple(i32, i32),
        VariantEmptyStruct {},
        VariantStruct {
            a: i32,
        },
    }

    {
        let mut map_outer = Map::new();
        map_outer.insert("tag".into(), "VariantUnit".into());
        assert_eq!(
            MyEnum::VariantUnit,
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    #[cfg(not(feature = "no_index"))]
    {
        let array: Array = vec![];
        let mut map_outer = Map::new();
        map_outer.insert("tag".into(), "VariantUnitTuple".into());
        map_outer.insert("content".into(), array.into());
        assert_eq!(
            MyEnum::VariantUnitTuple(),
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    {
        let mut map_outer = Map::new();
        map_outer.insert("tag".into(), "VariantNewtype".into());
        map_outer.insert("content".into(), (123 as INT).into());
        assert_eq!(
            MyEnum::VariantNewtype(123),
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    #[cfg(not(feature = "no_index"))]
    {
        let array: Array = vec![(123 as INT).into(), (456 as INT).into()];
        let mut map_outer = Map::new();
        map_outer.insert("tag".into(), "VariantTuple".into());
        map_outer.insert("content".into(), array.into());
        assert_eq!(
            MyEnum::VariantTuple(123, 456),
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    {
        let map_inner = Map::new();
        let mut map_outer = Map::new();
        map_outer.insert("tag".into(), "VariantEmptyStruct".into());
        map_outer.insert("content".into(), map_inner.into());
        assert_eq!(
            MyEnum::VariantEmptyStruct {},
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    {
        let mut map_inner = Map::new();
        map_inner.insert("a".into(), (123 as INT).into());
        let mut map_outer = Map::new();
        map_outer.insert("tag".into(), "VariantStruct".into());
        map_outer.insert("content".into(), map_inner.into());
        assert_eq!(
            MyEnum::VariantStruct { a: 123 },
            from_dynamic(&map_outer.into()).unwrap()
        );
    }

    Ok(())
}

#[test]
#[cfg(not(feature = "no_object"))]
fn test_serde_de_untagged_enum() -> Result<(), Box<EvalAltResult>> {
    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(untagged, deny_unknown_fields)]
    enum MyEnum {
        VariantEmptyStruct {},
        VariantStruct1 { a: i32 },
        VariantStruct2 { b: i32 },
    }

    {
        let map = Map::new();
        assert_eq!(
            MyEnum::VariantEmptyStruct {},
            from_dynamic(&map.into()).unwrap()
        );
    }

    {
        let mut map = Map::new();
        map.insert("a".into(), (123 as INT).into());
        assert_eq!(
            MyEnum::VariantStruct1 { a: 123 },
            from_dynamic(&map.into()).unwrap()
        );
    }

    {
        let mut map = Map::new();
        map.insert("b".into(), (123 as INT).into());
        assert_eq!(
            MyEnum::VariantStruct2 { b: 123 },
            from_dynamic(&map.into()).unwrap()
        );
    }

    Ok(())
}
