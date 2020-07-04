#[cfg(not(feature = "serde"))]
fn main() {
    println!(r#"This example requires the "serde" feature which is not enabled by default."#);
    println!("Try: cargo run --features serde --example serde");
}

#[cfg(feature = "serde")]
fn main() {
    example::ser();
    println!();
    example::de();
}

#[cfg(feature = "serde")]
mod example {
    use rhai::{de::from_dynamic, ser::to_dynamic};
    use rhai::{Dynamic, Engine, Map};
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize)]
    struct Point {
        x: f64,
        y: f64,
    }

    #[derive(Debug, Serialize, Deserialize)]
    struct MyStruct {
        a: i64,
        b: Vec<String>,
        c: bool,
        d: Point,
    }

    pub fn ser() {
        let x = MyStruct {
            a: 42,
            b: vec!["hello".into(), "world".into()],
            c: true,
            d: Point {
                x: 123.456,
                y: 999.0,
            },
        };

        println!("Source struct: {:#?}", x);

        // Convert the 'MyStruct' into a 'Dynamic'
        let map: Dynamic = to_dynamic(x).unwrap();

        assert!(map.is::<Map>());
        println!("Serialized to Dynamic: {:#?}", map);
    }

    pub fn de() {
        let engine = Engine::new();
        let result: Dynamic = engine
            .eval(
                r#"
                    #{
                        a: 42,
                        b: [ "hello", "world" ],
                        c: true,
                        d: #{ x: 123.456, y: 999.0 }
                    }
                "#,
            )
            .unwrap();

        println!("Source Dynamic: {:#?}", result);

        // Convert the 'Dynamic' object map into 'MyStruct'
        let x: MyStruct = from_dynamic(&result).unwrap();

        println!("Deserialized to struct: {:#?}", x);
    }
}
