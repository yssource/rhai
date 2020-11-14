use rhai::{Engine, EvalAltResult, Scope, INT};

#[test]
fn test_switch() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();
    let mut scope = Scope::new();
    scope.push("x", 42 as INT);

    assert_eq!(
        engine.eval_with_scope::<bool>(&mut scope, "switch x { 1 => (), 2 => 'a', 42 => true }")?,
        true
    );
    assert_eq!(
        engine.eval_with_scope::<bool>(&mut scope, "switch x { 1 => (), 2 => 'a', _ => true }")?,
        true
    );
    assert_eq!(
        engine.eval_with_scope::<()>(&mut scope, "switch x { 1 => 123, 2 => 'a' }")?,
        ()
    );
    assert_eq!(
        engine.eval_with_scope::<INT>(
            &mut scope,
            "switch x { 1 => 123, 42 => { x / 2 }, _ => 999 }"
        )?,
        21
    );
    #[cfg(not(feature = "no_index"))]
    assert_eq!(
        engine.eval_with_scope::<INT>(
            &mut scope,
            r"
                let y = [1, 2, 3];

                switch y {
                    42 => 1,
                    true => 2,
                    [1, 2, 3] => 3,
                    _ => 9
                }
            "
        )?,
        3
    );
    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval_with_scope::<INT>(
            &mut scope,
            r"
                let y = #{a:1, b:true, c:'x'};

                switch y {
                    42 => 1,
                    true => 2,
                    #{b:true, c:'x', a:1} => 3,
                    _ => 9
                }
            "
        )?,
        3
    );

    Ok(())
}

#[cfg(not(feature = "no_index"))]
#[cfg(not(feature = "no_object"))]
mod test_switch_enum {
    use super::*;
    use rhai::Array;
    #[derive(Debug, Clone)]
    enum MyEnum {
        Foo,
        Bar(INT),
        Baz(String, bool),
    }

    impl MyEnum {
        fn get_enum_data(&mut self) -> Array {
            match self {
                Self::Foo => vec!["Foo".into()] as Array,
                Self::Bar(num) => vec!["Bar".into(), (*num).into()] as Array,
                Self::Baz(name, option) => {
                    vec!["Baz".into(), name.clone().into(), (*option).into()] as Array
                }
            }
        }
    }

    #[test]
    fn test_switch_enum() -> Result<(), Box<EvalAltResult>> {
        let mut engine = Engine::new();

        engine
            .register_type_with_name::<MyEnum>("MyEnum")
            .register_get("get_data", MyEnum::get_enum_data);

        let mut scope = Scope::new();
        scope.push("x", MyEnum::Baz("hello".to_string(), true));

        assert_eq!(
            engine.eval_with_scope::<INT>(
                &mut scope,
                r#"
                    switch x.get_data {
                        ["Foo"] => 1,
                        ["Bar", 42] => 2,
                        ["Bar", 123] => 3,
                        ["Baz", "hello", false] => 4,
                        ["Baz", "hello", true] => 5,
                        _ => 9
                    }
                "#
            )?,
            5
        );

        Ok(())
    }
}
