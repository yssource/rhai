use rhai::Engine;

#[test]
fn test_if() {
    let mut engine = Engine::new();

    assert_eq!(engine.eval::<i64>("if true { 55 }"), Ok(55));
    assert_eq!(engine.eval::<i64>("if false { 55 } else { 44 }"), Ok(44));
    assert_eq!(engine.eval::<i64>("if true { 55 } else { 44 }"), Ok(55));
    assert_eq!(
        engine.eval::<i64>("if false { 55 } else if true { 33 } else { 44 }"),
        Ok(33)
    );
    assert_eq!(
        engine.eval::<i64>(
            r"
                if false { 55 }
                else if false { 33 }
                else if false { 66 }
                else if false { 77 }
                else if false { 88 }
                else { 44 }
        "
        ),
        Ok(44)
    );
}
