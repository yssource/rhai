use rhai::{Dynamic, Engine, EvalAltResult, ImmutableString, RegisterFn, Scope, INT};

#[test]
fn test_string() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<String>(r#""Test string: \u2764""#)?,
        "Test string: ❤"
    );
    assert_eq!(
        engine.eval::<String>(r#""Test string: \x58""#)?,
        "Test string: X"
    );
    assert_eq!(engine.eval::<String>(r#""\"hello\"""#)?, r#""hello""#);

    assert_eq!(engine.eval::<String>(r#""foo" + "bar""#)?, "foobar");

    assert!(engine.eval::<bool>(r#"let y = "hello, world!"; "world" in y"#)?);
    assert!(engine.eval::<bool>(r#"let y = "hello, world!"; 'w' in y"#)?);
    assert!(!engine.eval::<bool>(r#"let y = "hello, world!"; "hey" in y"#)?);

    assert_eq!(engine.eval::<String>(r#""foo" + 123"#)?, "foo123");

    #[cfg(not(feature = "no_object"))]
    assert_eq!(engine.eval::<String>("to_string(42)")?, "42");

    #[cfg(not(feature = "no_index"))]
    assert_eq!(engine.eval::<char>(r#"let y = "hello"; y[1]"#)?, 'e');

    #[cfg(not(feature = "no_object"))]
    assert_eq!(engine.eval::<INT>(r#"let y = "hello"; y.len"#)?, 5);

    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<INT>(r#"let y = "hello"; y.clear(); y.len"#)?,
        0
    );

    assert_eq!(engine.eval::<INT>(r#"let y = "hello"; len(y)"#)?, 5);

    #[cfg(not(feature = "no_object"))]
    #[cfg(not(feature = "no_index"))]
    assert_eq!(engine.eval::<char>(r#"let y = "hello"; y[y.len-1]"#)?, 'o');

    #[cfg(not(feature = "no_float"))]
    assert_eq!(engine.eval::<String>(r#""foo" + 123.4556"#)?, "foo123.4556");

    Ok(())
}

#[test]
fn test_string_dynamic() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();
    let mut scope = Scope::new();
    scope.push("x", "foo");
    scope.push("y", "foo");
    scope.push("z", "foo");

    assert!(engine.eval_with_scope::<bool>(&mut scope, r#"x == "foo""#)?);
    assert!(engine.eval_with_scope::<bool>(&mut scope, r#"y == "foo""#)?);
    assert!(engine.eval_with_scope::<bool>(&mut scope, r#"z == "foo""#)?);

    Ok(())
}

#[cfg(not(feature = "no_object"))]
#[test]
fn test_string_substring() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.sub_string(-1, 2)"#
        )?,
        "❤❤"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.sub_string(1, 5)"#
        )?,
        "❤❤ he"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.sub_string(1)"#
        )?,
        "❤❤ hello! ❤❤❤"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.sub_string(99)"#
        )?,
        ""
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.sub_string(1, -1)"#
        )?,
        ""
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.sub_string(1, 999)"#
        )?,
        "❤❤ hello! ❤❤❤"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.crop(1, -1); x"#
        )?,
        ""
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.crop(4, 6); x"#
        )?,
        "hello!"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.crop(1, 999); x"#
        )?,
        "❤❤ hello! ❤❤❤"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x -= 'l'; x"#
        )?,
        "❤❤❤ heo! ❤❤❤"
    );

    assert_eq!(
        engine.eval::<String>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x -= "\u2764\u2764"; x"#
        )?,
        "❤ hello! ❤"
    );
    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.index_of('\u2764')"#
        )?,
        0
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.index_of('\u2764', 5)"#
        )?,
        11
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.index_of('\u2764', -1)"#
        )?,
        0
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.index_of('\u2764', 999)"#
        )?,
        -1
    );

    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.index_of('x')"#
        )?,
        -1
    );

    Ok(())
}

#[test]
fn test_string_fn() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine.register_fn("set_to_x", |ch: &mut char| *ch = 'X');

    #[cfg(not(feature = "no_index"))]
    #[cfg(not(feature = "no_object"))]
    assert_eq!(
        engine.eval::<String>(r#"let x="foo"; x[0].set_to_x(); x"#)?,
        "Xoo"
    );
    #[cfg(not(feature = "no_index"))]
    assert_eq!(
        engine.eval::<String>(r#"let x="foo"; set_to_x(x[0]); x"#)?,
        "foo"
    );

    engine
        .register_fn("foo1", |s: &str| s.len() as INT)
        .register_fn("foo2", |s: ImmutableString| s.len() as INT)
        .register_fn("foo3", |s: String| s.len() as INT)
        .register_fn("foo4", |s: &mut ImmutableString| s.len() as INT);

    assert_eq!(engine.eval::<INT>(r#"foo1("hello")"#)?, 5);
    assert_eq!(engine.eval::<INT>(r#"foo2("hello")"#)?, 5);
    assert_eq!(engine.eval::<INT>(r#"foo3("hello")"#)?, 5);
    assert_eq!(engine.eval::<INT>(r#"foo4("hello")"#)?, 5);

    Ok(())
}

#[cfg(not(feature = "no_object"))]
#[cfg(not(feature = "no_index"))]
#[test]
fn test_string_split() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.split(' ').len"#
        )?,
        3
    );
    assert_eq!(
        engine.eval::<INT>(
            r#"let x = "\u2764\u2764\u2764 hello! \u2764\u2764\u2764"; x.split("hello").len"#
        )?,
        2
    );

    Ok(())
}
