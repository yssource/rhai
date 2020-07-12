use rhai::{Engine, EvalAltResult};
use std::sync::{Arc, RwLock};

#[test]
fn test_print() -> Result<(), Box<EvalAltResult>> {
    let logbook = Arc::new(RwLock::new(Vec::<String>::new()));

    // Redirect print/debug output to 'log'
    let log1 = logbook.clone();
    let log2 = logbook.clone();

    let mut engine = Engine::new();

    engine
        .on_print(move |s| log1.write().unwrap().push(format!("entry: {}", s)))
        .on_debug(move |s| log2.write().unwrap().push(format!("DEBUG: {}", s)));

    // Evaluate script
    engine.eval::<()>("print(40 + 2)")?;
    engine.eval::<()>(r#"debug("hello!")"#)?;

    // 'logbook' captures all the 'print' and 'debug' output
    assert_eq!(logbook.read().unwrap().len(), 2);
    assert_eq!(logbook.read().unwrap()[0], "entry: 42");
    assert_eq!(logbook.read().unwrap()[1], r#"DEBUG: "hello!""#);

    for entry in logbook.read().unwrap().iter() {
        println!("{}", entry);
    }

    Ok(())
}
