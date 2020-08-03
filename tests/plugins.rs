use rhai::plugin::*;
use rhai::{Engine, EvalAltResult, INT};

#[export_module]
mod special_array_package {
    use rhai::{Array, INT};

    pub fn len(array: &mut Array, mul: INT) -> INT {
        (array.len() as INT) * mul
    }
}

#[export_fn]
fn make_greeting(n: INT) -> String {
    format!("{} {}", n, if n > 1 { "kitties" } else { "kitty" }).into()
}

#[test]
fn test_plugins_package() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    let mut m = exported_module!(special_array_package);
    register_exported_fn!(m, "greet", make_greeting);

    engine.load_package(m);

    assert_eq!(engine.eval::<INT>("let a = [1, 2, 3]; len(a, 2)")?, 6);
    assert_eq!(
        engine.eval::<String>("let a = [1, 2, 3]; greet(len(a, 2))")?,
        "6 kitties"
    );

    Ok(())
}
