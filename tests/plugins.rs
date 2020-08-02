use rhai::{export_module, exported_module};
use rhai::{Engine, EvalAltResult, INT};

#[export_module]
pub mod array_package {
    use rhai::{Array, INT};

    pub fn len(array: &mut Array, mul: INT) -> INT {
        (array.len() as INT) * mul
    }
}

#[test]
fn test_plugins() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    let m = exported_module!(array_package);

    engine.load_package(m.into());

    assert_eq!(engine.eval::<INT>("let a = [1, 2, 3]; a.len(2)")?, 6);

    Ok(())
}
