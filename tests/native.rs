use rhai::{Dynamic, Engine, EvalAltResult, NativeCallContext, RegisterFn, INT};
use std::any::TypeId;

#[test]
fn test_native_context() -> Result<(), Box<EvalAltResult>> {
    fn add_double(
        context: NativeCallContext,
        args: &mut [&mut Dynamic],
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let x = args[0].as_int().unwrap();
        let y = args[1].as_int().unwrap();
        Ok(format!("{}_{}", context.fn_name(), x + 2 * y).into())
    }

    let mut engine = Engine::new();

    engine
        .register_raw_fn(
            "add_double",
            &[TypeId::of::<INT>(), TypeId::of::<INT>()],
            add_double,
        )
        .register_raw_fn(
            "adbl",
            &[TypeId::of::<INT>(), TypeId::of::<INT>()],
            add_double,
        );

    assert_eq!(engine.eval::<String>("add_double(40, 1)")?, "add_double_42");

    assert_eq!(engine.eval::<String>("adbl(40, 1)")?, "adbl_42");

    Ok(())
}
