use rhai::{Engine, EvalAltResult, RegisterFn, Scope};
use std::cell::Cell;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct CommandWrapper {
    value: Rc<Cell<i64>>,
}

impl CommandWrapper {
    pub fn set_value(&mut self, x: i64) {
        let val = self.value.get();
        self.value.set(val + x);
    }
}

#[test]
fn test_side_effects() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    let payload = Rc::new(Cell::new(12));
    assert_eq!(payload.get(), 12);

    let command = CommandWrapper {
        value: payload.clone(),
    };

    scope.push_constant("Command", command);

    engine.register_type_with_name::<CommandWrapper>("CommandType");
    engine.register_fn("action", CommandWrapper::set_value);

    engine.eval_with_scope::<()>(&mut scope, "Command.action(30)")?;

    assert_eq!(payload.get(), 42);

    Ok(())
}
