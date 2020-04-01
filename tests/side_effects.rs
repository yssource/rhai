#![cfg(not(feature = "no_object"))]

///! This test simulates an external command object that is driven by a script.
use rhai::{Engine, EvalAltResult, RegisterFn, Scope, INT};
use std::cell::RefCell;
use std::rc::Rc;

/// External command.
struct Command {
    state: i64,
}

impl Command {
    /// Do some action.
    pub fn action(&mut self, val: i64) {
        self.state = val;
    }
    /// Get current value.
    pub fn get(&self) -> i64 {
        self.state
    }
}

/// Wrapper object to wrap a command object.
#[derive(Clone)]
struct CommandWrapper {
    command: Rc<RefCell<Command>>,
}

impl CommandWrapper {
    /// Delegate command action.
    pub fn do_action(&mut self, x: i64) {
        let mut command = self.command.borrow_mut();
        let val = command.get();
        command.action(val + x);
    }
    /// Delegate get value action.
    pub fn get_value(&mut self) -> i64 {
        let command = self.command.borrow();
        command.get()
    }
}

#[test]
fn test_side_effects() -> Result<(), EvalAltResult> {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    // Create the command object with initial state, handled by an `Rc`.
    let command = Rc::new(RefCell::new(Command { state: 12 }));
    assert_eq!(command.borrow().get(), 12);

    // Create the wrapper.
    let wrapper = CommandWrapper {
        command: command.clone(), // Notice this clones the `Rc` only
    };

    // Make the wrapper a singleton in the script environment.
    scope.push_constant("Command", wrapper);

    // Register type.
    engine.register_type_with_name::<CommandWrapper>("CommandType");
    engine.register_fn("action", CommandWrapper::do_action);
    engine.register_get("value", CommandWrapper::get_value);

    assert_eq!(
        engine.eval_with_scope::<INT>(
            &mut scope,
            r"
                // Drive the command object via the wrapper
                Command.action(30);
                Command.value
            "
        )?,
        42
    );

    // Make sure the actions are properly performed
    assert_eq!(command.borrow().get(), 42);

    Ok(())
}
