///! This test simulates an external command object that is driven by a script.
use rhai::{Engine, EvalAltResult, RegisterFn, Scope, INT};
use std::sync::{Arc, Mutex};

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
    command: Arc<Mutex<Command>>,
}

impl CommandWrapper {
    /// Delegate command action.
    pub fn do_action(&mut self, x: i64) {
        let mut command = self.command.lock().unwrap();
        let val = command.get();
        command.action(val + x);
    }
    /// Delegate get value action.
    pub fn get_value(&mut self) -> i64 {
        let command = self.command.lock().unwrap();
        command.get()
    }
}

#[cfg(not(feature = "no_object"))]
#[test]
fn test_side_effects_command() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    // Create the command object with initial state, handled by an `Rc`.
    let command = Arc::new(Mutex::new(Command { state: 12 }));
    assert_eq!(command.lock().unwrap().get(), 12);

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
    assert_eq!(command.lock().unwrap().get(), 42);

    Ok(())
}

#[test]
fn test_side_effects_print() -> Result<(), Box<EvalAltResult>> {
    use std::sync::Arc;
    use std::sync::RwLock;

    let result = Arc::new(RwLock::new(String::from("")));

    let mut engine = Engine::new();

    // Override action of 'print' function
    let logger = result.clone();
    engine.on_print(move |s| logger.write().unwrap().push_str(s));

    engine.consume("print(40 + 2);")?;

    assert_eq!(*result.read().unwrap(), "42");
    Ok(())
}
