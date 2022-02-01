#![cfg(feature = "debugging")]
use rhai::{Dynamic, Engine, EvalAltResult, INT};

#[cfg(not(feature = "no_index"))]
use rhai::Array;

#[cfg(not(feature = "no_object"))]
use rhai::Map;

#[test]
fn test_debugging() -> Result<(), Box<EvalAltResult>> {
    let engine = Engine::new();

    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_index"))]
    {
        let r = engine.eval::<Array>(
            "
                fn foo(x) {
                    if x >= 5 {
                        stack_trace()
                    } else {
                        foo(x+1)
                    }
                }

                foo(0)
            ",
        )?;

        assert_eq!(r.len(), 6);

        assert_eq!(engine.eval::<INT>("len(stack_trace())")?, 0);
    }

    Ok(())
}

#[test]
#[cfg(not(feature = "no_object"))]
fn test_debugger_state() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine.register_debugger(
        || {
            // Say, use an object map for the debugger state
            let mut state = Map::new();
            // Initialize properties
            state.insert("hello".into(), (42 as INT).into());
            state.insert("foo".into(), false.into());
            Dynamic::from_map(state)
        },
        |context, _, _, _, _| {
            // Get global runtime state
            let global = context.global_runtime_state_mut();

            // Get debugger
            let debugger = &mut global.debugger;

            // Print debugger state - which is an object map
            println!("Current state = {}", debugger.state());

            // Modify state
            let mut state = debugger.state_mut().write_lock::<Map>().unwrap();
            let hello = state.get("hello").unwrap().as_int().unwrap();
            state.insert("hello".into(), (hello + 1).into());
            state.insert("foo".into(), true.into());
            state.insert("something_new".into(), "hello, world!".into());

            // Continue with debugging
            Ok(rhai::debugger::DebuggerCommand::StepInto)
        },
    );

    engine.run("let x = 42;")?;

    Ok(())
}
