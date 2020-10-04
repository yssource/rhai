Scriptable Event Handler with State
==================================

{{#include ../links.md}}


Usage Scenario
--------------

* A system sends _events_ that must be handled.

* Flexibility in event handling must be provided, through user-side scripting.

* State must be kept between invocations of event handlers.

* Default implementations of event handlers can be provided.


Key Concepts
------------

* An _event handler_ object is declared that holds the following items:
  * [`Engine`] with registered functions serving as an API,
  * [`AST`] of the user script,
  * a [`Scope`] containing state.

* Upon an event, the appropriate event handler function in the script is called via [`Engine::call_fn`][`call_fn`].

* Optionally, trap the `EvalAltResult::ErrorFunctionNotFound` error to provide a default implementation.


Implementation
--------------

### Declare Handler Object

In most cases, it would be simpler to store an [`Engine`] instance together with the handler object
because it only requires registering all API functions only once.

In rare cases where handlers are created and destroyed in a tight loop, a new [`Engine`] instance
can be created for each event. See [_One Engine Instance Per Call_](parallel.md) for more details.

```rust
use rhai::{Engine, Scope, AST, EvalAltResult};

// Event handler
struct Handler {
    // Scripting engine
    pub engine: Engine,
    // Use a custom 'Scope' to keep stored state
    pub scope: Scope<'static>,
    // Program script
    pub ast: AST
}
```

### Initialize Handler Object

Steps to initialize the event handler:

1. Register an API with the [`Engine`],
2. Create a custom [`Scope`] to serve as the stored state,
3. Add default state variables into the custom [`Scope`],
4. Get the handler script and [compile][`AST`] it,
5. Store the compiled [`AST`] for future evaluations,
6. Run the [`AST`] to initialize event handler state variables.

```rust
impl Handler {
    pub new(path: impl Into<PathBuf>) -> Self {
        let mut engine = Engine::new();

        // Register API functions here
        engine
            .register_fn("func1", func1)
            .register_fn("func2", func2)
            .register_fn("func3", func3)
            .register_type_with_name::<SomeType>("SomeType")
            .register_get_set("value",
                |obj: &mut SomeType| obj.data,
                |obj: &mut SomeType, value: i64| obj.data = value
            );

        // Create a custom 'Scope' to hold state
        let mut scope = Scope::new();

        // Add initialized state into the custom 'Scope'
        scope.push("state1", false);
        scope.push("state2", SomeType::new(42));

        // Compile the handler script.
        // In a real application you'd be handling errors...
        let ast = engine.compile_file(path).unwrap();

        // Evaluate the script to initialize it and other state variables.
        // In a real application you'd again be handling errors...
        engine.consume_ast_with_scope(&mut scope, &ast).unwrap();

        // The event handler is essentially these three items:
        Handler { engine, scope, ast }
    }
}
```

### Hook up events

There is usually an interface or trait that gets called when an event comes from the system.

Mapping an event from the system into a scripted handler is straight-forward:

```rust
impl Handler {
    // Say there are three events: 'start', 'end', 'update'.
    // In a real application you'd be handling errors...
    pub fn on_event(&mut self, event_name: &str, event_data: i64) {
        match event_name {
            // The 'start' event maps to function 'start'.
            // In a real application you'd be handling errors...
            "start" =>
                self.engine
                    .call_fn(&mut self.scope, &self.ast, "start", (event_data,)).unwrap(),

            // The 'end' event maps to function 'end'.
            // In a real application you'd be handling errors...
            "end" =>
                self.engine
                    .call_fn(&mut self.scope, &self.ast, "end", (event_data,)).unwrap(),

            // The 'update' event maps to function 'update'.
            // This event provides a default implementation when the scripted function
            // is not found.
            "update" =>
                self.engine
                    .call_fn(&mut self.scope, &self.ast, "update", (event_data,))
                    .or_else(|err| match *err {
                        EvalAltResult::ErrorFunctionNotFound(fn_name, _) if fn_name == "update" => {
                            // Default implementation of 'update' event handler
                            self.scope.set_value("state2", SomeType::new(42));
                            // Turn function-not-found into a success
                            Ok(().into())
                        }
                        _ => Err(err.into())
                    })
                    .unwrap()
        }
    }
}
```

### Sample Handler Script

Because the stored state is kept in a custom [`Scope`], it is possible for all functions defined
in the handler script to access and modify these state variables.

The API registered with the [`Engine`] can be also used throughout the script.

```rust
fn start(data) {
    if state1 {
        throw "Already started!";
    }
    if func1(state2) || func2() {
        throw "Conditions not yet ready to start!";
    }
    state1 = true;
    state2.value = 0;
}

fn end(data) {
    if !state1 {
        throw "Not yet started!";
    }
    if func1(state2) || func2() {
        throw "Conditions not yet ready to start!";
    }
    state1 = false;
}

fn update(data) {
    state2.value += func3(data);
}
```
