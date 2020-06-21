`print` and `debug`
===================

{{#include ../links.md}}

The `print` and `debug` functions default to printing to `stdout`, with `debug` using standard debug formatting.

```rust
print("hello");         // prints hello to stdout
print(1 + 2 + 3);       // prints 6 to stdout
print("hello" + 42);    // prints hello42 to stdout
debug("world!");        // prints "world!" to stdout using debug formatting
```

Override `print` and `debug` with Callback Functions
--------------------------------------------------

When embedding Rhai into an application, it is usually necessary to trap `print` and `debug` output
(for logging into a tracking log, for example) with the `Engine::on_print` and `Engine::on_debug` methods:

```rust
// Any function or closure that takes an '&str' argument can be used to override
// 'print' and 'debug'
engine.on_print(|x| println!("hello: {}", x));
engine.on_debug(|x| println!("DEBUG: {}", x));

// Example: quick-'n-dirty logging
let logbook = Arc::new(RwLock::new(Vec::<String>::new()));

// Redirect print/debug output to 'log'
let log = logbook.clone();
engine.on_print(move |s| log.write().unwrap().push(format!("entry: {}", s)));

let log = logbook.clone();
engine.on_debug(move |s| log.write().unwrap().push(format!("DEBUG: {}", s)));

// Evaluate script
engine.eval::<()>(script)?;

// 'logbook' captures all the 'print' and 'debug' output
for entry in logbook.read().unwrap().iter() {
    println!("{}", entry);
}
```
