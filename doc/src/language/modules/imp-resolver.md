Implement a Custom Module Resolver
=================================

{{#include ../../links.md}}

For many applications in which Rhai is embedded, it is necessary to customize the way that modules
are resolved.  For instance, modules may need to be loaded from script texts stored in a database,
not in the file system.

A module resolver must implement the trait `rhai::ModuleResolver`, which contains only one function:
`resolve`.

When Rhai prepares to load a module, `ModuleResolver::resolve` is called with the name
of the _module path_ (i.e. the path specified in the [`import`] statement). Upon success, it should
return a [`Module`]; if the module cannot be load, return `EvalAltResult::ErrorModuleNotFound`.

Example
-------

```rust
use rhai::{ModuleResolver, Module, Engine, EvalAltResult};

// Define a custom module resolver.
struct MyModuleResolver {}

// Implement the 'ModuleResolver' trait.
impl ModuleResolver for MyModuleResolver {
    // Only required function.
    fn resolve(
        &self,
        engine: &Engine,    // reference to the current 'Engine'
        path: &str,         // the module path
        pos: Position,      // location of the 'import' statement
    ) -> Result<Module, Box<EvalAltResult>> {
        // Check module path.
        if is_valid_module_path(path) {
            // Load the custom module.
            let module: Module = load_secret_module(path);
            Ok(module)
        } else {
            Err(Box::new(EvalAltResult::ErrorModuleNotFound(path.into(), pos)))
        }
    }
}

fn main() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    // Set the custom module resolver into the 'Engine'.
    engine.set_module_resolver(Some(MyModuleResolver {}));

    engine.consume(r#"
        import "hello" as foo;  // this 'import' statement will call
                                // 'MyModuleResolver::resolve' with "hello" as path
        foo:bar();
    "#)?;

    Ok(())
}
```
