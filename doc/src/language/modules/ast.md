Create a Module from an AST
==========================

{{#include ../../links.md}}

It is easy to convert a pre-compiled `AST` into a module: just use `Module::eval_ast_as_new`.

Don't forget the [`export`] statement, otherwise there will be no variables exposed by the module
other than non-[`private`] functions (unless that's intentional).

```rust
use rhai::{Engine, Module};

let engine = Engine::new();

// Compile a script into an 'AST'
let ast = engine.compile(r#"
    // Functions become module functions
    fn calc(x) {
        x + 1
    }
    fn add_len(x, y) {
        x + y.len
    }

    // Imported modules can become sub-modules
    import "another module" as extra;

    // Variables defined at global level can become module variables
    const x = 123;
    let foo = 41;
    let hello;

    // Variable values become constant module variable values
    foo = calc(foo);
    hello = "hello, " + foo + " worlds!";

    // Finally, export the variables and modules
    export
        x as abc,           // aliased variable name
        foo,
        hello,
        extra as foobar;    // export sub-module
"#)?;

// Convert the 'AST' into a module, using the 'Engine' to evaluate it first
let module = Module::eval_ast_as_new(Scope::new(), &ast, &engine)?;

// 'module' now can be loaded into a custom 'Scope' for future use.  It contains:
//   - sub-module: 'foobar' (renamed from 'extra')
//   - functions: 'calc', 'add_len'
//   - variables: 'abc' (renamed from 'x'), 'foo', 'hello'
```
