Create a Module from an AST
==========================

{{#include ../../links.md}}


`Module::eval_ast_as_new`
------------------------

A _module_ can be created from a single script (or pre-compiled [`AST`]) containing global variables,
functions and sub-modules via the `Module::eval_ast_as_new` method.

When given an [`AST`], it is first evaluated, then the following items are exposed as members of the new module:

* Global variables - essentially all variables that remain in the [`Scope`] at the end of a script run - that are exported. Variables not exported (via the `export` statement) remain hidden.

* Functions not specifically marked `private`.

* Global modules that remain in the [`Scope`] at the end of a script run.


`merge_namespaces` Parameter
---------------------------

The parameter `merge_namespaces` in `Module::eval_ast_as_new` determines the exact behavior of
functions exposed by the module and the namespace that they can access:

| `merge_namespaces` value | Description                                      |      Namespace      | Performance | Call global functions | Call functions in same module |
| :----------------------: | ------------------------------------------------ | :-----------------: | :---------: | :-------------------: | :---------------------------: |
|          `true`          | encapsulate entire `AST` into each function call | module, then global |  2x slower  |          yes          |              yes              |
|         `false`          | register each function independently             |     global only     |    fast     |          yes          |              no               |

If the ultimate intention is to load the [module] directly into an [`Engine`] via `Engine::load_package`,
set `merge_namespaces` to `false` because there will not be any _module_ namespace as `Engine::load_package`
flattens everything into the _global_ namespace anyway.


Examples
--------

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
//
// The second parameter ('merge_namespaces'), when set to true, will encapsulate
// a copy of the entire 'AST' into each function, allowing functions in the module script
// to cross-call each other.
//
// This incurs additional overhead, avoidable by setting 'merge_namespaces' to false
// which makes function calls 2x faster but at the expense of not being able to cross-call
// functions in the same module script.
let module = Module::eval_ast_as_new(Scope::new(), &ast, true, &engine)?;

// 'module' now contains:
//   - sub-module: 'foobar' (renamed from 'extra')
//   - functions: 'calc', 'add_len'
//   - constants: 'abc' (renamed from 'x'), 'foo', 'hello'
```
