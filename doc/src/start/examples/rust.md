Rust Examples
============

{{#include ../../links.md}}

A number of examples can be found in the `examples` folder:

| Example                                                                                                                | Description                                                                  |
| ---------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| [`arrays_and_structs`](https://github.com/jonathandturner/rhai/tree/master/examples/arrays_and_structs.rs)             | Shows how to register a custom Rust type and using [arrays] on it.           |
| [`custom_types_and_methods`](https://github.com/jonathandturner/rhai/tree/master/examples/custom_types_and_methods.rs) | Shows how to register a custom Rust type and methods for it.                 |
| [`hello`](https://github.com/jonathandturner/rhai/tree/master/examples/hello.rs)                                       | Simple example that evaluates an expression and prints the result.           |
| [`no_std`](https://github.com/jonathandturner/rhai/tree/master/examples/no_std.rs)                                     | Example to test out `no-std` builds.                                         |
| [`reuse_scope`](https://github.com/jonathandturner/rhai/tree/master/examples/reuse_scope.rs)                           | Evaluates two pieces of code in separate runs, but using a common [`Scope`]. |
| [`rhai_runner`](https://github.com/jonathandturner/rhai/tree/master/examples/rhai_runner.rs)                           | Runs each filename passed to it as a Rhai script.                            |
| [`simple_fn`](https://github.com/jonathandturner/rhai/tree/master/examples/simple_fn.rs)                               | Shows how to register a simple function.                                     |
| [`strings`](https://github.com/jonathandturner/rhai/tree/master/examples/strings.rs)                                   | Shows different ways to register functions taking string arguments.          |
| [`repl`](https://github.com/jonathandturner/rhai/tree/master/examples/repl.rs)                                         | A simple REPL, interactively evaluate statements from stdin.                 |

The `repl` example is a particularly good one as it allows one to interactively try out Rhai's
language features in a standard REPL (**R**ead-**E**val-**P**rint **L**oop).


Running Examples
----------------

Examples can be run with the following command:

```bash
cargo run --example {example_name}
```
