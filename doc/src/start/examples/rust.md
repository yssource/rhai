Rust Examples
============

{{#include ../../links.md}}

A number of examples can be found in the `examples` directory:

| Example                                                                                                                | Description                                                                                                                                   |
| ---------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| [`arrays_and_structs`](https://github.com/jonathandturner/rhai/tree/master/examples/arrays_and_structs.rs)             | Shows how to register a custom Rust type and using [arrays] on it.                                                                            |
| [`custom_types_and_methods`](https://github.com/jonathandturner/rhai/tree/master/examples/custom_types_and_methods.rs) | Shows how to register a custom Rust type and methods for it.                                                                                  |
| [`hello`](https://github.com/jonathandturner/rhai/tree/master/examples/hello.rs)                                       | Simple example that evaluates an expression and prints the result.                                                                            |
| [`reuse_scope`](https://github.com/jonathandturner/rhai/tree/master/examples/reuse_scope.rs)                           | Evaluates two pieces of code in separate runs, but using a common [`Scope`].                                                                  |
| [`rhai_runner`](https://github.com/jonathandturner/rhai/tree/master/examples/rhai_runner.rs)                           | Runs each filename passed to it as a Rhai script.                                                                                             |
| [`serde`](https://github.com/jonathandturner/rhai/tree/master/examples/serde.rs)                                       | Example to serialize and deserialize Rust types with [`serde`](https://crates.io/crates/serde).<br/>The [`serde`] feature is required to run. |
| [`simple_fn`](https://github.com/jonathandturner/rhai/tree/master/examples/simple_fn.rs)                               | Shows how to register a simple function.                                                                                                      |
| [`strings`](https://github.com/jonathandturner/rhai/tree/master/examples/strings.rs)                                   | Shows different ways to register functions taking string arguments.                                                                           |
| [`repl`](https://github.com/jonathandturner/rhai/tree/master/examples/repl.rs)                                         | A simple REPL, interactively evaluate statements from stdin.                                                                                  |

The `repl` example is a particularly good one as it allows one to interactively try out Rhai's
language features in a standard REPL (**R**ead-**E**val-**P**rint **L**oop).


Running Examples
----------------

Examples can be run with the following command:

```bash
cargo run --example {example_name}
```

`no-std` Samples
----------------

To illustrate `no-std` builds, a number of sample applications are available under the `no_std` directory:

| Sample                                                                                  | Description                                                                                           | Optimization |                     Allocator                     | Panics |
| --------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- | :----------: | :-----------------------------------------------: | :----: |
| [`no_std_test`](https://github.com/jonathandturner/rhai/tree/master/no_std/no_std_test) | Bare-bones test application that evaluates a Rhai expression and sets the result as the return value. |     Size     | [`wee_alloc`](https://crates.io/crates/wee_alloc) | Abort  |

`cargo run` cannot be used to run a `no-std` sample.  It must first be built:

```bash
cd no_std/no_std_test

cargo +nightly build --release

./target/release/no_std_test
```
