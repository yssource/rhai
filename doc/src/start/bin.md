Rhai Tools
==========

{{#include ../links.md}}

A number of Rhai tools can be found in the `src/bin` directory:

| Tool                                              | Description                                                 |
| ------------------------------------------------- | ----------------------------------------------------------- |
| [`rhai-repl`]({{repoTree}}/examples/rhai-repl.rs) | a simple REPL, interactively evaluate statements from stdin |
| [`rhai-run`]({{repoTree}}/examples/rhai-run.rs)   | runs each filename passed to it as a Rhai script            |

`rhai-repl` is particularly useful - it allows one to interactively try out Rhai's
language features in a standard REPL (**R**ead-**E**val-**P**rint **L**oop).


Running a Tool
--------------

Tools can be run with the following command:

```bash
cargo run --bin {tool_name}
```
