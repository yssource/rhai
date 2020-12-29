Packaged Utilities
==================

{{#include ../links.md}}

A number of Rhai-driven utility programs can be found in the `src/bin` directory:

|                  Utility program                  | Description                                                 |
| :-----------------------------------------------: | ----------------------------------------------------------- |
| [`rhai-repl`]({{repoTree}}/examples/rhai-repl.rs) | a simple REPL, interactively evaluate statements from stdin |
|  [`rhai-run`]({{repoTree}}/examples/rhai-run.rs)  | runs each filename passed to it as a Rhai script            |

`rhai-repl` is particularly useful  &ndash;  it allows one to interactively try out Rhai's
language features in a standard REPL (**R**ead-**E**val-**P**rint **L**oop).


Running a Utility Program
-------------------------

Utilities can be run with the following command:

```bash
cargo run --bin {program_name}
```
