Keywords
========

{{#include ../links.md}}

The following are reserved keywords in Rhai:

| Active keywords                                   | Reserved keywords                                | Usage                 | Inactive under feature |
| ------------------------------------------------- | ------------------------------------------------ | --------------------- | :--------------------: |
| `true`, `false`                                   |                                                  | Boolean constants     |                        |
| `let`, `const`                                    | `var`, `static`                                  | Variable declarations |                        |
| `shared`, `take`                                  |                                                  | Shared values         |     [`no_shared`]      |
| `if`, `else`                                      | `then`, `goto`, `exit`                           | Control flow          |                        |
|                                                   | `switch`, `match`, `case`                        | Matching              |                        |
| `while`, `loop`, `for`, `in`, `continue`, `break` | `do`, `each`                                     | Looping               |                        |
| `fn`, `private`                                   | `public`, `new`                                  | Functions             |    [`no_function`]     |
| `return`                                          |                                                  | Return values         |                        |
| `throw`                                           | `try`, `catch`                                   | Throw exceptions      |                        |
| `import`, `export`, `as`                          | `use`, `with`, `module`, `package`               | Modules/packages      |     [`no_module`]      |
| `Fn`, `call`, `curry`                             |                                                  | Function pointers     |                        |
|                                                   | `spawn`, `go`, `sync`, `async`, `await`, `yield` | Threading/async       |                        |
| `type_of`, `print`, `debug`, `eval`               |                                                  | Special functions     |                        |
|                                                   | `default`, `void`, `null`, `nil`                 | Special values        |                        |

Keywords cannot become the name of a [function] or [variable], even when they are disabled.

