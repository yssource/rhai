Keywords
========

{{#include ../links.md}}

The following are reserved keywords in Rhai:

| Keywords                                          | Usage                 | Not available under feature |
| ------------------------------------------------- | --------------------- | :-------------------------: |
| `true`, `false`                                   | Boolean constants     |                             |
| `let`, `const`                                    | Variable declarations |                             |
| `if`, `else`                                      | Control flow          |                             |
| `while`, `loop`, `for`, `in`, `continue`, `break` | Looping               |                             |
| `fn`, `private`                                   | Functions             |       [`no_function`]       |
| `return`                                          | Return values         |                             |
| `throw`                                           | Return errors         |                             |
| `import`, `export`, `as`                          | Modules               |        [`no_module`]        |

Keywords cannot be the name of a [function] or [variable], unless the relevant exclusive feature is enabled.
For example, `fn` is a valid variable name under [`no_function`].
