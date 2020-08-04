Keywords List
=============

{{#include ../links.md}}

|        Keyword        | Description                              | Inactive under  | Overloadable |
| :-------------------: | ---------------------------------------- | :-------------: | :----------: |
|        `true`         | Boolean true literal                     |                 |      No      |
|        `false`        | Boolean false literal                    |                 |      No      |
|         `let`         | Variable declaration                     |                 |      No      |
|        `const`        | Constant declaration                     |                 |      No      |
|      `is_shared`      | Is a value shared?                       |                 |      No      |
|         `if`          | If statement                             |                 |      No      |
|        `else`         | else block of if statement               |                 |      No      |
|        `while`        | While loop                               |                 |      No      |
|        `loop`         | Infinite loop                            |                 |      No      |
|         `for`         | For loop                                 |                 |      No      |
|         `in`          | Containment test, part of for loop       |                 |      No      |
|      `continue`       | Continue a loop at the next iteration    |                 |      No      |
|        `break`        | Loop breaking                            |                 |      No      |
|       `return`        | Return value                             |                 |      No      |
|        `throw`        | Throw exception                          |                 |      No      |
|       `import`        | Import module                            |  [`no_module`]  |      No      |
|       `export`        | Export variable                          |  [`no_module`]  |      No      |
|         `as`          | Alias for variable export                |  [`no_module`]  |      No      |
|       `private`       | Mark function private                    | [`no_function`] |      No      |
| `fn` (lower-case `f`) | Function definition                      | [`no_function`] |      No      |
|  `Fn` (capital `F`)   | Function to create a [function pointer]  |                 |     Yes      |
|        `call`         | Call a [function pointer]                |                 |      No      |
|        `curry`        | Curry a [function pointer]               |                 |      No      |
|        `this`         | Reference to base object for method call | [`no_function`] |      No      |
|       `type_of`       | Get type name of value                   |                 |     Yes      |
|        `print`        | Print value                              |                 |     Yes      |
|        `debug`        | Print value in debug format              |                 |     Yes      |
|        `eval`         | Evaluate script                          |                 |     Yes      |


Reserved Keywords
-----------------

| Keyword   | Potential usage       |
| --------- | --------------------- |
| `var`     | Variable declaration  |
| `static`  | Variable declaration  |
| `shared`  | Share value           |
| `do`      | Looping               |
| `each`    | Looping               |
| `then`    | Control flow          |
| `goto`    | Control flow          |
| `exit`    | Control flow          |
| `switch`  | Matching              |
| `match`   | Matching              |
| `case`    | Matching              |
| `public`  | Function/field access |
| `new`     | Constructor           |
| `try`     | Trap exception        |
| `catch`   | Catch exception       |
| `use`     | Import namespace      |
| `with`    | Scope                 |
| `module`  | Module                |
| `package` | Package               |
| `spawn`   | Threading             |
| `go`      | Threading             |
| `await`   | Async                 |
| `async`   | Async                 |
| `sync`    | Async                 |
| `yield`   | Async                 |
| `default` | Special value         |
| `void`    | Special value         |
| `null`    | Special value         |
| `nil`     | Special value         |
