Variables
=========

{{#include ../links.md}}

Variables in Rhai follow normal C naming rules (i.e. must contain only ASCII letters, digits and underscores '`_`').

Variable names must start with an ASCII letter or an underscore '`_`', must contain at least one ASCII letter,
and must start with an ASCII letter before a digit.

Therefore, names like '`_`', '`_42`', '`3a`' etc. are not legal variable names, but '`_c3po`' and '`r2d2`' are.
Variable names are also case _sensitive_.

Variables are defined using the `let` keyword. A variable defined within a statement block is _local_ to that block.

```rust
let x = 3;          // ok
let _x = 42;        // ok
let x_ = 42;        // also ok
let _x_ = 42;       // still ok

let _ = 123;        // <- syntax error: illegal variable name
let _9 = 9;         // <- syntax error: illegal variable name

let x = 42;         // variable is 'x', lower case
let X = 123;        // variable is 'X', upper case
x == 42;
X == 123;

{
    let x = 999;    // local variable 'x' shadows the 'x' in parent block
    x == 999;       // access to local 'x'
}
x == 42;            // the parent block's 'x' is not changed
```
