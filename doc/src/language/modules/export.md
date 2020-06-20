Export Variables and Functions from Modules
==========================================

{{#include ../../links.md}}

A _module_ is a single script (or pre-compiled `AST`) containing global variables and functions.

The `export` statement, which can only be at global level, exposes selected variables as members of a module.

Variables not exported are _private_ and invisible to the outside.

On the other hand, all functions are automatically exported, _unless_ it is explicitly opt-out with the [`private`] prefix.

Functions declared [`private`] are invisible to the outside.

Everything exported from a module is **constant** (**read-only**).

```rust
// This is a module script.

fn inc(x) { x + 1 }     // script-defined function - default public

private fn foo() {}     // private function - invisible to outside

let private = 123;      // variable not exported - default invisible to outside
let x = 42;             // this will be exported below

export x;               // the variable 'x' is exported under its own name

export x as answer;     // the variable 'x' is exported under the alias 'answer'
                        // another script can load this module and access 'x' as 'module::answer'
```
