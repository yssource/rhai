Export Variables, Functions and Sub-Modules in Module
===================================================

{{#include ../../links.md}}

A _module_ is a single script (or pre-compiled [`AST`]) containing global variables, functions and sub-modules.

A module can be created from a script via the `Module::eval_ast_as_new` method. When given an [`AST`],
it is first evaluated, then the following items are exposed as members of the new module:

* Global variables - essentially all variables that remain in the [`Scope`] at the end of a script run - that are exported. Variables not exported (via the `export` statement) remain hidden.

* Functions not specifically marked `private`.

* Global modules that remain in the [`Scope`] at the end of a script run.


Global Variables
----------------

The `export` statement, which can only be at global level, exposes selected variables as members of a module.

Variables not exported are _private_ and hidden to the outside.

Everything exported from a module is **constant** (**read-only**).

```rust
// This is a module script.

let private = 123;      // variable not exported - default hidden
let x = 42;             // this will be exported below

export x;               // the variable 'x' is exported under its own name

export x as answer;     // the variable 'x' is exported under the alias 'answer'
                        // another script can load this module and access 'x' as 'module::answer'

{
    let inner = 0;      // local variable - it disappears when the statement block ends,
                        //                  therefore it is not 'global' and is not exported

    export inner;       // exporting an temporary variable has no effect
}
```


Functions
---------

All functions are automatically exported, _unless_ it is explicitly opt-out with the [`private`] prefix.

Functions declared [`private`] are hidden to the outside.

```rust
// This is a module script.

fn inc(x) { x + 1 }     // script-defined function - default public

private fn foo() {}     // private function - hidden
```


Sub-Modules
-----------

All loaded modules are automatically exported as sub-modules.

To prevent a module from being exported, load it inside a block statement so that it goes away at the
end of the block.

```rust
// This is a module script.

import "hello" as foo;      // exported as sub-module 'foo'

{
    import "world" as bar;  // not exported - the module disappears at the end
                            //                of the statement block and is not 'global'
}
```
