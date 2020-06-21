Create a Module from Rust
========================

{{#include ../../links.md}}

To load a custom module (written in Rust) into an [`Engine`], first create a [`Module`] type,
add variables/functions into it, then finally push it into a custom [`Scope`].

This has the equivalent effect of putting an [`import`] statement at the beginning of any script run.

```rust
use rhai::{Engine, Scope, Module, i64};

let mut engine = Engine::new();
let mut scope = Scope::new();

let mut module = Module::new();             // new module
module.set_var("answer", 41_i64);           // variable 'answer' under module
module.set_fn_1("inc", |x: i64| Ok(x+1));   // use the 'set_fn_XXX' API to add functions

// Push the module into the custom scope under the name 'question'
// This is equivalent to 'import "..." as question;'
scope.push_module("question", module);

// Use module-qualified variables
engine.eval_expression_with_scope::<i64>(&scope, "question::answer + 1")? == 42;

// Call module-qualified functions
engine.eval_expression_with_scope::<i64>(&scope, "question::inc(question::answer)")? == 42;
```
