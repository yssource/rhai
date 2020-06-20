Re-Optimize an AST
==================

{{#include ../../links.md}}

If it is ever needed to _re_-optimize an `AST`, use the `optimize_ast` method:

```rust
// Compile script to AST
let ast = engine.compile("40 + 2")?;

// Create a new 'Scope' - put constants in it to aid optimization if using 'OptimizationLevel::Full'
let scope = Scope::new();

// Re-optimize the AST
let ast = engine.optimize_ast(&scope, &ast, OptimizationLevel::Full);
```
