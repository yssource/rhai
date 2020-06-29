Compile a Script (to AST)
========================

{{#include ../links.md}}

To repeatedly evaluate a script, _compile_ it first into an `AST` (abstract syntax tree) form:

```rust
// Compile to an AST and store it for later evaluations
let ast = engine.compile("40 + 2")?;

for _ in 0..42 {
    let result: i64 = engine.eval_ast(&ast)?;

    println!("Answer #{}: {}", i, result);      // prints 42
}
```

Compiling a script file is also supported (not available under [`no_std`] or in [WASM] builds):

```rust
let ast = engine.compile_file("hello_world.rhai".into())?;
```
