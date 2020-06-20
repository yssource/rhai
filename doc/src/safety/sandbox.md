Sand-Boxing - Block Access to External Data
==========================================

{{#include ../links.md}}

Rhai is _sand-boxed_ so a script can never read from outside its own environment.

Furthermore, an [`Engine`] created non-`mut` cannot mutate any state outside of itself;
so it is highly recommended that [`Engine`]'s are created immutable as much as possible.

```rust
let mut engine = Engine::new();     // create mutable 'Engine'

engine.register_get("add", add);    // configure 'engine'

let engine = engine;                // shadow the variable so that 'engine' is now immutable
```
