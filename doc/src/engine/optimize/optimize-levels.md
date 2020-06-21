Optimization Levels
==================

{{#include ../../links.md}}

There are three levels of optimization: `None`, `Simple` and `Full`.

* `None` is obvious - no optimization on the AST is performed.

* `Simple` (default) performs only relatively _safe_ optimizations without causing side-effects
  (i.e. it only relies on static analysis and will not actually perform any function calls).

* `Full` is _much_ more aggressive, _including_ running functions on constant arguments to determine their result.
  One benefit to this is that many more optimization opportunities arise, especially with regards to comparison operators.


Set Optimization Level
---------------------

An [`Engine`]'s optimization level is set via a call to `Engine::set_optimization_level`:

```rust
// Turn on aggressive optimizations
engine.set_optimization_level(rhai::OptimizationLevel::Full);
```
