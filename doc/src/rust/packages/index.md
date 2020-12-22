Packages
========

{{#include ../../links.md}}

Standard built-in Rhai features are provided in various _packages_ that can be registered into the
_global namespace_ of an [`Engine`] via `Engine::register_global_module`.

Packages reside under `rhai::packages::*` and the trait `rhai::packages::Package` must be loaded in order for
packages to be used.

Packages typically contain Rust functions that are callable within a Rhai script.
All _top-level_ functions in a package are available under the _global namespace_
(i.e. they're available without namespace qualifiers).

Once a package is created (e.g. via `Package::new`), it can be _shared_ (via `Package::as_shared_module`)
among multiple instances of [`Engine`], even across threads (under [`sync`]).
Therefore, a package only has to be created _once_.

```rust
use rhai::Engine;
use rhai::packages::Package         // load the 'Package' trait to use packages
use rhai::packages::CorePackage;    // the 'core' package contains basic functionalities (e.g. arithmetic)

// Create a 'raw' Engine
let mut engine = Engine::new_raw();

// Create a package - can be shared among multiple `Engine` instances
let package = CorePackage::new();

// Register the package into the global namespace.
// 'Package::as_shared_module' converts the package into a shared module.
engine.register_global_module(package.as_shared_module());
```


Share a Package Among `Engine`s
------------------------------

`Engine::register_global_module` consumes the input shared module.

However, `Package::as_shared_module` can be called multiple times for multiple instances of [`Engine`].
