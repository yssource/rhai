Module Resolvers
================

{{#include ../../links.md}}

When encountering an [`import`] statement, Rhai attempts to _resolve_ the module based on the path string.

_Module Resolvers_ are service types that implement the [`ModuleResolver`][traits] trait.


Built-In Module Resolvers
------------------------

There are a number of standard resolvers built into Rhai, the default being the `FileModuleResolver`
which simply loads a script file based on the path (with `.rhai` extension attached)
and execute it to form a module.

Built-in module resolvers are grouped under the `rhai::module_resolvers` module namespace.


`FileModuleResolver` (default)
-----------------------------

The _default_ module resolution service, not available for [`no_std`] or [WASM] builds.
Loads a script file (based off the current directory) with `.rhai` extension.

All functions in the _global_ namespace, plus all those defined in the same module,
are _merged_ into a _unified_ namespace.

```rust
------------------
| my_module.rhai |
------------------

private fn inner_message() { "hello! from module!" }

fn greet(callback) { print(callback.call()); }

-------------
| main.rhai |
-------------

fn main_message() { "hi! from main!" }

import "my_module" as m;

m::greet(|| "hello, " + "world!");  // works - anonymous function in global

m::greet(|| inner_message());       // works - function in module

m::greet(|| main_message());        // works - function in global
```

The base directory can be changed via the `FileModuleResolver::new_with_path` constructor function.

`FileModuleResolver::create_module` loads a script file and returns a module.


`StaticModuleResolver`
---------------------

Loads modules that are statically added. This can be used under [`no_std`].

Functions are searched in the _global_ namespace by default.

```rust
use rhai::{Module, module_resolvers::StaticModuleResolver};

let module: Module = create_a_module();

let mut resolver = StaticModuleResolver::new();
resolver.insert("my_module", module);
```


`ModuleResolversCollection`
--------------------------

A collection of module resolvers. Modules will be resolved from each resolver in sequential order.

This is useful when multiple types of modules are needed simultaneously.


Set into `Engine`
-----------------

An [`Engine`]'s module resolver is set via a call to `Engine::set_module_resolver`:

```rust
use rhai::module_resolvers::StaticModuleResolver;

// Create a module resolver
let resolver = StaticModuleResolver::new();

// Register functions into 'resolver'...

// Use the module resolver
engine.set_module_resolver(Some(resolver));

// Effectively disable 'import' statements by setting module resolver to 'None'
engine.set_module_resolver(None);
```
