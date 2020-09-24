Module Resolvers
================

{{#include ../../links.md}}

When encountering an [`import`] statement, Rhai attempts to _resolve_ the module based on the path string.

_Module Resolvers_ are service types that implement the [`ModuleResolver`][traits] trait.


Built-In Module Resolvers
------------------------

There are a number of standard resolvers built into Rhai, the default being the `FileModuleResolver`
which simply loads a script file based on the path (with `.rhai` extension attached) and execute it to form a module.

Built-in module resolvers are grouped under the `rhai::module_resolvers` module namespace.

| Module Resolver             | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |                Namespace                |
| --------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------: |
| `FileModuleResolver`        | The default module resolution service, not available under [`no_std`] or [WASM] builds. Loads a script file (based off the current directory) with `.rhai` extension.<br/>The base directory can be changed via the `FileModuleResolver::new_with_path()` constructor function.<br/>`FileModuleResolver::create_module()` loads a script file and returns a module.                                                                                                                                                        | Module (cannot access global namespace) |
| `GlobalFileModuleResolver`  | A simpler but more efficient version of `FileModuleResolver`, intended for short utility modules. Not available under [`no_std`] or [WASM] builds. Loads a script file (based off the current directory) with `.rhai` extension.<br/>**Note:** All functions are assumed absolutely _pure_ and cannot cross-call each other.<br/>The base directory can be changed via the `FileModuleResolver::new_with_path()` constructor function.<br/>`FileModuleResolver::create_module()` loads a script file and returns a module. |                 Global                  |
| `StaticModuleResolver`      | Loads modules that are statically added. This can be used under [`no_std`].                                                                                                                                                                                                                                                                                                                                                                                                                                                |                 Global                  |
| `ModuleResolversCollection` | A collection of module resolvers. Modules will be resolved from each resolver in sequential order.<br/>This is useful when multiple types of modules are needed simultaneously.                                                                                                                                                                                                                                                                                                                                            |                 Global                  |


Set into `Engine`
-----------------

An [`Engine`]'s module resolver is set via a call to `Engine::set_module_resolver`:

```rust
// Use the 'StaticModuleResolver'
let resolver = rhai::module_resolvers::StaticModuleResolver::new();
engine.set_module_resolver(Some(resolver));

// Effectively disable 'import' statements by setting module resolver to 'None'
engine.set_module_resolver(None);
```
