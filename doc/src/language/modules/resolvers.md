Module Resolvers
================

{{#include ../../links.md}}

When encountering an [`import`] statement, Rhai attempts to _resolve_ the module based on the path string.

_Module Resolvers_ are service types that implement the [`ModuleResolver`](/rust/traits.md) trait.

There are a number of standard resolvers built into Rhai, the default being the `FileModuleResolver`
which simply loads a script file based on the path (with `.rhai` extension attached) and execute it to form a module.

Built-in module resolvers are grouped under the `rhai::module_resolvers` module namespace.

| Module Resolver        | Description                                                                                                                                                                                                                                                                                                                                                         |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `FileModuleResolver`   | The default module resolution service, not available under [`no_std`] or [WASM] builds. Loads a script file (based off the current directory) with `.rhai` extension.<br/>The base directory can be changed via the `FileModuleResolver::new_with_path()` constructor function.<br/>`FileModuleResolver::create_module()` loads a script file and returns a module. |
| `StaticModuleResolver` | Loads modules that are statically added. This can be used under [`no_std`].                                                                                                                                                                                                                                                                                         |

An [`Engine`]'s module resolver is set via a call to `Engine::set_module_resolver`:

```rust
// Use the 'StaticModuleResolver'
let resolver = rhai::module_resolvers::StaticModuleResolver::new();
engine.set_module_resolver(Some(resolver));

// Effectively disable 'import' statements by setting module resolver to 'None'
engine.set_module_resolver(None);
```
