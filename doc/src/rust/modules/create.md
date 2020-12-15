Create a Module from Rust
========================

{{#include ../../links.md}}


Create via Plugin
-----------------

By far the simplest way to create a [module] is via a [plugin module]
which converts a normal Rust module into a Rhai [module] via procedural macros.


Create via `Module` API
-----------------------

Manually creating a [module] is possible via the `Module` API.

For the complete `Module` API, refer to the [documentation](https://docs.rs/rhai/{{version}}/rhai/struct.Module.html) online.


Make the `Module` Available to the `Engine`
------------------------------------------

`Engine::load_package` supports loading a [module] as a [package].

Since it acts as a [package], all functions will be registered into the _global_ namespace
and can be accessed without _namespace qualifiers_.  This is by far the easiest way to expose
a module's functionalities to Rhai.

```rust
use rhai::{Engine, Module};

let mut module = Module::new();             // new module

// Use the 'set_fn_XXX' API to add functions.
let hash = module.set_fn_1("inc", |x: i64| Ok(x+1));

// Remember to update the parameter names/types and return type metadata.
// 'set_fn_XXX' by default does not set function metadata.
module.update_fn_metadata(hash, ["x: i64", "i64"]);

// Load the module into the Engine as a new package.
let mut engine = Engine::new();
engine.load_package(module);

engine.eval::<i64>("inc(41)")? == 42;       // no need to import module
```


Make the `Module` a Global Module
------------------------------------

`Engine::register_module` loads a [module] and makes it available globally under a specific namespace.

```rust
use rhai::{Engine, Module};

let mut module = Module::new();             // new module

// Use the 'set_fn_XXX' API to add functions.
let hash = module.set_fn_1("inc", |x: i64| Ok(x+1));

// Remember to update the parameter names/types and return type metadata.
// 'set_fn_XXX' by default does not set function metadata.
module.update_fn_metadata(hash, ["x: i64", "i64"]);

// Load the module into the Engine as a sub-module named 'calc'
let mut engine = Engine::new();
engine.register_module("calc", module);

engine.eval::<i64>("calc::inc(41)")? == 42; // refer to the 'Calc' module
```

`Module::set_fn_XXX_mut` can expose functions (usually _methods_) in the module
to the _global_ namespace, so [getters/setters] and [indexers] for [custom types] can work as expected.

[Type iterators], because of their special nature, are always exposed to the _global_ namespace.

```rust
use rhai::{Engine, Module, FnNamespace};

let mut module = Module::new();             // new module

// Expose method 'inc' to the global namespace (default is 'Internal')
let hash = module.set_fn_1_mut("inc", FnNamespace::Global, |x: &mut i64| Ok(x+1));

// Remember to update the parameter names/types and return type metadata.
// 'set_fn_XXX' by default does not set function metadata.
module.update_fn_metadata(hash, ["x: &mut i64", "i64"]);

// Load the module into the Engine as a sub-module named 'calc'
let mut engine = Engine::new();
engine.register_module("calc", module);

// The method 'inc' works as expected because it is exposed to the global namespace
engine.eval::<i64>("let x = 41; x.inc()")? == 42;
```


Make the `Module` Dynamically Loadable
-------------------------------------

In order to dynamically load a custom module, there must be a [module resolver] which serves
the module when loaded via `import` statements.

The easiest way is to use, for example, the [`StaticModuleResolver`][module resolver] to hold such
a custom module.

```rust
use rhai::{Engine, Scope, Module};
use rhai::module_resolvers::StaticModuleResolver;

let mut module = Module::new();             // new module
module.set_var("answer", 41_i64);           // variable 'answer' under module
module.set_fn_1("inc", |x: i64| Ok(x+1));   // use the 'set_fn_XXX' API to add functions

// Create the module resolver
let mut resolver = StaticModuleResolver::new();

// Add the module into the module resolver under the name 'question'
// They module can then be accessed via: 'import "question" as q;'
resolver.insert("question", module);

// Set the module resolver into the 'Engine'
let mut engine = Engine::new();
engine.set_module_resolver(Some(resolver));

// Use namespace-qualified variables
engine.eval::<i64>(r#"import "question" as q; q::answer + 1"#)? == 42;

// Call namespace-qualified functions
engine.eval::<i64>(r#"import "question" as q; q::inc(q::answer)"#)? == 42;
```
