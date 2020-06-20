Packages
========

{{#include ../links.md}}

Standard built-in Rhai features are provided in various _packages_ that can be loaded via a call to `Engine::load_package`.

Packages reside under `rhai::packages::*` and the trait `rhai::packages::Package` must be loaded in order for
packages to be used.

```rust
use rhai::Engine;
use rhai::packages::Package         // load the 'Package' trait to use packages
use rhai::packages::CorePackage;    // the 'core' package contains basic functionalities (e.g. arithmetic)

let mut engine = Engine::new_raw(); // create a 'raw' Engine
let package = CorePackage::new();   // create a package - can be shared among multiple `Engine` instances

engine.load_package(package.get()); // load the package manually. 'get' returns a reference to the shared package
```

The follow packages are available:

| Package                | Description                                                                                            | In `Core` | In `Standard` |
| ---------------------- | ------------------------------------------------------------------------------------------------------ | :-------: | :-----------: |
| `ArithmeticPackage`    | Arithmetic operators (e.g. `+`, `-`, `*`, `/`) for numeric types that are not built in (e.g. `u16`)    |    Yes    |      Yes      |
| `BasicIteratorPackage` | Numeric ranges (e.g. `range(1, 10)`)                                                                   |    Yes    |      Yes      |
| `LogicPackage`         | Logical and comparison operators (e.g. `==`, `>`) for numeric types that are not built in (e.g. `u16`) |    Yes    |      Yes      |
| `BasicStringPackage`   | Basic string functions (e.g. `print`, `debug`, `len`) that are not built in                            |    Yes    |      Yes      |
| `BasicTimePackage`     | Basic time functions (e.g. [timestamps])                                                               |    Yes    |      Yes      |
| `MoreStringPackage`    | Additional string functions, including converting common types to string                               |    No     |      Yes      |
| `BasicMathPackage`     | Basic math functions (e.g. `sin`, `sqrt`)                                                              |    No     |      Yes      |
| `BasicArrayPackage`    | Basic [array] functions (not available under `no_index`)                                               |    No     |      Yes      |
| `BasicMapPackage`      | Basic [object map] functions (not available under `no_object`)                                         |    No     |      Yes      |
| `EvalPackage`          | Disable [`eval`]                                                                                       |    No     |      No       |
| `CorePackage`          | Basic essentials                                                                                       |    Yes    |      Yes      |
| `StandardPackage`      | Standard library (default for `Engine::new`)                                                           |    No     |      Yes      |

Packages typically contain Rust functions that are callable within a Rhai script.
All functions registered in a package is loaded under the _global namespace_ (i.e. they're available without module qualifiers).

Once a package is created (e.g. via `new`), it can be _shared_ (via `get`) among multiple instances of [`Engine`],
even across threads (under [`sync`]). Therefore, a package only has to be created _once_.

Packages are actually implemented as [modules], so they share a lot of behavior and characteristics.
The main difference is that a package loads under the _global_ namespace, while a module loads under its own
namespace alias specified in an [`import`] statement (see also [modules]).

A package is _static_ (i.e. pre-loaded into an [`Engine`]), while a module is _dynamic_ (i.e. loaded with
the `import` statement).

Custom packages can also be created.  See the macro [`def_package!`](https://docs.rs/rhai/0.13.0/rhai/macro.def_package.html).
