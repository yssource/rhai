Built-In Packages
================

{{#include ../../links.md}}

`Engine::new` creates an [`Engine`] with the `StandardPackage` loaded.

`Engine::new_raw` creates an [`Engine`] with _no_ package loaded.

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
| `BasicFnPackage`       | Basic methods for [function pointers].                                                                 |    Yes    |      Yes      |
| `EvalPackage`          | Disable [`eval`]                                                                                       |    No     |      No       |
| `CorePackage`          | Basic essentials                                                                                       |    Yes    |      Yes      |
| `StandardPackage`      | Standard library (default for `Engine::new`)                                                           |    No     |      Yes      |


Load the `CorePackage`
---------------------

If only minimal functionalities is required, load the `CorePackage` instead:

```rust
use rhai::Engine;
use rhai::packages::{Package, CorePackage};

let mut engine = Engine::new_raw();
let package = CorePackage::new();

engine.load_package(package.get());
```
