Built-In Packages
================

{{#include ../../links.md}}

`Engine::new` creates an [`Engine`] with the `StandardPackage` loaded.

`Engine::new_raw` creates an [`Engine`] with _no_ package loaded.

| Package                | Description                                                                                            | In `Core` | In `Standard` |
| ---------------------- | ------------------------------------------------------------------------------------------------------ | :-------: | :-----------: |
| `ArithmeticPackage`    | arithmetic operators (e.g. `+`, `-`, `*`, `/`) for numeric types that are not built in (e.g. `u16`)    |    yes    |      yes      |
| `BasicIteratorPackage` | numeric ranges (e.g. `range(1, 10)`)                                                                   |    yes    |      yes      |
| `LogicPackage`         | logical and comparison operators (e.g. `==`, `>`) for numeric types that are not built in (e.g. `u16`) |    yes    |      yes      |
| `BasicStringPackage`   | basic string functions (e.g. `print`, `debug`, `len`) that are not built in                            |    yes    |      yes      |
| `BasicTimePackage`     | basic time functions (e.g. [timestamps])                                                               |    yes    |      yes      |
| `MoreStringPackage`    | additional string functions, including converting common types to string                               |    no     |      yes      |
| `BasicMathPackage`     | basic math functions (e.g. `sin`, `sqrt`)                                                              |    no     |      yes      |
| `BasicArrayPackage`    | basic [array] functions (not available under `no_index`)                                               |    no     |      yes      |
| `BasicMapPackage`      | basic [object map] functions (not available under `no_object`)                                         |    no     |      yes      |
| `BasicFnPackage`       | basic methods for [function pointers].                                                                 |    yes    |      yes      |
| `EvalPackage`          | disable [`eval`]                                                                                       |    no     |      no       |
| `CorePackage`          | basic essentials                                                                                       |    yes    |      yes      |
| `StandardPackage`      | standard library (default for `Engine::new`)                                                           |    no     |      yes      |


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
