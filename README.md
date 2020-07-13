Rhai - Embedded Scripting for Rust
=================================

![GitHub last commit](https://img.shields.io/github/last-commit/jonathandturner/rhai)
[![Travis (.org)](https://img.shields.io/travis/jonathandturner/rhai)](http://travis-ci.org/jonathandturner/rhai)
[![license](https://img.shields.io/github/license/jonathandturner/rhai)](https://github.com/license/jonathandturner/rhai)
[![crates.io](https://img.shields.io/crates/v/rhai.svg)](https://crates.io/crates/rhai/)
[![crates.io](https://img.shields.io/crates/d/rhai)](https://crates.io/crates/rhai/)
[![API Docs](https://docs.rs/rhai/badge.svg)](https://docs.rs/rhai/)

Rhai is an embedded scripting language and evaluation engine for Rust that gives a safe and easy way
to add scripting to any application.

Supported targets and builds
---------------------------

* All common CPU targets for Windows, Linux and MacOS.
* WebAssembly (WASM)
* `no-std`

Standard Features
----------------

* Easy-to-use language similar to JavaScript+Rust with dynamic typing.
* Tight integration with native Rust [functions](https://schungx.github.io/rhai/rust/functions.html) and [types]([#custom-types-and-methods](https://schungx.github.io/rhai/rust/custom.html)), including [getters/setters](https://schungx.github.io/rhai/rust/getters-setters.html), [methods](https://schungx.github.io/rhai/rust/custom.html) and [indexers](https://schungx.github.io/rhai/rust/indexers.html).
* Freely pass Rust variables/constants into a script via an external [`Scope`](https://schungx.github.io/rhai/rust/scope.html).
* Easily [call a script-defined function](https://schungx.github.io/rhai/engine/call-fn.html) from Rust.
* Fairly low compile-time overhead.
* Fairly efficient evaluation (1 million iterations in 0.3 sec on a single core, 2.3 GHz Linux VM).
* Relatively little `unsafe` code (yes there are some for performance reasons, and most `unsafe` code is limited to
  one single source file, all with names starting with `"unsafe_"`).
* Re-entrant scripting engine can be made `Send + Sync` (via the `sync` feature).
* [Function overloading](https://schungx.github.io/rhai/language/overload.html).
* [Operator overloading](https://schungx.github.io/rhai/rust/operators.html).
* Dynamic dispatch via [function pointers](https://schungx.github.io/rhai/language/fn-ptr.html).
* Some support for [object-oriented programming (OOP)](https://schungx.github.io/rhai/language/oop.html).
* Organize code base with dynamically-loadable [modules](https://schungx.github.io/rhai/language/modules.html).
* Serialization/deserialization support via [serde](https://crates.io/crates/serde) (requires the `serde` feature).
* Scripts are [optimized](https://schungx.github.io/rhai/engine/optimize.html) (useful for template-based machine-generated scripts) for repeated evaluations.
* Support for [minimal builds](https://schungx.github.io/rhai/start/builds/minimal.html) by excluding unneeded language [features](https://schungx.github.io/rhai/start/features.html).

Protection Against Attacks
-------------------------

* Sand-boxed - the scripting engine, if declared immutable, cannot mutate the containing environment unless explicitly permitted (e.g. via a `RefCell`).
* Rugged - protected against malicious attacks (such as [stack-overflow](https://schungx.github.io/rhai/safety/max-call-stack.html), [over-sized data](https://schungx.github.io/rhai/safety/max-string-size.html), and [runaway scripts](https://schungx.github.io/rhai/safety/max-operations.html) etc.) that may come from untrusted third-party user-land scripts.
* Track script evaluation [progress](https://schungx.github.io/rhai/safety/progress.html) and manually terminate a script run.

For Those Who Actually Want Their Own Language
---------------------------------------------

* Use as a [DSL](https://schungx.github.io/rhai/engine/dsl.html).
* Define [custom operators](https://schungx.github.io/rhai/engine/custom-op.html).
* Restrict the language by surgically [disabling keywords and operators](https://schungx.github.io/rhai/engine/disable.html).
* Extend the language with [custom syntax](https://schungx.github.io/rhai/engine/custom-syntax.html).

Documentation
-------------

See [The Rhai Book](https://schungx.github.io/rhai) for details on the Rhai scripting engine and language.

Playground
----------

An [Online Playground](https://alvinhochun.github.io/rhai-demo/) is available with syntax-highlighting editor.
Scripts can be evaluated directly from the editor.
