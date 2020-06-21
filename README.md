Rhai - Embedded Scripting for Rust
=================================

![GitHub last commit](https://img.shields.io/github/last-commit/jonathandturner/rhai)
[![Travis (.org)](https://img.shields.io/travis/jonathandturner/rhai)](http://travis-ci.org/jonathandturner/rhai)
[![license](https://img.shields.io/github/license/jonathandturner/rhai)](https://github.com/license/jonathandturner/rhai)
[![crates.io](https://img.shields.io/crates/v/rhai.svg)](https:/crates.io/crates/rhai/)
![crates.io](https://img.shields.io/crates/d/rhai)
[![API Docs](https://docs.rs/rhai/badge.svg)](https://docs.rs/rhai/)

Rhai is an embedded scripting language and evaluation engine for Rust that gives a safe and easy way
to add scripting to any application.

Supported targets and builds
---------------------------

* All common CPU targets for Windows, Linux and MacOS.
* WebAssembly (WASM)
* `no-std`

Features
--------

* Easy-to-use language similar to JS+Rust with dynamic typing.
* Tight integration with native Rust [functions](https://schungx.github.io/rhai/rust/functions.html) and [types]([#custom-types-and-methods](https://schungx.github.io/rhai/rust/custom.html)), including [getters/setters](https://schungx.github.io/rhai/rust/getters-setters.html), [methods](https://schungx.github.io/rhai/rust/custom.html) and [indexers](https://schungx.github.io/rhai/rust/indexers.html).
* Freely pass Rust variables/constants into a script via an external [`Scope`](https://schungx.github.io/rhai/rust/scope.html).
* Easily [call a script-defined function](https://schungx.github.io/rhai/engine/call-fn.html) from Rust.
* Fairly low compile-time overhead.
* Fairly efficient evaluation (1 million iterations in 0.25 sec on a single core, 2.3 GHz Linux VM).
* Relatively little `unsafe` code (yes there are some for performance reasons, and most `unsafe` code is limited to
  one single source file, all with names starting with `"unsafe_"`).
* Re-entrant scripting engine can be made `Send + Sync` (via the [`sync`] feature).
* Sand-boxed - the scripting engine, if declared immutable, cannot mutate the containing environment unless explicitly permitted (e.g. via a `RefCell`).
* Rugged - protection against malicious attacks (such as [stack-overflow](https://schungx.github.io/rhai/safety/max-call-stack.html), [over-sized data](https://schungx.github.io/rhai/safety/max-string-size.html), and [runaway scripts](https://schungx.github.io/rhai/safety/max-operations.html) etc.) that may come from untrusted third-party user-land scripts.
* Track script evaluation [progress](https://schungx.github.io/rhai/safety/progress.html) and manually terminate a script run.
* [Function overloading](https://schungx.github.io/rhai/language/overload.html).
* [Operator overloading](https://schungx.github.io/rhai/rust/operators.html).
* Organize code base with dynamically-loadable [modules](https://schungx.github.io/rhai/language/modules.html).
* Scripts are [optimized](https://schungx.github.io/rhai/engine/optimize.html) (useful for template-based machine-generated scripts) for repeated evaluations.
* Support for [minimal builds](https://schungx.github.io/rhai/start/builds/minimal.html) by excluding unneeded language [features](https://schungx.github.io/rhai/start/features.html).

Documentation
-------------

See [The Rhai Book](https://schungx.github.io/rhai) for details on the Rhai scripting engine and language.
