Rhai - Embedded Scripting for Rust
=================================

![GitHub last commit](https://img.shields.io/github/last-commit/jonathandturner/rhai?logo=github)
[![Build Status](https://github.com/jonathandturner/rhai/workflows/Build/badge.svg)](https://github.com/jonathandturner/rhai/actions)
[![license](https://img.shields.io/crates/l/rhai)](https://github.com/license/jonathandturner/rhai)
[![crates.io](https://img.shields.io/crates/v/rhai?logo=rust)](https://crates.io/crates/rhai/)
[![crates.io](https://img.shields.io/crates/d/rhai?logo=rust)](https://crates.io/crates/rhai/)
[![API Docs](https://docs.rs/rhai/badge.svg?logo=docs.rs)](https://docs.rs/rhai/)
[![chat](https://img.shields.io/discord/767611025456889857.svg?logo=discord)](https://discord.gg/HquqbYFcZ9)
[![Reddit](https://img.shields.io/reddit/subreddit-subscribers/Rhai?logo=reddit)](https://www.reddit.com/r/Rhai)

Rhai is an embedded scripting language and evaluation engine for Rust that gives a safe and easy way
to add scripting to any application.


Supported targets and builds
---------------------------

* All common CPU targets for Windows, Linux and MacOS.
* WebAssembly (WASM)
* `no-std`
* Minimum Rust version 1.45


Standard features
-----------------

* Easy-to-use language similar to JavaScript+Rust with dynamic typing.
* Fairly low compile-time overhead.
* Fairly efficient evaluation (1 million iterations in 0.3 sec on a single core, 2.3 GHz Linux VM).
* Tight integration with native Rust [functions](https://schungx.github.io/rhai/rust/functions.html) and [types]([#custom-types-and-methods](https://schungx.github.io/rhai/rust/custom.html)), including [getters/setters](https://schungx.github.io/rhai/rust/getters-setters.html), [methods](https://schungx.github.io/rhai/rust/custom.html) and [indexers](https://schungx.github.io/rhai/rust/indexers.html).
* Freely pass Rust variables/constants into a script via an external [`Scope`](https://schungx.github.io/rhai/rust/scope.html).
* Easily [call a script-defined function](https://schungx.github.io/rhai/engine/call-fn.html) from Rust.
* Relatively little `unsafe` code (yes there are some for performance reasons).
* Few dependencies (currently only [`smallvec`](https://crates.io/crates/smallvec)).
* Re-entrant scripting engine can be made `Send + Sync` (via the `sync` feature).
* Scripts are [optimized](https://schungx.github.io/rhai/engine/optimize.html) (useful for template-based machine-generated scripts) for repeated evaluations.
* Easy custom API development via [plugins](https://schungx.github.io/rhai/plugins/index.html) system powered by procedural macros.
* [Function overloading](https://schungx.github.io/rhai/language/overload.html) and [operator overloading](https://schungx.github.io/rhai/rust/operators.html).
* Dynamic dispatch via [function pointers](https://schungx.github.io/rhai/language/fn-ptr.html) with additional support for [currying](https://schungx.github.io/rhai/language/fn-curry.html).
* [Closures](https://schungx.github.io/rhai/language/fn-closure.html) (anonymous functions) that can capture shared values.
* Some syntactic support for [object-oriented programming (OOP)](https://schungx.github.io/rhai/language/oop.html).
* Organize code base with dynamically-loadable [modules](https://schungx.github.io/rhai/language/modules.html).
* Serialization/deserialization support via [serde](https://crates.io/crates/serde) (requires the `serde` feature).
* Support for [minimal builds](https://schungx.github.io/rhai/start/builds/minimal.html) by excluding unneeded language [features](https://schungx.github.io/rhai/start/features.html).


Protected against attacks
-------------------------

* Sand-boxed - the scripting engine, if declared immutable, cannot mutate the containing environment unless [explicitly permitted](https://schungx.github.io/rhai/patterns/control.html).
* Rugged - protected against malicious attacks (such as [stack-overflow](https://schungx.github.io/rhai/safety/max-call-stack.html), [over-sized data](https://schungx.github.io/rhai/safety/max-string-size.html), and [runaway scripts](https://schungx.github.io/rhai/safety/max-operations.html) etc.) that may come from untrusted third-party user-land scripts.
* Track script evaluation [progress](https://schungx.github.io/rhai/safety/progress.html) and manually terminate a script run.


For those who actually want their own language
---------------------------------------------

* Use as a [DSL](https://schungx.github.io/rhai/engine/dsl.html).
* Restrict the language by surgically [disabling keywords and operators](https://schungx.github.io/rhai/engine/disable.html).
* Define [custom operators](https://schungx.github.io/rhai/engine/custom-op.html).
* Extend the language with [custom syntax](https://schungx.github.io/rhai/engine/custom-syntax.html).


Documentation
-------------

See [The Rhai Book](https://schungx.github.io/rhai) for details on the Rhai scripting engine and language.

To build _The Book_, first install [`mdbook`](https://github.com/rust-lang/mdBook)
and [`mdbook-tera`](https://github.com/avitex/mdbook-tera) (for templating).
Running `mdbook build` builds it.


Playground
----------

An [Online Playground](https://alvinhochun.github.io/rhai-demo/) is available with syntax-highlighting editor.
Scripts can be evaluated directly from the editor.


License
-------

Licensed under either of the following, at your choice:

* [Apache License, Version 2.0](https://github.com/jonathandturner/rhai/blob/master/LICENSE-APACHE.txt), or
* [MIT license](https://github.com/jonathandturner/rhai/blob/master/LICENSE-MIT.txt)

Unless explicitly stated otherwise, any contribution intentionally submitted
for inclusion in this crate, as defined in the Apache-2.0 license, shall
be dual-licensed as above, without any additional terms or conditions.
