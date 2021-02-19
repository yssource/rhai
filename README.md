Rhai - Embedded Scripting for Rust
=================================

![GitHub last commit](https://img.shields.io/github/last-commit/rhaiscript/rhai?logo=github)
[![Build Status](https://github.com/rhaiscript/rhai/workflows/Build/badge.svg)](https://github.com/rhaiscript/rhai/actions)
[![license](https://img.shields.io/crates/l/rhai)](https://github.com/license/rhaiscript/rhai)
[![crates.io](https://img.shields.io/crates/v/rhai?logo=rust)](https://crates.io/crates/rhai/)
[![crates.io](https://img.shields.io/crates/d/rhai?logo=rust)](https://crates.io/crates/rhai/)
[![API Docs](https://docs.rs/rhai/badge.svg?logo=docs.rs)](https://docs.rs/rhai/)
[![chat](https://img.shields.io/discord/767611025456889857.svg?logo=discord)](https://discord.gg/HquqbYFcZ9)
[![Reddit](https://img.shields.io/reddit/subreddit-subscribers/Rhai?logo=reddit)](https://www.reddit.com/r/Rhai)

![Rhai logo](https://rhai.rs/book/images/logo/rhai-banner-transparent-colour.svg)

Rhai is an embedded scripting language and evaluation engine for Rust that gives a safe and easy way
to add scripting to any application.


Supported targets and builds
---------------------------

* All common CPU targets for Windows, Linux and MacOS.
* WebAssembly (WASM)
* `no-std`
* Minimum Rust version 1.49


Standard features
-----------------

* Simple language similar to JavaScript+Rust with dynamic typing.
* Fairly efficient evaluation (1 million iterations in 0.3 sec on a single-core, 2.3 GHz Linux VM).
* Tight integration with native Rust [functions](https://rhai.rs/book/rust/functions.html) and [types]([#custom-types-and-methods](https://rhai.rs/book/rust/custom.html)), including [getters/setters](https://rhai.rs/book/rust/getters-setters.html), [methods](https://rhai.rs/book/rust/custom.html) and [indexers](https://rhai.rs/book/rust/indexers.html).
* Freely pass Rust variables/constants into a script via an external [`Scope`](https://rhai.rs/book/rust/scope.html) - all clonable Rust types are supported; no need to implement any special trait.
* Built-in support for most common [data types](https://rhai.rs/book/language/values-and-types.html) including booleans, integers, floating-point numbers (including [`Decimal`](https://crates.io/crates/rust_decimal)), strings, Unicode characters, arrays and maps.
* Easily [call a script-defined function](https://rhai.rs/book/engine/call-fn.html) from Rust.
* Relatively little `unsafe` code (yes there are some for performance reasons).
* Few dependencies (currently only [`smallvec`](https://crates.io/crates/smallvec) and [`ahash`](https://crates.io/crates/ahash)).
* Re-entrant scripting engine can be made `Send + Sync` (via the `sync` feature).
* Compile once to [AST](https://rhai.rs/book/engine/compile.html) form for repeated evaluations.
* Scripts are [optimized](https://rhai.rs/book/engine/optimize.html) (useful for template-based machine-generated scripts).
* Easy custom API development via [plugins](https://rhai.rs/book/plugins/index.html) system powered by procedural macros.
* [Function overloading](https://rhai.rs/book/language/overload.html) and [operator overloading](https://rhai.rs/book/rust/operators.html).
* Dynamic dispatch via [function pointers](https://rhai.rs/book/language/fn-ptr.html) with additional support for [currying](https://rhai.rs/book/language/fn-curry.html).
* [Closures](https://rhai.rs/book/language/fn-closure.html) (anonymous functions) that can capture shared values.
* Some syntactic support for [object-oriented programming (OOP)](https://rhai.rs/book/language/oop.html).
* Organize code base with dynamically-loadable [modules](https://rhai.rs/book/language/modules.html), optionally overriding the resolution process
* Serialization/deserialization support via [serde](https://crates.io/crates/serde) (requires the `serde` feature).
* Support for [minimal builds](https://rhai.rs/book/start/builds/minimal.html) by excluding unneeded language [features](https://rhai.rs/book/start/features.html).


Protected against attacks
-------------------------

* Sand-boxed - the scripting engine, if declared immutable, cannot mutate the containing environment unless [explicitly permitted](https://rhai.rs/book/patterns/control.html).
* Rugged - protected against malicious attacks (such as [stack-overflow](https://rhai.rs/book/safety/max-call-stack.html), [over-sized data](https://rhai.rs/book/safety/max-string-size.html), and [runaway scripts](https://rhai.rs/book/safety/max-operations.html) etc.) that may come from untrusted third-party user-land scripts.
* Track script evaluation [progress](https://rhai.rs/book/safety/progress.html) and manually terminate a script run.


For those who actually want their own language
---------------------------------------------

* Use as a [DSL](https://rhai.rs/book/engine/dsl.html).
* Restrict the language by surgically [disabling keywords and operators](https://rhai.rs/book/engine/disable.html).
* Define [custom operators](https://rhai.rs/book/engine/custom-op.html).
* Extend the language with [custom syntax](https://rhai.rs/book/engine/custom-syntax.html).


Documentation
-------------

See [The Rhai Book](https://rhai.rs/book) for details on the Rhai scripting engine and language.


Playground
----------

An [Online Playground](https://rhai.rs/playground) is available with
syntax-highlighting editor, powered by WebAssembly.

Scripts can be evaluated directly from the editor.


License
-------

Licensed under either of the following, at your choice:

* [Apache License, Version 2.0](https://github.com/rhaiscript/rhai/blob/master/LICENSE-APACHE.txt), or
* [MIT license](https://github.com/rhaiscript/rhai/blob/master/LICENSE-MIT.txt)

Unless explicitly stated otherwise, any contribution intentionally submitted
for inclusion in this crate, as defined in the Apache-2.0 license, shall
be dual-licensed as above, without any additional terms or conditions.
