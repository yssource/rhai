Features
========

{{#include ../links.md}}

Easy
----

* Easy-to-use language similar to JavaScript+Rust with dynamic typing.

* Tight integration with native Rust [functions]({{rootUrl}}/rust/functions.md) and [types]({{rootUrl}}/rust/custom.md), including [getters/setters]({{rootUrl}}/rust/getters-setters.md), [methods]({{rootUrl}}/rust/custom.md) and [indexers]({{rootUrl}}/rust/indexers.md).

* Freely pass Rust variables/constants into a script via an external [`Scope`].

* Easily [call a script-defined function]({{rootUrl}}/engine/call-fn.md) from Rust.

* Very few additional dependencies (right now only [`num-traits`](https://crates.io/crates/num-traits/) to do checked arithmetic operations);
  for [`no-std`] builds, a number of additional dependencies are pulled in to provide for functionalities that used to be in `std`.

Fast
----

* Fairly low compile-time overhead.

* Fairly efficient evaluation (1 million iterations in 0.25 sec on a single core, 2.3 GHz Linux VM).

* Scripts are [optimized]({{rootUrl}}/engine/optimize.md) (useful for template-based machine-generated scripts) for repeated evaluations.

Dynamic
-------

* [Function overloading]({{rootUrl}}/language/overload.md).

* [Operator overloading]({{rootUrl}}/rust/operators.md).

* Organize code base with dynamically-loadable [modules].

* [Function pointers].

Safe
----

* Relatively little `unsafe` code (yes there are some for performance reasons, and most `unsafe` code is limited to
  one single source file, all with names starting with `"unsafe_"`).

Rugged
------

* Sand-boxed - the scripting [`Engine`], if declared immutable, cannot mutate the containing environment unless explicitly permitted (e.g. via a `RefCell`).

* Protected against malicious attacks (such as [stack-overflow]({{rootUrl}}/safety/max-call-stack.md), [over-sized data]({{rootUrl}}/safety/max-string-size.md), and [runaway scripts]({{rootUrl}}/safety/max-operations.md) etc.) that may come from untrusted third-party user-land scripts.

* Track script evaluation [progress] and manually terminate a script run.

Flexible
--------

* Re-entrant scripting [`Engine`] can be made `Send + Sync` (via the [`sync`] feature).

* Support for [minimal builds] by excluding unneeded language [features].

* Supports [most build targets](targets.md) including `no-std` and [WASM].
