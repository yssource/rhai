Minimal Build
=============

{{#include ../../links.md}}

Configuration
-------------

In order to compile a _minimal_ build - i.e. a build optimized for size - perhaps for `no-std` embedded targets or for
compiling to [WASM], it is essential that the correct linker flags are used in `cargo.toml`:

```toml
[profile.release]
lto = "fat"         # turn on Link-Time Optimizations
codegen-units = 1   # trade compile time with maximum optimization
opt-level = "z"     # optimize for size
```


Use `i32` Only
--------------

For embedded systems that must optimize for code size, the architecture is commonly 32-bit.
Use [`only_i32`] to prune away large sections of code implementing functions for other numeric types
(including `i64`).

If, for some reason, 64-bit long integers must be supported, use [`only_i64`] instead of [`only_i32`].


Opt-Out of Features
------------------

Opt out of as many features as possible, if they are not needed, to reduce code size because, remember, by default
all code is compiled in as what a script requires cannot be predicted. If a language feature is not needed,
omitting them via special features is a prudent strategy to optimize the build for size.

Omitting arrays ([`no_index`]) yields the most code-size savings, followed by floating-point support
([`no_float`]), checked arithmetic/script resource limits ([`unchecked`]) and finally object maps and custom types ([`no_object`]).

Where the usage scenario does not call for loading externally-defined modules, use [`no_module`] to save some bytes.
Disable script-defined functions ([`no_function`]) when the feature is not needed.
Both of these have little code size savings.


Use a Raw [`Engine`]
-------------------

[`Engine::new_raw`][raw `Engine`] creates a _raw_ engine.
A _raw_ engine supports, out of the box, only a very [restricted set]({{rootUrl}}/engine/raw.md#built-in-operators)
of basic arithmetic and logical operators.

Selectively include other necessary functionalities by loading specific [packages] to minimize the footprint.

Packages are sharable (even across threads via the [`sync`] feature), so they only have to be created once.
