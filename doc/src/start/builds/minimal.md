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


Opt-Out of Features
------------------

Opt out of as many features as possible, if they are not needed, to reduce code size because, remember, by default
all code is compiled in as what a script requires cannot be predicted. If a language feature is not needed,
omitting them via special features is a prudent strategy to optimize the build for size.

Omitting arrays ([`no_index`]) yields the most code-size savings, followed by floating-point support
([`no_float`]), checked arithmetic/script resource limits ([`unchecked`]) and finally object maps and custom types ([`no_object`]).

Where the usage scenario does not call for loading externally-defined modules, use [`no_module`] to save some bytes.
Disable script-defined functions ([`no_function`]) only when the feature is not needed because code size savings is minimal.


Use a Raw [`Engine`]
-------------------

[`Engine::new_raw`](#raw-engine) creates a _raw_ engine.
A _raw_ engine supports, out of the box, only a very [restricted set](#built-in-operators) of basic arithmetic and logical operators.
Selectively include other necessary functionalities by loading specific [packages] to minimize the footprint.
Packages are sharable (even across threads via the [`sync`] feature), so they only have to be created once.
