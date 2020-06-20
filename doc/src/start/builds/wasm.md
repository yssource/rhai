Building to WebAssembly (WASM)
=============================

{{#include ../../links.md}}

It is possible to use Rhai when compiling to WebAssembly (WASM). This yields a scripting engine (and language)
that can be run in a standard web browser.

Why you would want to is another matter... as there is already a nice, fast, complete scripting language
for the the common WASM environment (i.e. a browser) - and it is called JavaScript.

But anyhow, do it because you _can_!

When building for WASM, certain features will not be available, such as the script file API's and loading modules
from external script files.

Also look into [minimal builds] to reduce generated WASM size.  As of this version, a typical, full-featured
Rhai scripting engine compiles to a single WASM file less than 200KB gzipped. When excluding features that are
marginal in WASM environment, the gzipped payload can be further shrunk to 160KB.

In benchmark tests, a WASM build runs scripts roughly 1.7-2.2x slower than a native optimized release build.
