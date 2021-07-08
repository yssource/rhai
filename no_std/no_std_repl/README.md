`no-std` REPL Sample
====================

This sample application is the same version as the `rhai-repl` tool, compiled for `no-std`.

[`wee_alloc`](https://crates.io/crates/wee_alloc) is used as the allocator.


To Compile
----------

The nightly compiler is required:

```bash
cargo +nightly build --profile unix -Z unstable-options
```

Available profiles are: `unix`, `windows` and `macos`.

The release build is optimized for size.  It can be changed to optimize on speed instead.
