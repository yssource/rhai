`no-std` Sample for Windows API
==============================

This sample application is a bare-bones `no-std` build for the Windows API.

[`wee_alloc`](https://crates.io/crates/wee_alloc) is used as the allocator.


To Compile
----------

The nightly compiler is required:

```bash
cargo +nightly build --release
```

The release build is optimized for size.  It can be changed to optimize on speed instead.
