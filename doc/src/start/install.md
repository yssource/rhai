Install the Rhai Crate
=====================

{{#include ../links.md}}

Install the Rhai crate from [`crates.io`](https:/crates.io/crates/rhai/), start by looking up the
latest version and adding this line under `dependencies` in `Cargo.toml`:

```toml
[dependencies]
rhai = "{{version}}"    # assuming {{version}} is the latest version
```

Or to automatically use the latest released crate version on [`crates.io`](https:/crates.io/crates/rhai/):

```toml
[dependencies]
rhai = "*"
```

Crate versions are released on [`crates.io`](https:/crates.io/crates/rhai/) infrequently,
so to track the latest features, enhancements and bug fixes, pull directly from GitHub:

```toml
[dependencies]
rhai = { git = "https://github.com/jonathandturner/rhai" }
```
