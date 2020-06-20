Install the Rhai Crate
=====================

{{#include ../links.md}}

Install the Rhai crate from [`crates.io`](https:/crates.io/crates/rhai/) by adding this line
under `dependencies` in `Cargo.toml`:

```toml
[dependencies]
rhai = "0.15.2"
```

Use the latest released crate version on [`crates.io`](https:/crates.io/crates/rhai/):

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
