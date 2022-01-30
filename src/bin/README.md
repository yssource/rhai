Rhai Tools
==========

Tools for running Rhai scripts.

| Tool                                                                             | Required feature(s) | Description                                         |
| -------------------------------------------------------------------------------- | :-----------------: | --------------------------------------------------- |
| [`rhai-run`](https://github.com/rhaiscript/rhai/blob/main/src/bin/rhai-run.rs)   |                     | runs each filename passed to it as a Rhai script    |
| [`rhai-repl`](https://github.com/rhaiscript/rhai/blob/main/src/bin/rhai-repl.rs) |     `rustyline`     | simple REPL that interactively evaluates statements |
| [`rhai-dbg`](https://github.com/rhaiscript/rhai/blob/main/src/bin/rhai-dbg.rs)   |     `debugging`     | the _Rhai Debugger_                                 |


How to Run
----------

```sh
cargo run --bin sample_app_to_run
```

or with required features

```sh
cargo run --bin sample_app_to_run --features feature1,feature2,feature3
```


How to Install
--------------

To install these all tools (with [`decimal`] and [`metadata`] support), use the following command:

```sh
cargo install --path . --bins  --features decimal,metadata,debugging,rustyline
```

or specifically:

```sh
cargo install --path . --bin rhai-run  --features decimal,metadata,debugging,rustyline
```
