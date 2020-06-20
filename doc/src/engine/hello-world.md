Hello World in Rhai
===================

{{#include ../links.md}}

To get going with Rhai is as simple as creating an instance of the scripting engine `rhai::Engine` via
`Engine::new`, then calling the `eval` method:

```rust
use rhai::{Engine, EvalAltResult};

fn main() -> Result<(), Box<EvalAltResult>>
{
    let engine = Engine::new();

    let result = engine.eval::<i64>("40 + 2")?;
    //                      ^^^^^^^ cast the result to an 'i64', this is required

    println!("Answer: {}", result);             // prints 42

    Ok(())
}
```

`rhai::EvalAltResult` is a Rust `enum` containing all errors encountered during the parsing or evaluation process.


Evaluate a Script
----------------

The type parameter is used to specify the type of the return value, which _must_ match the actual type or an error is returned.
Rhai is very strict here.

Use [`Dynamic`] for uncertain return types.

There are two ways to specify the return type - _turbofish_ notation, or type inference.

```rust
let result = engine.eval::<i64>("40 + 2")?;     // return type is i64, specified using 'turbofish' notation

let result: i64 = engine.eval("40 + 2")?;       // return type is inferred to be i64

result.is::<i64>() == true;

let result: Dynamic = engine.eval("boo()")?;    // use 'Dynamic' if you're not sure what type it'll be!

let result = engine.eval::<String>("40 + 2")?;  // returns an error because the actual return type is i64, not String
```

Evaluate a script file directly:

```rust
let result = engine.eval_file::<i64>("hello_world.rhai".into())?;   // 'eval_file' takes a 'PathBuf'
```
