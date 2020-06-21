Eager Function Evaluation When Using Full Optimization Level
==========================================================

{{#include ../../links.md}}

When the optimization level is [`OptimizationLevel::Full`], the [`Engine`] assumes all functions to be _pure_ and will _eagerly_
evaluated all function calls with constant arguments, using the result to replace the call.

This also applies to all operators (which are implemented as functions).

For instance, the same example above:

```rust
// When compiling the following with OptimizationLevel::Full...

const DECISION = 1;
                            // this condition is now eliminated because 'DECISION == 1'
if DECISION == 1 {          // is a function call to the '==' function, and it returns 'true'
    print("hello!");        // this block is promoted to the parent level
} else {
    print("boo!");          // this block is eliminated because it is never reached
}

print("hello!");            // <- the above is equivalent to this
                            //    ('print' and 'debug' are handled specially)
```

Because of the eager evaluation of functions, many constant expressions will be evaluated and replaced by the result.
This does not happen with [`OptimizationLevel::Simple`] which doesn't assume all functions to be _pure_.

```rust
// When compiling the following with OptimizationLevel::Full...

let x = (1+2)*3-4/5%6;      // <- will be replaced by 'let x = 9'
let y = (1>2) || (3<=4);    // <- will be replaced by 'let y = true'
```
