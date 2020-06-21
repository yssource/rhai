Import a Module
===============

{{#include ../../links.md}}

A module can be _imported_ via the `import` statement, and its members are accessed via '`::`' similar to C++.

```rust
import "crypto" as lock;        // import the script file 'crypto.rhai' as a module named 'lock'

lock::encrypt(secret);          // use functions defined under the module via '::'

lock::hash::sha256(key);        // sub-modules are also supported

print(lock::status);            // module variables are constants

lock::status = "off";           // <- runtime error - cannot modify a constant
```

`import` statements are _scoped_, meaning that they are only accessible inside the scope that they're imported.

They can appear anywhere a normal statement can be, but in the vast majority of cases `import` statements are
group at the beginning of a script. It is, however, not advised to deviate from this common practice unless
there is a _Very Good Reason™_.

Especially, do not place an `import` statement within a loop; doing so will repeatedly re-load the same module
during every iteration of the loop!

```rust
let mod = "crypto";

if secured {                    // new block scope
    import mod as c;            // import module (the path needs not be a constant string)

    c::encrypt(key);            // use a function in the module
}                               // the module disappears at the end of the block scope

c::encrypt(others);             // <- this causes a run-time error because the 'crypto' module
                                //    is no longer available!

for x in range(0, 1000) {
    import "crypto" as c;       // <- importing a module inside a loop is a Very Bad Idea™

    c.encrypt(something);
}
```
