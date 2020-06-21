Call Method as Function
======================

{{#include ../links.md}}

Properties and methods in a Rust custom type registered with the [`Engine`] can be called just like a regular function in Rust.

Unlike functions defined in script (for which all arguments are passed by _value_),
native Rust functions may mutate the object (or the first argument if called in normal function call style).

Custom types, properties and methods can be disabled via the [`no_object`] feature.

```rust
let a = new_ts();   // constructor function
a.field = 500;      // property setter
a.update();         // method call, 'a' can be modified

update(a);          // <- this de-sugars to 'a.update()' thus if 'a' is a simple variable
                    //    unlike scripted functions, 'a' can be modified and is not a copy

let array = [ a ];

update(array[0]);   // <- 'array[0]' is an expression returning a calculated value,
                    //    a transient (i.e. a copy) so this statement has no effect
                    //    except waste a lot of time cloning

array[0].update();  // <- call this method-call style will update 'a'
```
