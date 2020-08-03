Capture External Variables via Automatic Currying
================================================

{{#include ../links.md}}

Poor Man's Closures
-------------------

Since [anonymous functions] de-sugar to standard function definitions, they retain all the behaviors of
Rhai functions, including being _pure_, having no access to external variables.

The anonymous function syntax, however, automatically _captures_ variables that are not defined within
the current scope, but are defined in the external scope - i.e. the scope where the anonymous function
is created.

Variables that are accessible during the time the [anonymous function] is created can be captured,
as long as they are not shadowed by local variables defined within the function's scope.
The captured variables are automatically converted into reference-counted shared values.


New Parameters For Captured Variables
------------------------------------

In actual implementation, this de-sugars to:

1. Keeping track of what variables are accessed inside the anonymous function,

2. If a variable is not defined within the anonymous function's scope, it is looked up _outside_ the function and in the current execution scope - where the anonymous function is created.

3. The variable is added to the parameters list of the anonymous function, at the front.

4. The variable is then turned into a reference-counted shared value.

5. The shared value is then [curried][currying] into the [function pointer] itself, essentially carrying a reference to that shared value and inserting it into future calls of the function.

Automatic currying can be turned off via the [`no_closure`] feature.


Examples
--------

```rust
let x = 1;

let f = |y| x + y;                  // variable 'x' is auto-curried (captured) into 'f'
                                    // 'x' is converted into a shared value

x = 40;                             // 'x' can be changed

f.call(2) == 42;                    // the value of 'x' is 40 because 'x' is shared

// The above de-sugars into this:
fn anon$1001(x, y) { x + y }        // parameter 'x' is inserted

make_shared(x);                     // convert 'x' into a shared value

let f = Fn("anon$1001").curry(x);   // shared 'x' is curried

f.call(2) == 42;
```
