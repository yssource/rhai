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
The values captured are the values of those variables at the time of the [anonymous function]'s creation.


New Parameters For Captured Variables
------------------------------------

In actual implementation, this de-sugars to:

1. Keeping track of what variables are accessed inside the anonymous function,

2. If a variable is not defined within the anonymous function's scope, it is looked up _outside_ the function and in the current execution scope - where the anonymous function is created.

3. The variable is added to the parameters list of the anonymous function, at the front.

4. The current value of the variable is then [curried][currying] into the [function pointer] itself, essentially carrying that value and inserting it into future calls of the function.

Automatic currying can be turned off via the [`no_capture`] feature.


Examples
--------

```rust
let x = 40;

let f = |y| x + y;                  // current value of variable 'x' is auto-curried
                                    // the value 40 is curried into 'f'

x = 1;                              // 'x' can be changed but the curried value is not

f.call(2) == 42;                    // the value of 'x' is still 40

// The above de-sugars into this:
fn anon$1001(x, y) { x + y }        // parameter 'x' is inserted

let f = Fn("anon$1001").curry(x);   // current value of 'x' is curried

f.call(2) == 42;
```
