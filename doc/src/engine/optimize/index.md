Script Optimization
===================

{{#include ../../links.md}}

Rhai includes an _optimizer_ that tries to optimize a script after parsing.
This can reduce resource utilization and increase execution speed.

Script optimization can be turned off via the [`no_optimize`] feature.


Dead Code Removal
----------------

For example, in the following:

```rust
{
    let x = 999;            // NOT eliminated: variable may be used later on (perhaps even an 'eval')
    123;                    // eliminated: no effect
    "hello";                // eliminated: no effect
    [1, 2, x, x*2, 5];      // eliminated: no effect
    foo(42);                // NOT eliminated: the function 'foo' may have side-effects
    666                     // NOT eliminated: this is the return value of the block,
                            // and the block is the last one so this is the return value of the whole script
}
```

Rhai attempts to eliminate _dead code_ (i.e. code that does nothing, for example an expression by itself as a statement,
which is allowed in Rhai).

The above script optimizes to:

```rust
{
    let x = 999;
    foo(42);
    666
}
```


Constants Propagation
--------------------

Constants propagation is used to remove dead code:

```rust
const ABC = true;
if ABC || some_work() { print("done!"); }   // 'ABC' is constant so it is replaced by 'true'...
if true || some_work() { print("done!"); }  // since '||' short-circuits, 'some_work' is never called
if true { print("done!"); }                 // <- the line above is equivalent to this
print("done!");                             // <- the line above is further simplified to this
                                            //    because the condition is always true
```

These are quite effective for template-based machine-generated scripts where certain constant values
are spliced into the script text in order to turn on/off certain sections.

For fixed script texts, the constant values can be provided in a user-defined [`Scope`] object
to the [`Engine`] for use in compilation and evaluation.


Watch Out for Function Calls
---------------------------

Beware, however, that most operators are actually function calls, and those functions can be overridden,
so they are not optimized away:

```rust
const DECISION = 1;

if DECISION == 1 {          // NOT optimized away because you can define
    :                       // your own '==' function to override the built-in default!
    :
} else if DECISION == 2 {   // same here, NOT optimized away
    :
} else if DECISION == 3 {   // same here, NOT optimized away
    :
} else {
    :
}
```

because no operator functions will be run (in order not to trigger side-effects) during the optimization process
(unless the optimization level is set to [`OptimizationLevel::Full`]).

So, instead, do this:

```rust
const DECISION_1 = true;
const DECISION_2 = false;
const DECISION_3 = false;

if DECISION_1 {
    :                       // this branch is kept and promoted to the parent level
} else if DECISION_2 {
    :                       // this branch is eliminated
} else if DECISION_3 {
    :                       // this branch is eliminated
} else {
    :                       // this branch is eliminated
}
```

In general, boolean constants are most effective for the optimizer to automatically prune
large `if`-`else` branches because they do not depend on operators.

Alternatively, turn the optimizer to [`OptimizationLevel::Full`].
