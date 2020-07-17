Track Progress and Force-Termination
===================================

{{#include ../links.md}}

It is impossible to know when, or even whether, a script run will end
(a.k.a. the [Halting Problem](http://en.wikipedia.org/wiki/Halting_problem)).

When dealing with third-party untrusted scripts that may be malicious, to track evaluation progress and
to force-terminate a script prematurely (for any reason), provide a closure to the `Engine::on_progress` method:

```rust
let mut engine = Engine::new();

engine.on_progress(|&count| {   // parameter is '&u64' - number of operations already performed
    if count % 1000 == 0 {
        println!("{}", count);  // print out a progress log every 1,000 operations
    }
    true                        // return 'true' to continue running the script
                                // return 'false' to immediately terminate the script
});
```

The closure passed to `Engine::on_progress` will be called once for every operation.
Return `false` to terminate the script immediately.


Operations Count vs. Progress Percentage
---------------------------------------

Notice that the _operations count_ value passed into the closure does not indicate the _percentage_ of work
already done by the script (and thus it is not real _progress_ tracking), because it is impossible to determine
how long a script may run.  It is possible, however, to calculate this percentage based on an estimated
total number of operations for a typical run.
