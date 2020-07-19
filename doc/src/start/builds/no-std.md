`no-std` Build
=============

{{#include ../../links.md}}

The feature [`no_std`] automatically converts the scripting engine into a `no-std` build.

Usually, a `no-std` build goes hand-in-hand with [minimal builds] because typical embedded
hardware (the primary target for `no-std`) has limited storage.


Nightly Required
----------------

Currently, [`no_std`] requires the nightly compiler due to the crates that it uses.
