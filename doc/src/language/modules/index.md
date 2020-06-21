Modules
=======

{{#include ../../links.md}}

Rhai allows organizing code (functions, both Rust-based or script-based, and variables) into _modules_.
Modules can be disabled via the [`no_module`] feature.

A module is of the type `Module` and encapsulates a Rhai script together with the functions defined
by that script.

The script text is run, variables are then selectively exposed via the [`export`] statement.
Functions defined by the script are automatically exported.

Modules loaded within this module at the global level become _sub-modules_ and are also automatically exported.

Other scripts can then load this module and use the variables and functions exported
as if they were defined inside the same script.
