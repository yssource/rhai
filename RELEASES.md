Rhai Release Notes
==================

Version 0.19.10
===============

Breaking changes
----------------

* The error variant `EvalAltResult::ErrorInFunctionCall` has a new parameter holding the _source_ of the function.
* `ParseErrorType::WrongFnDefinition` is renamed `FnWrongDefinition`.
* Redefining an existing function within the same script now throws a new `ParseErrorType::FnDuplicatedDefinition`. This is to prevent accidental overwriting an earlier function definition.

Enhancements
------------

* Source information is provided when there is an error within a call to a function defined in another module.
* Source information is provided to the `NativeCallContext` for native Rust functions.


Version 0.19.9
==============

This version fixes a bug introduced in `0.19.8` which breaks property access
within closures.

It also removes the confusing differences between _packages_ and _modules_
by unifying the terminology and API under the global umbrella of _modules_.

Bug fixes
---------

* Fix bug when accessing properties in closures.
* Fix bug when accessing a deep index with a function call.
* Fix bug that sometimes allow assigning to an invalid l-value.
* Fix off-by-one error with `Engine::set_max_call_levels`.

Breaking changes
----------------

* `Engine::load_package` is renamed `Engine::register_global_module` and now must explicitly pass a shared [`Module`].
* `Engine::register_module` is renamed `Engine::register_static_module` and now must explicitly pass a shared [`Module`].
* `Package::get` is renamed `Package::as_shared_module`.
* `Engine::set_module_resolver` now takes a straight module resolver instead of an `Option`. To disable module resolving, use the new `DummyModuleResolver`.

Enhancements
------------

* `Scope` is now `Clone + Hash`.
* `Engine::register_static_module` now supports sub-module paths (e.g. `foo::bar::baz`).
* `Engine::register_custom_operator` now accepts reserved symbols.
* `Engine::register_custom_operator` now returns an error if given a precedence of zero.
* The examples `repl` and `rhai_runner` are moved into `bin` and renamed `rhai-repl` and `rhai-run` respectively.


Version 0.19.8
==============

This version makes it easier to generate documentation for a Rhai code base.

Each function defined in an `AST` can optionally attach _doc-comments_ (which, as in Rust,
are comments prefixed by either `///` or `/**`).  Doc-comments allow third-party tools to
automatically generate documentation for functions defined in a Rhai script.

A new API, `Engine::gen_fn_metadata_to_json` and `Engine::gen_fn_metadata_with_ast_to_json`,
paired with the new `metadata` feature, exports the full list of functions metadata
(including those in an `AST`) as a JSON document.

There are also a sizable number of bug fixes.

Bug fixes
---------

* Unary prefix operators `-`, `+` and `!` now bind correctly when applied to an expression. Previously, `-x.len` is parsed as `(-x).len` which is obviously counter-intuitive.
* Indexing of namespace-qualified variables now work properly, such as `path::to::var[x]`.
* Constants are no longer propagated by the optimizer if shadowed by a non-constant variable.
* A constant passed as the `this` parameter to Rhai functions now throws an error if assigned to.
* Generic type parameter of `Engine::register_iterator` is `IntoIterator` instead of `Iterator`.
* Fixes parsing of block comments ending with `**/` or inner blocks starting with `//*`.

Breaking changes
----------------

* `Engine::on_progress` now takes `u64` instead of `&u64`.
* The closure for `Engine::on_debug` now takes two additional parameters: `source: Option<&str>` and `pos: Position`.
* `AST::iter_functions` now returns `ScriptFnMetadata`.
* The parser function passed to `Engine::register_custom_syntax_raw` now takes an additional parameter containing the _look-ahead_ symbol.

New features
------------

* `AST::iter_functions` now returns `ScriptFnMetadata` which includes, among others, _doc-comments_ for functions prefixed by `///` or `/**`.
* _Doc-comments_ can be enabled/disabled with the new `Engine::set_doc_comments` method.
* A new feature `metadata` is added that pulls in `serde_json` and enables `Engine::gen_fn_metadata_to_json` and `Engine::gen_fn_metadata_with_ast_to_json` which exports the full list of functions metadata (including those inside an `AST`) in JSON format.
* `Engine::on_debug` provides two additional parameters: `source: Option<&str>` and `pos: Position`, containing the current source (if any) and position of the `debug` statement.
* `NativeCallContext` and `EvalContext` both expose `source()` which returns the current source, if any.

Enhancements
------------

* A functions lookup cache is added to make function call resolution faster.
* Capturing a constant variable in a closure is now supported, with no cloning.
* A _look-ahead_ symbol is provided to custom syntax parsers, which can be used to parse variable-length symbol streams.


Version 0.19.7
==============

Bug fixes
---------

* Fixes compilation errors with certain feature flag combinations.

Enhancements
------------

* Property getters/setters and indexers defined in a plugin module are by default `#[rhai_fn(global)]`.
* `to_debug` is a new standard function for converting a value into debug format.
* Arrays and object maps now print values using `to_debug` (if available).


Version 0.19.6
==============

This version adds the `switch` statement.

It also allows exposing selected module functions (usually methods) to the global namespace.
This is very convenient when encapsulating the API of a custom Rust type into a module while having methods
and iterators registered on the custom type work normally.

A new `gen_fn_signatures` API enables enumerating the registered functions of an `Engine` for documentation purposes.
It also prepares the way for a future reflection API.

Bug fixes
---------

* Custom syntax that introduces a shadowing variable now works properly.

Breaking changes
----------------

* `Module::set_fn`, `Module::set_raw_fn` and `Module::set_fn_XXX_mut` all take an additional parameter of `FnNamespace`.
* `Module::set_fn` takes a further parameter with a list of parameter names/types plus the function return type, if any.
* `Module::get_sub_module_mut` is removed.
* `begin`, `end`, `unless` are now reserved keywords.
* `EvalPackage` is removed in favor of `Engine::disable_symbol`.

New features
------------

* New `switch` statement.
* New `do ... while` and `do ... until` statements.
* New `Engine::gen_fn_signatures`, `Module::gen_fn_signatures` and `PackagesCollection::gen_fn_signatures` to generate a list of signatures for functions registered.
* New `Engine::register_static_module` to register a module as a sub-module in the global namespace.
* New `set_exported_global_fn!` macro to register a plugin function and expose it to the global namespace.
* `Module::set_fn_XXX_mut` can expose a module function to the global namespace. This is convenient when registering an API for a custom type.
* `Module::set_getter_fn`, `Module::set_setter_fn`, `Module::set_indexer_get_fn`, `Module::set_indexer_set_fn` all expose the function to the global namespace by default. This is convenient when registering an API for a custom type.
* New `Module::update_fn_metadata` to update a module function's parameter names and types.
* New `#[rhai_fn(global)]` and `#[rhai_fn(internal)]` attributes to determine whether a function defined in a plugin module should be exposed to the global namespace. This is convenient when defining an API for a custom type.
* New `get_fn_metadata_list` to get the metadata of all script-defined functions in scope.

Enhancements
------------

* New constants under `Dynamic` including `UNIT`, `TRUE`, `FALSE`, `ZERO`, `ONE` etc.
* Floating-point numbers ending with a decimal point without a trailing `0` are supported.


Version 0.19.5
==============

This version fixes a bug that prevents compilation with the `internals` feature.
It also speeds up importing modules.

Bug fixes
---------

* Fixes compilation error when using the `internals` feature.  Bug introduced in `0.19.4`.
* Importing script files recursively no longer panics.

Breaking changes
----------------

* Modules imported at global level can now be accessed in functions.
* `ModuleResolver::resolve` now returns `Shared<Module>` for better resources sharing when loading modules.
* `ParseErrorType::DuplicatedExport` is removed as multiple `export`'s are now allowed.

Enhancements
------------

* Modules imported via `import` statements at global level can now be used in functions. There is no longer any need to re-`import` the modules at the beginning of each function block.
* Modules imported via `import` statements are encapsulated into the `AST` when loading a module from a script file.
* `export` keyword can now be tagged onto `let` and `const` statements as a short-hand, e.g.: `export let x = 42;`
* Variables can now be `export`-ed multiple times under different names.
* `index_of`, `==` and `!=` are defined for arrays.
* `==` and `!=` are defined for object maps.


Version 0.19.4
==============

This version basically cleans up the code structure in preparation for a potential `1.0` release in the future.
Most scripts should see a material speed increase.

This version also adds a low-level API for more flexibility when defining custom syntax.

Bug fixes
---------

* Fixes `Send + Sync` for `EvalAltResult` under the `sync` feature. Bug introduced with `0.19.3`.

Breaking changes
----------------

* Custom syntax can no longer start with a keyword (even a _reserved_ one), even if it has been disabled. That is to avoid breaking scripts later when the keyword is no longer disabled.

Changes to Error Handling
------------------------

* `EvalAltResult::ErrorAssignmentToUnknownLHS` is moved to `ParseError::AssignmentToInvalidLHS`. `ParseError::AssignmentToCopy` is removed.
* `EvalAltResult::ErrorDataTooLarge` is simplified.
* `Engine::on_progress` closure signature now returns `Option<Dynamic>` with the termination value passed on to `EvalAltResult::ErrorTerminated`.
* `ParseErrorType::BadInput` now wraps a `LexError` instead of a text string.

New features
------------

* `f32_float` feature to set `FLOAT` to `f32`.
* Low-level API for custom syntax allowing more flexibility in designing the syntax.
* `Module::fill_with` to poly-fill a module with another.
* Scripts terminated via `Engine::on_progress` can now pass on a value as a termination token.

Enhancements
------------

* Essential AST structures like `Expr` and `Stmt` are packed into smaller sizes (16 bytes and 32 bytes on 64-bit), stored inline for more cache friendliness, and de-`Box`ed as much as possible.
* `Scope` is optimized for cache friendliness.


Version 0.19.3
==============

This version streamlines some of the advanced API's, and adds the `try` ... `catch` statement
to catch exceptions.

Breaking changes
----------------

* `EvalAltResult::ErrorReadingScriptFile` is removed in favor of the new `EvalAltResult::ErrorSystem`.
* `EvalAltResult::ErrorLoopBreak` is renamed to `EvalAltResult::LoopBreak`.
* `Engine::register_raw_fn` and `FnPtr::call_dynamic` function signatures have changed.
* Callback signatures to `Engine::on_var` and `Engine::register_custom_syntax` have changed.
* `EvalAltResult::ErrorRuntime` now wraps a `Dynamic` instead of a string.
* Default call stack depth for `debug` builds is reduced to 8 (from 12) because it keeps overflowing the stack in GitHub CI!
* Keyword `thread` is reserved.

New features
------------

* The plugins system is enhanced to support functions taking a `NativeCallContext` as the first parameter.
* `throw` statement can now throw any value instead of just text strings.
* New `try` ... `catch` statement to catch exceptions.

Enhancements
------------

* Calling `eval` or `Fn` in method-call style, which is an error, is now caught during parsing.
* `func!()` call style is valid even under `no_closure` feature.


Version 0.19.2
==============

Bug fix on call module functions.


Version 0.19.1
==============

This version adds a variable resolver with the ability to short-circuit variable access,
plus a whole bunch of array methods.

Breaking changes
----------------

* `AST::iter_functions` now returns an iterator instead of taking a closure.
* `Module::get_script_function_by_signature` renamed to `Module::get_script_fn` and returns `&<Shared<ScriptFnDef>>`.
* `Module::num_fn`, `Module::num_var` and `Module::num_iter` are removed and merged into `Module::count`.
* The `merge_namespaces` parameter to `Module::eval_ast_as_new` is removed and now defaults to `true`.
* `GlobalFileModuleResolver` is removed because its performance gain over the `FileModuleResolver` is no longer very significant.
* The following `EvalAltResult` variants are removed and merged into `EvalAltResult::ErrorMismatchDataType`: `ErrorCharMismatch`, `ErrorNumericIndexExpr`, `ErrorStringIndexExpr`, `ErrorImportExpr`, `ErrorLogicGuard`, `ErrorBooleanArgMismatch`
* `Scope::iter_raw` returns an iterator with an additional field indicating whether the variable is constant or not.
* `rhai::ser` and `rhai::de` namespaces are merged into `rhai::serde`.
* New reserved symbols: `++`, `--`, `..`, `...`.
* Callback signature for custom syntax implementation function is changed to allow for more flexibility.
* Default call stack depth for `debug` builds is reduced to 12 (from 16).
* Precedence for `~` is raised, while `in` is moved below logic comparison operators.

New features
------------

* New `Engine::on_var` to register a _variable resolver_.
* `const` statements can now take any expression (or none at all) instead of only constant values.
* `OptimizationLevel::Simple` now eagerly evaluates built-in binary operators of primary types (if not overloaded).
* `is_def_var()` to detect if variable is defined, and `is_def_fn()` to detect if script function is defined.
* `Dynamic::from(&str)` now constructs a `Dynamic` with a copy of the string as value.
* `AST::combine` and `AST::combine_filtered` allows combining two `AST`'s without creating a new one.
* `map`, `filter`, `reduce`, `reduce_rev`, `some`, `all`, `extract`, `splice`, `chop` and `sort` functions for arrays.
* New `Module::set_iterable` and `Module::set_iterator` to define type iterators more easily. `Engine::register_iterator` is changed to use the simpler version.

Enhancements
------------

* Many one-liners and few-liners are now marked `#[inline]` or `[inline(always)]`, just in case it helps when LTO is not turned on.


Version 0.19.0
==============

The major new feature for this version is _Plugins_ support, powered by procedural macros.
Plugins make it extremely easy to develop and register Rust functions with an `Engine`.

Bug fixes
---------

* `if` statement with an empty `true` block would not evaluate the `false` block.  This is now fixed.
* Fixes a bug in `Module::set_fn_4_mut`.
* Module API's now properly handle `&str` and `String` parameters.
* Indexers are available under `no_object`.
* Registered operator-assignment functions (e.g. `+=`) now work correctly.

Breaking changes
----------------

* `Engine::register_set_result` and `Engine::register_indexer_set_result` now take a function that returns `Result<(), Box<EvalAltResult>>`.
* `Engine::register_indexer_XXX` and `Module::set_indexer_XXX` panic when the type is `Array`, `Map` or `String`.
* `EvalAltResult` has a new variant `ErrorInModule` which holds errors when loading an external module.
* `Module::eval_ast_as_new` now takes an extra boolean parameter, indicating whether to encapsulate the entire module into a separate namespace.
* Functions in `FileModuleResolver` loaded modules now can cross-call each other in addition to functions in the global namespace. For the old behavior, use `MergingFileModuleResolver` instead.
* New `EvalAltResult::ErrorInModule` variant capturing errors when loading a module from a script file.

New features
------------

* Plugins support via procedural macros.
* Scripted functions are allowed in packages.
* `parse_int` and `parse_float` functions for parsing numbers; `split` function for splitting strings.
* `AST::iter_functions` and `Module::iter_script_fn_info` to iterate functions.
* Functions iteration functions for `AST` and `Module` now take `FnMut` instead of `Fn`.
* New `FileModuleResolver` that encapsulates the entire `AST` of the module script, allowing function cross-calling. The old version is renamed `MergingFileModuleResolver`.
* `+` and `-` operators for timestamps to increment/decrement by seconds.


Version 0.18.3
==============

Bug fixes
---------

* `Engine::compile_expression`, `Engine::eval_expression` etc. no longer parse anonymous functions and closures.
* Imported modules now work inside closures.
* Closures that capture now work under `no_object`.

New features
------------

* Adds `Module::combine_flatten` to combine two modules while flattening to the root level.


Version 0.18.2
==============

Bug fixes
---------

* Fixes bug that prevents calling functions in closures.
* Fixes bug that erroneously consumes the first argument to a namespace-qualified function call.

New features
------------

* Adds `Engine::register_get_result`, `Engine::register_set_result`, `Engine::register_indexer_get_result`, `Engine::register_indexer_set_result` API.
* Adds `Module::combine` to combine two modules.
* `Engine::parse_json` now also accepts a JSON object starting with `#{`.


Version 0.18.1
==============

This version adds:

* Anonymous functions (in Rust closure syntax).  Simplifies creation of single-use ad-hoc functions.
* Currying of function pointers.
* Closures - auto-currying of anonymous functions to capture shared variables from the external scope. Use the `no_closure` feature to disable sharing values and capturing.
* Binding the `this` pointer in a function pointer `call`.
* Capturing call scope via `func!(...)` syntax.

New features
------------

* `call` can now be called function-call style for function pointers - this is to handle builds with `no_object`.
* Reserve language keywords, such as `print`, `eval`, `call`, `this` etc.
* `x.call(f, ...)` allows binding `x` to `this` for the function referenced by the function pointer `f`.
* Anonymous functions are supported in the syntax of a Rust closure, e.g. `|x, y, z| x + y - z`.
* Custom syntax now works even without the `internals` feature.
* Currying of function pointers is supported via the new `curry` keyword.
* Automatic currying of anonymous functions to capture shared variables from the external scope.
* Capturing of the calling scope for function call via the `func!(...)` syntax.
* `Module::set_indexer_get_set_fn` is added as a short-hand of both `Module::set_indexer_get_fn` and `Module::set_indexer_set_fn`.
* New `unicode-xid-ident` feature to allow [Unicode Standard Annex #31](http://www.unicode.org/reports/tr31/) for identifiers.
* `Scope::iter_raw` returns an iterator with a reference to the underlying `Dynamic` value (which may be shared).

Breaking changes
----------------

* Language keywords are now _reserved_ (even when disabled) and they can no longer be used as variable names.
* Function signature for defining custom syntax is simplified.
* `Engine::register_raw_fn_XXX` API shortcuts are removed.
* `PackagesCollection::get_fn`, `PackagesCollection::contains_fn`, `Module::get_fn` and `Module::contains_fn` now take an additional `public_only` parameter indicating whether only public functions are accepted.
* The iterator returned by `Scope::iter` now contains a clone of the `Dynamic` value (unshared).
* `Engine::register_global_module` takes any type that is `Into<PackageLibrary>`.
* Error in `Engine::register_custom_syntax` is no longer `Box`-ed.

Housekeeping
------------

* Most compilation warnings are eliminated via feature gates.


Version 0.17.0
==============

This version adds:

* [`serde`](https://crates.io/crates/serde) support for working with `Dynamic` values (particularly _object maps_).
* Low-level API to register functions.
* Surgically disable keywords and/or operators in the language.
* Define custom operators.
* Extend the language via custom syntax.

Bug fixes
---------

* Fixed method calls in the middle of a dot chain.

Breaking changes
----------------

* `EvalAltResult::ErrorMismatchOutputType` has an extra argument containing the name of the requested type.
* `Engine::call_fn_dynamic` take an extra argument, allowing a `Dynamic` value to be bound to the `this` pointer.
* Precedence of the `%` (modulo) operator is lowered to below `<<` ad `>>`. This is to handle the case of `x << 3 % 10`.

New features
------------

* New `serde` feature to allow serializing/deserializing to/from `Dynamic` values using [`serde`](https://crates.io/crates/serde).
  This is particularly useful when converting a Rust `struct` to a `Dynamic` _object map_ and back.
* `Engine::disable_symbol` to surgically disable keywords and/or operators.
* `Engine::register_custom_operator` to define a custom operator.
* `Engine::register_custom_syntax` to define a custom syntax.
* New low-level API `Engine::register_raw_fn`.
* New low-level API `Module::set_raw_fn` mirroring `Engine::register_raw_fn`.
* `AST::clone_functions_only`, `AST::clone_functions_only_filtered` and `AST::clone_statements_only` to clone only part of an `AST`.
* The boolean `^` (XOR) operator is added.
* `FnPtr` is exposed as the function pointer type.
* `rhai::module_resolvers::ModuleResolversCollection` added to try a list of module resolvers.
* It is now possible to mutate the first argument of a namespace-qualified function call when the argument is a simple variable (but not a module constant).
* Many configuration/setting API's now returns `&mut Self` so that the calls can be chained.
* `String` parameters in functions are supported (but inefficiently).


Version 0.16.1
==============

Bug fix release to fix errors when compiling with features.


Version 0.16.0
==============

The major new feature in this version is OOP - well, poor man's OOP, that is.

The `README` is officially transferred to [The Rhai Book](https://rhaiscript.github.io/book).

An online [Playground](https://alvinhochun.github.io/rhai-demo/) is available.

Breaking changes
----------------

* The trait function `ModuleResolver::resolve` no longer takes a `Scope` as argument.
* Functions defined in script now differentiates between using method-call style and normal function-call style.
  The method-call style will bind the object to the `this` parameter instead of consuming the first parameter.
* Imported modules are no longer stored in the `Scope`.  `Scope::push_module` is removed.
  Therefore, cannot rely on module imports to persist across invocations using a `Scope`.
* `AST::retain_functions` is used for another purpose. The old `AST::retain_functions` is renamed to `AST::clear_statements`.

New features
------------

* Support for _function pointers_ via `Fn(name)` and `Fn.call(...)` syntax - a poor man's first-class function.
* Support for calling script-defined functions in method-call style with `this` binding to the object.
* Special support in object maps for OOP.
* Expanded the `AST` API for fine-tuned manipulation of functions.

Enhancements
------------

* [The Rhai Book](https://rhaiscript.github.io/book) is online.  Most content in the original `README` was transferred to the Book.
* New feature `internals` to expose internal data structures (e.g. the AST nodes).


Version 0.15.1
==============

This is a minor release which enables updating indexers (via registered indexer setters) and supports functions
with `&str` parameters (maps transparently to `ImmutableString`). WASM is also a tested target.

Bug fix
-------

* `let s="abc"; s[1].change_to('X');` now correctly sets the character '`X`' into '`s`' yielding `"aXc"`.

Breaking changes
----------------

* Callback closure passed to `Engine::on_progress` now takes `&u64` instead of `u64` to be consistent with other callback signatures.
* `Engine::register_indexer` is renamed to `Engine::register_indexer_get`.
* `Module::set_indexer_fn` is renamed to `Module::set_indexer_get_fn`.
* The tuple `ParseError` now exposes the internal fields and the `ParseError::error_type` and `ParseError::position` methods are removed.  The first tuple field is the `ParseErrorType` and the second tuple field is the `Position`.
* `Engine::call_fn_dynamic` now takes any type that implements `IntoIterator<Item = Dynamic>`.

New features
------------

* Indexers are now split into getters and setters (which now support updates).  The API is split into `Engine::register_indexer_get` and `Engine::register_indexer_set` with `Engine::register_indexer_get_set` being a short-hand.  Similarly, `Module::set_indexer_get_fn` and `Module::set_indexer_set_fn` are added.
* `Engine:register_fn` and `Engine:register_result_fn` accepts functions that take parameters of type `&str` (immutable string slice), which maps directly to `ImmutableString`. This is to avoid needing wrappers for functions taking string parameters.
* Set maximum limit on data sizes: `Engine::set_max_string_size`, `Engine::set_max_array_size` and `Engine::set_max_map_size`.
* Supports trailing commas on array literals, object map literals, function definitions and function calls.
* Enhances support for compiling to WASM.


Version 0.15.0
==============

This version uses immutable strings (`ImmutableString` type) and built-in operator functions (e.g. `+`, `>`, `+=`) to improve speed, plus some bug fixes.

Regression fix
--------------

* Do not optimize script with `eval_expression` - it is assumed to be one-off and short.

Bug fixes
---------

* Indexing with an index or dot expression now works property (it compiled wrongly before).
  For example, `let s = "hello"; s[s.len-1] = 'x';` now works property instead of causing a runtime error.
* `if` expressions are not supposed to be allowed when compiling for expressions only. This is fixed.

Breaking changes
----------------

* `Engine::compile_XXX` functions now return `ParseError` instead of `Box<ParseError>`.
* The `RegisterDynamicFn` trait is merged into the `RegisterResultFn` trait which now always returns `Result<Dynamic, Box<EvalAltResult>>`.
* Default maximum limit on levels of nested function calls is fine-tuned and set to a different value.
* Some operator functions are now built in (see _Speed enhancements_ below), so they are available even under `Engine::new_raw`.
* Strings are now immutable. The type `rhai::ImmutableString` is used instead of `std::string::String`. This is to avoid excessive cloning of strings.  All native-Rust functions taking string parameters should switch to `rhai::ImmutableString` (which is either `Rc<String>` or `Arc<String>` depending on whether the `sync` feature is used).
* Native Rust functions registered with the `Engine` also mutates the first argument when called in normal function-call style (previously the first argument will be passed by _value_ if not called in method-call style).  Of course, if the first argument is a calculated value (e.g. result of an expression), then mutating it has no effect, but at least it is not cloned.
* Some built-in methods (e.g. `len` for string, `floor` for `FLOAT`) now have _property_ versions in addition to methods to simplify coding.

New features
------------

* Set limit on maximum level of nesting expressions and statements to avoid panics during parsing.
* New `EvalPackage` to disable `eval`.
* `Module::set_getter_fn`, `Module::set_setter_fn` and `Module:set_indexer_fn` to register getter/setter/indexer functions.
* `Engine::call_fn_dynamic` for more control in calling script functions.

Speed enhancements
------------------

* Common operators (e.g. `+`, `>`, `==`) now call into highly efficient built-in implementations for standard types (i.e. `INT`, `FLOAT`, `bool`, `char`, `()` and `ImmutableString`) if not overridden by a registered function. This yields a 5-10% speed benefit depending on script operator usage. Scripts running tight loops will see significant speed-up.
* Common assignment operators (e.g. `+=`, `%=`) now call into highly efficient built-in implementations for standard types (i.e. `INT`, `FLOAT`, `bool`, `char`, `()` and `ImmutableString`) if not overridden by a registered function.
* Implementations of common operators for standard types are removed from the `ArithmeticPackage` and `LogicPackage` (and therefore the `CorePackage`) because they are now always available, even under `Engine::new_raw`.
* Operator-assignment statements (e.g. `+=`) are now handled directly and much faster.
* Strings are now _immutable_ and use the `rhai::ImmutableString` type, eliminating large amounts of cloning.
* For Native Rust functions taking a first `&mut` parameter, the first argument is passed by reference instead of by value, even if not called in method-call style.  This allows many functions declared with `&mut` parameter to avoid excessive cloning. For example, if `a` is a large array, getting its length in this manner: `len(a)` used to result in a full clone of `a` before taking the length and throwing the copy away. Now, `a` is simply passed by reference, avoiding the cloning altogether.
* A custom hasher simply passes through `u64` keys without hashing to avoid function call hash keys (which are by themselves `u64`) being hashed twice.


Version 0.14.1
==============

The major features for this release is modules, script resource limits, and speed improvements
(mainly due to avoiding allocations).

New features
------------

* Modules and _module resolvers_ allow loading external scripts under a module namespace. A module can contain constant variables, Rust functions and Rhai functions.
* `export` variables and `private` functions.
* _Indexers_ for Rust types.
* Track script evaluation progress and terminate script run.
* Set limit on maximum number of operations allowed per script run.
* Set limit on maximum number of modules loaded per script run.
* A new API, `Engine::compile_scripts_with_scope`, can compile a list of script segments without needing to first concatenate them together into one large string.
* Stepped `range` function with a custom step.

Speed improvements
------------------

### `StaticVec`

A script contains many lists - statements in a block, arguments to a function call etc.
In a typical script, most of these lists tend to be short - e.g. the vast majority of function calls contain
fewer than 4 arguments, while most statement blocks have fewer than 4-5 statements, with one or two being
the most common. Before, dynamic `Vec`'s are used to hold these short lists for very brief periods of time,
causing allocations churn.

In this version, large amounts of allocations are avoided by converting to a `StaticVec` -
a list type based on a static array for a small number of items (currently four) -
wherever possible plus other tricks. Most real-life scripts should see material speed increases.

### Pre-computed variable lookups

Almost all variable lookups, as well as lookups in loaded modules, are now pre-computed.
A variable's name is almost never used to search for the variable in the current scope.

_Getters_ and _setter_ function names are also pre-computed and cached, so no string allocations are
performed during a property get/set call.

### Pre-computed function call hashes

Lookup of all function calls, including Rust and Rhai ones, are now through pre-computed hashes.
The function name is no longer used to search for a function, making function call dispatches
much faster.

### Large Boxes for expressions and statements

The expression (`Expr`) and statement (`Stmt`) types are modified so that all of the variants contain only
one single `Box` to a large allocated structure containing _all_ the fields.  This makes the `Expr` and
`Stmt` types very small (only one single pointer) and improves evaluation speed due to cache efficiency.

Error handling
--------------

Previously, when an error occurs inside a function call, the error position reported is the function
call site. This makes it difficult to diagnose the actual location of the error within the function.

A new error variant `EvalAltResult::ErrorInFunctionCall` is added in this version.
It wraps the internal error returned by the called function, including the error position within the function.
