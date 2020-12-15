Comments
========

{{#include ../links.md}}

Comments are C-style, including '`/*` ... `*/`' pairs for block comments
and '`//`' for comments to the end of the line.

Comments can be nested.

```rust
let /* intruder comment */ name = "Bob";

// This is a very important one-line comment

/* This comment spans
   multiple lines, so it
   only makes sense that
   it is even more important */

/* Fear not, Rhai satisfies all nesting needs with nested comments:
   /*/*/*/*/**/*/*/*/*/
*/
```


Doc-Comments
------------

Similar to Rust, comments starting with `///` (three slashes) or `/**` (two asterisks) are
_doc-comments_.

Doc-comments can only appear in front of [function] definitions, not any other elements:

```rust
/// This is a valid one-line doc-comment
fn foo() {}

/** This is a
 ** valid block
 ** doc-comment
 **/
fn bar(x) {
   /// Syntax error - this doc-comment is invalid
   x + 1
}

/** Syntax error - this doc-comment is invalid */
let x = 42;

/// Syntax error - this doc-comment is also invalid
{
   let x = 42;
}
```

Doc-comments are stored within the script's [`AST`] after compilation.

The `AST::iter_functions` method provides a `ScriptFnMetadata` instance
for each function defined within the script, which includes doc-comments.

Doc-comments never affect the evaluation of a script nor do they incur
significant performance overhead.  However, third party tools can take advantage
of this information to auto-generate documentation for Rhai script functions.
