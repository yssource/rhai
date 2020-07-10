Use Rhai as a Domain-Specific Language (DSL)
===========================================

{{#include ../links.md}}

Rhai can be successfully used as a domain-specific language (DSL).


Expressions Only
----------------

In many DSL scenarios, only evaluation of expressions is needed.

The `Engine::eval_expression_XXX`[`eval_expression`] API can be used to restrict
a script to expressions only.


Disable Keywords and/or Operators
--------------------------------

In some DSL scenarios, it is necessary to further restrict the language to exclude certain
language features that are not necessary or dangerous to the application.

For example, a DSL may disable the `while` loop altogether while keeping all other statement
types intact.

It is possible, in Rhai, to surgically [disable keywords and operators].


Custom Operators
----------------

On the other hand, some DSL scenarios require special operators that make sense only for
that specific environment.  In such cases, it is possible to define [custom operators] in Rhai.

For example:

```rust
let animal = "rabbit";
let food = "carrot";

animal eats food            // custom operator - 'eats'

eats(animal, food)          // <- the above really de-sugars to this
```

Although a [custom operator] always de-sugars to a simple function call,
nevertheless it makes the DSL syntax much simpler and expressive.


Custom Syntax
-------------

For advanced DSL scenarios, it is possible to define entire expression [_syntax_][custom syntax] -
essentially custom statement types.

The [`internals`] feature is needed to be able to define [custom syntax] in Rhai.

For example, the following is a SQL like syntax for some obscure DSL operation:

```rust
let table = [..., ..., ..., ...];

// Syntax = "calculate" $ident$ $ident$ "from" $expr$ "->" $ident$ ":" $expr$
let total = calculate sum price from table -> row : row.weight > 50;

// Note: There is nothing special about the use of symbols; to make it look exactly like SQL:
// Syntax = "SELECT" $ident$ "(" $ident$ ")" "FROM" $expr$ "AS" $ident$ "WHERE" $expr$
let total = SELECT sum(price) FROM table AS row WHERE row.weight > 50;
```

After registering this custom syntax with Rhai, it can be used anywhere inside a script as
a normal expression.

For its evaluation, the callback function will receive the following list of parameters:

`exprs[0] = "sum"` - math operator  
`exprs[1] = "price"` - field name  
`exprs[2] = Expr(table)` - data source  
`exprs[3] = "row"` - loop variable name  
`exprs[4] = Expr(row.wright > 50)` - expression  

The other identified, such as `"select"`, `"from"`, as as as symbols `->` and `:` are
parsed in the order defined within the custom syntax.
