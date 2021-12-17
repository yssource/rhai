//! Module defining the AST (abstract syntax tree).

pub mod ast;
pub mod expr;
pub mod flags;
pub mod ident;
pub mod script_fn;
pub mod stmt;

pub use ast::{ASTNode, AST};
pub use expr::{BinaryExpr, CustomExpr, Expr, FloatWrapper, FnCallExpr, FnCallHashes};
pub use flags::{FnAccess, OptionFlags, AST_OPTION_FLAGS};
pub use ident::Ident;
#[cfg(not(feature = "no_function"))]
pub use script_fn::{ScriptFnDef, ScriptFnMetadata};
pub use stmt::{OpAssignment, Stmt, StmtBlock};

#[cfg(feature = "no_function")]
pub struct ScriptFnDef;
