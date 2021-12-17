//! Module defining script statements.

use super::{ASTNode, Expr, FnCallExpr, Ident, OptionFlags, AST_OPTION_FLAGS};
use crate::tokenizer::Token;
use crate::{calc_fn_hash, Position, StaticVec, INT};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    collections::BTreeMap,
    fmt,
    hash::Hash,
    mem,
    ops::{Deref, DerefMut},
};

/// _(internals)_ An op-assignment operator.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct OpAssignment<'a> {
    /// Hash of the op-assignment call.
    pub hash_op_assign: u64,
    /// Hash of the underlying operator call (for fallback).
    pub hash_op: u64,
    /// Op-assignment operator.
    pub op: &'a str,
}

impl OpAssignment<'_> {
    /// Create a new [`OpAssignment`].
    ///
    /// # Panics
    ///
    /// Panics if the name is not an op-assignment operator.
    #[must_use]
    #[inline(always)]
    pub fn new(name: &str) -> Self {
        Self::new_from_token(Token::lookup_from_syntax(name).expect("operator"))
    }
    /// Create a new [`OpAssignment`] from a [`Token`].
    ///
    /// # Panics
    ///
    /// Panics if the token is not an op-assignment operator.
    #[must_use]
    pub fn new_from_token(op: Token) -> Self {
        let op_raw = op
            .map_op_assignment()
            .expect("op-assignment operator")
            .literal_syntax();
        Self {
            hash_op_assign: calc_fn_hash(op.literal_syntax(), 2),
            hash_op: calc_fn_hash(op_raw, 2),
            op: op.literal_syntax(),
        }
    }
}

/// _(internals)_ A scoped block of statements.
/// Exported under the `internals` feature only.
#[derive(Clone, Hash, Default)]
pub struct StmtBlock(StaticVec<Stmt>, Position);

impl StmtBlock {
    /// A [`StmtBlock`] that does not exist.
    pub const NONE: Self = Self::empty(Position::NONE);

    /// Create a new [`StmtBlock`].
    #[must_use]
    pub fn new(statements: impl IntoIterator<Item = Stmt>, pos: Position) -> Self {
        let mut statements: StaticVec<_> = statements.into_iter().collect();
        statements.shrink_to_fit();
        Self(statements, pos)
    }
    /// Create an empty [`StmtBlock`].
    #[inline(always)]
    #[must_use]
    pub const fn empty(pos: Position) -> Self {
        Self(StaticVec::new_const(), pos)
    }
    /// Is this statements block empty?
    #[inline(always)]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// Number of statements in this statements block.
    #[inline(always)]
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    /// Get the statements of this statements block.
    #[inline(always)]
    #[must_use]
    pub fn statements(&self) -> &[Stmt] {
        &self.0
    }
    /// Extract the statements.
    #[inline(always)]
    #[must_use]
    pub(crate) fn take_statements(&mut self) -> StaticVec<Stmt> {
        mem::take(&mut self.0)
    }
    /// Get an iterator over the statements of this statements block.
    #[inline(always)]
    #[must_use]
    pub fn iter(&self) -> impl Iterator<Item = &Stmt> {
        self.0.iter()
    }
    /// Get the position (location of the beginning `{`) of this statements block.
    #[inline(always)]
    #[must_use]
    pub const fn position(&self) -> Position {
        self.1
    }
    /// Set the position (location of the beginning `{`) of this statements block.
    #[inline(always)]
    pub fn set_position(&mut self, pos: Position) {
        self.1 = pos;
    }
}

impl Deref for StmtBlock {
    type Target = StaticVec<Stmt>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for StmtBlock {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Debug for StmtBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Block")?;
        fmt::Debug::fmt(&self.0, f)?;
        self.1.debug_print(f)
    }
}

impl From<StmtBlock> for Stmt {
    #[inline(always)]
    fn from(block: StmtBlock) -> Self {
        Self::Block(block.0.into_boxed_slice(), block.1)
    }
}

impl IntoIterator for StmtBlock {
    type Item = Stmt;
    type IntoIter = smallvec::IntoIter<[Stmt; 3]>;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Extend<Stmt> for StmtBlock {
    #[inline(always)]
    fn extend<T: IntoIterator<Item = Stmt>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

/// _(internals)_ A statement.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone, Hash)]
pub enum Stmt {
    /// No-op.
    Noop(Position),
    /// `if` expr `{` stmt `}` `else` `{` stmt `}`
    If(Expr, Box<(StmtBlock, StmtBlock)>, Position),
    /// `switch` expr `if` condition `{` literal or _ `=>` stmt `,` ... `}`
    Switch(
        Expr,
        Box<(
            BTreeMap<u64, Box<(Option<Expr>, StmtBlock)>>,
            StmtBlock,
            StaticVec<(INT, INT, bool, Option<Expr>, StmtBlock)>,
        )>,
        Position,
    ),
    /// `while` expr `{` stmt `}` | `loop` `{` stmt `}`
    ///
    /// If the guard expression is [`UNIT`][Expr::Unit], then it is a `loop` statement.
    While(Expr, Box<StmtBlock>, Position),
    /// `do` `{` stmt `}` `while`|`until` expr
    ///
    /// ### Option Flags
    ///
    /// * [`AST_OPTION_NONE`][AST_OPTION_FLAGS::AST_OPTION_NONE] = `while`
    /// * [`AST_OPTION_NEGATED`][AST_OPTION_FLAGS::AST_OPTION_NEGATED] = `until`
    Do(Box<StmtBlock>, Expr, OptionFlags, Position),
    /// `for` `(` id `,` counter `)` `in` expr `{` stmt `}`
    For(Expr, Box<(Ident, Option<Ident>, StmtBlock)>, Position),
    /// \[`export`\] `let`|`const` id `=` expr
    ///
    /// ### Option Flags
    ///
    /// * [`AST_OPTION_PUBLIC`][AST_OPTION_FLAGS::AST_OPTION_PUBLIC] = `export`
    /// * [`AST_OPTION_CONSTANT`][AST_OPTION_FLAGS::AST_OPTION_CONSTANT] = `const`
    Var(Expr, Box<Ident>, OptionFlags, Position),
    /// expr op`=` expr
    Assignment(Box<(Expr, Option<OpAssignment<'static>>, Expr)>, Position),
    /// func `(` expr `,` ... `)`
    ///
    /// Note - this is a duplicate of [`Expr::FnCall`] to cover the very common pattern of a single
    ///        function call forming one statement.
    FnCall(Box<FnCallExpr>, Position),
    /// `{` stmt`;` ... `}`
    Block(Box<[Stmt]>, Position),
    /// `try` `{` stmt; ... `}` `catch` `(` var `)` `{` stmt; ... `}`
    TryCatch(Box<(StmtBlock, Option<Ident>, StmtBlock)>, Position),
    /// [expression][Expr]
    Expr(Expr),
    /// `continue`/`break`
    ///
    /// ### Option Flags
    ///
    /// * [`AST_OPTION_NONE`][AST_OPTION_FLAGS::AST_OPTION_NONE] = `continue`
    /// * [`AST_OPTION_BREAK_OUT`][AST_OPTION_FLAGS::AST_OPTION_BREAK_OUT] = `break`
    BreakLoop(OptionFlags, Position),
    /// `return`/`throw`
    ///
    /// ### Option Flags
    ///
    /// * [`AST_OPTION_NONE`][AST_OPTION_FLAGS::AST_OPTION_NONE] = `return`
    /// * [`AST_OPTION_BREAK_OUT`][AST_OPTION_FLAGS::AST_OPTION_BREAK_OUT] = `throw`
    Return(OptionFlags, Option<Expr>, Position),
    /// `import` expr `as` var
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    Import(Expr, Option<Box<Ident>>, Position),
    /// `export` var `as` var `,` ...
    ///
    /// Not available under `no_module`.
    #[cfg(not(feature = "no_module"))]
    Export(Box<[(Ident, Ident)]>, Position),
    /// Convert a variable to shared.
    ///
    /// Not available under `no_closure`.
    ///
    /// # Notes
    ///
    /// This variant does not map to any language structure.  It is currently only used only to
    /// convert a normal variable into a shared variable when the variable is _captured_ by a closure.
    #[cfg(not(feature = "no_closure"))]
    Share(crate::Identifier),
}

impl Default for Stmt {
    #[inline(always)]
    fn default() -> Self {
        Self::Noop(Position::NONE)
    }
}

impl From<Stmt> for StmtBlock {
    #[inline]
    fn from(stmt: Stmt) -> Self {
        match stmt {
            Stmt::Block(mut block, pos) => Self(block.iter_mut().map(mem::take).collect(), pos),
            Stmt::Noop(pos) => Self(StaticVec::new_const(), pos),
            _ => {
                let pos = stmt.position();
                Self(vec![stmt].into(), pos)
            }
        }
    }
}

impl Stmt {
    /// Is this statement [`Noop`][Stmt::Noop]?
    #[inline(always)]
    #[must_use]
    pub const fn is_noop(&self) -> bool {
        matches!(self, Self::Noop(_))
    }
    /// Get the [position][Position] of this statement.
    #[must_use]
    pub const fn position(&self) -> Position {
        match self {
            Self::Noop(pos)
            | Self::BreakLoop(_, pos)
            | Self::Block(_, pos)
            | Self::Assignment(_, pos)
            | Self::FnCall(_, pos)
            | Self::If(_, _, pos)
            | Self::Switch(_, _, pos)
            | Self::While(_, _, pos)
            | Self::Do(_, _, _, pos)
            | Self::For(_, _, pos)
            | Self::Return(_, _, pos)
            | Self::Var(_, _, _, pos)
            | Self::TryCatch(_, pos) => *pos,

            Self::Expr(x) => x.position(),

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, pos) => *pos,
            #[cfg(not(feature = "no_module"))]
            Self::Export(_, pos) => *pos,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => Position::NONE,
        }
    }
    /// Override the [position][Position] of this statement.
    pub fn set_position(&mut self, new_pos: Position) -> &mut Self {
        match self {
            Self::Noop(pos)
            | Self::BreakLoop(_, pos)
            | Self::Block(_, pos)
            | Self::Assignment(_, pos)
            | Self::FnCall(_, pos)
            | Self::If(_, _, pos)
            | Self::Switch(_, _, pos)
            | Self::While(_, _, pos)
            | Self::Do(_, _, _, pos)
            | Self::For(_, _, pos)
            | Self::Return(_, _, pos)
            | Self::Var(_, _, _, pos)
            | Self::TryCatch(_, pos) => *pos = new_pos,

            Self::Expr(x) => {
                x.set_position(new_pos);
            }

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, pos) => *pos = new_pos,
            #[cfg(not(feature = "no_module"))]
            Self::Export(_, pos) => *pos = new_pos,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => (),
        }

        self
    }
    /// Does this statement return a value?
    #[must_use]
    pub const fn returns_value(&self) -> bool {
        match self {
            Self::If(_, _, _)
            | Self::Switch(_, _, _)
            | Self::Block(_, _)
            | Self::Expr(_)
            | Self::FnCall(_, _) => true,

            Self::Noop(_)
            | Self::While(_, _, _)
            | Self::Do(_, _, _, _)
            | Self::For(_, _, _)
            | Self::TryCatch(_, _) => false,

            Self::Var(_, _, _, _)
            | Self::Assignment(_, _)
            | Self::BreakLoop(_, _)
            | Self::Return(_, _, _) => false,

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, _) | Self::Export(_, _) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => false,
        }
    }
    /// Is this statement self-terminated (i.e. no need for a semicolon terminator)?
    #[must_use]
    pub const fn is_self_terminated(&self) -> bool {
        match self {
            Self::If(_, _, _)
            | Self::Switch(_, _, _)
            | Self::While(_, _, _)
            | Self::For(_, _, _)
            | Self::Block(_, _)
            | Self::TryCatch(_, _) => true,

            // A No-op requires a semicolon in order to know it is an empty statement!
            Self::Noop(_) => false,

            Self::Expr(Expr::Custom(x, _)) if x.is_self_terminated() => true,

            Self::Var(_, _, _, _)
            | Self::Assignment(_, _)
            | Self::Expr(_)
            | Self::FnCall(_, _)
            | Self::Do(_, _, _, _)
            | Self::BreakLoop(_, _)
            | Self::Return(_, _, _) => false,

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, _) | Self::Export(_, _) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => false,
        }
    }
    /// Is this statement _pure_?
    ///
    /// A pure statement has no side effects.
    #[must_use]
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Noop(_) => true,
            Self::Expr(expr) => expr.is_pure(),
            Self::If(condition, x, _) => {
                condition.is_pure()
                    && (x.0).0.iter().all(Stmt::is_pure)
                    && (x.1).0.iter().all(Stmt::is_pure)
            }
            Self::Switch(expr, x, _) => {
                expr.is_pure()
                    && x.0.values().all(|block| {
                        block.0.as_ref().map(Expr::is_pure).unwrap_or(true)
                            && (block.1).0.iter().all(Stmt::is_pure)
                    })
                    && (x.2).iter().all(|(_, _, _, condition, stmt)| {
                        condition.as_ref().map(Expr::is_pure).unwrap_or(true)
                            && stmt.0.iter().all(Stmt::is_pure)
                    })
                    && (x.1).0.iter().all(Stmt::is_pure)
            }

            // Loops that exit can be pure because it can never be infinite.
            Self::While(Expr::BoolConstant(false, _), _, _) => true,
            Self::Do(body, Expr::BoolConstant(x, _), options, _)
                if *x == options.contains(AST_OPTION_FLAGS::AST_OPTION_NEGATED) =>
            {
                body.iter().all(Stmt::is_pure)
            }

            // Loops are never pure since they can be infinite - and that's a side effect.
            Self::While(_, _, _) | Self::Do(_, _, _, _) => false,

            // For loops can be pure because if the iterable is pure, it is finite,
            // so infinite loops can never occur.
            Self::For(iterable, x, _) => iterable.is_pure() && (x.2).0.iter().all(Stmt::is_pure),

            Self::Var(_, _, _, _) | Self::Assignment(_, _) | Self::FnCall(_, _) => false,
            Self::Block(block, _) => block.iter().all(|stmt| stmt.is_pure()),
            Self::BreakLoop(_, _) | Self::Return(_, _, _) => false,
            Self::TryCatch(x, _) => {
                (x.0).0.iter().all(Stmt::is_pure) && (x.2).0.iter().all(Stmt::is_pure)
            }

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, _) => false,
            #[cfg(not(feature = "no_module"))]
            Self::Export(_, _) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => false,
        }
    }
    /// Is this statement _pure_ within the containing block?
    ///
    /// An internally pure statement only has side effects that disappear outside the block.
    ///
    /// Currently only variable definitions (i.e. `let` and `const`) and `import`/`export`
    /// statements are internally pure.
    #[inline]
    #[must_use]
    pub fn is_internally_pure(&self) -> bool {
        match self {
            Self::Var(expr, _, _, _) => expr.is_pure(),

            #[cfg(not(feature = "no_module"))]
            Self::Import(expr, _, _) => expr.is_pure(),
            #[cfg(not(feature = "no_module"))]
            Self::Export(_, _) => true,

            _ => self.is_pure(),
        }
    }
    /// Does this statement break the current control flow through the containing block?
    ///
    /// Currently this is only true for `return`, `throw`, `break` and `continue`.
    ///
    /// All statements following this statement will essentially be dead code.
    #[inline]
    #[must_use]
    pub const fn is_control_flow_break(&self) -> bool {
        match self {
            Self::Return(_, _, _) | Self::BreakLoop(_, _) => true,
            _ => false,
        }
    }
    /// Recursively walk this statement.
    /// Return `false` from the callback to terminate the walk.
    pub fn walk<'a>(
        &'a self,
        path: &mut Vec<ASTNode<'a>>,
        on_node: &mut impl FnMut(&[ASTNode]) -> bool,
    ) -> bool {
        // Push the current node onto the path
        path.push(self.into());

        if !on_node(path) {
            return false;
        }

        match self {
            Self::Var(e, _, _, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
            }
            Self::If(e, x, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in &(x.0).0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in &(x.1).0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Switch(e, x, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for b in x.0.values() {
                    if !b.0.as_ref().map(|e| e.walk(path, on_node)).unwrap_or(true) {
                        return false;
                    }
                    for s in &(b.1).0 {
                        if !s.walk(path, on_node) {
                            return false;
                        }
                    }
                }
                for (_, _, _, c, stmt) in &x.2 {
                    if !c.as_ref().map(|e| e.walk(path, on_node)).unwrap_or(true) {
                        return false;
                    }
                    for s in &stmt.0 {
                        if !s.walk(path, on_node) {
                            return false;
                        }
                    }
                }
                for s in &(x.1).0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::While(e, s, _) | Self::Do(s, e, _, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in &s.0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::For(e, x, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in &(x.2).0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Assignment(x, _) => {
                if !x.0.walk(path, on_node) {
                    return false;
                }
                if !x.2.walk(path, on_node) {
                    return false;
                }
            }
            Self::FnCall(x, _) => {
                for s in &x.args {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Block(x, _) => {
                for s in x.iter() {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::TryCatch(x, _) => {
                for s in &(x.0).0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in &(x.2).0 {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Expr(e) | Self::Return(_, Some(e), _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
            }
            #[cfg(not(feature = "no_module"))]
            Self::Import(e, _, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
            }
            _ => (),
        }

        path.pop().expect("contains current node");

        true
    }
}
