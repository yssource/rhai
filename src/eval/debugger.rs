//! Module defining the debugging interface.
#![cfg(feature = "debugging")]

use super::{EvalContext, EvalState, GlobalRuntimeState};
use crate::ast::{ASTNode, Expr, Stmt};
use crate::{Dynamic, Engine, Identifier, Module, Position, Scope, StaticVec};
use std::fmt;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// A standard callback function for debugging.
#[cfg(not(feature = "sync"))]
pub type OnDebuggerCallback =
    Box<dyn Fn(&mut EvalContext, ASTNode, Option<&str>, Position) -> DebuggerCommand + 'static>;
/// A standard callback function for debugging.
#[cfg(feature = "sync")]
pub type OnDebuggerCallback = Box<
    dyn Fn(&mut EvalContext, ASTNode, Option<&str>, Position) -> DebuggerCommand
        + Send
        + Sync
        + 'static,
>;

/// A command for the debugger on the next iteration.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DebuggerCommand {
    // Continue normal execution.
    Continue,
    // Step into the next expression, diving into functions.
    StepInto,
    // Run to the next statement, stepping over functions.
    StepOver,
}

/// A break-point for debugging.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BreakPoint {
    /// Break at a particular position under a particular source.
    /// Not available under `no_position`.
    ///
    /// Source is empty if not available.
    #[cfg(not(feature = "no_position"))]
    AtPosition { source: Identifier, pos: Position },
    /// Break at a particular function call.
    AtFunctionName { fn_name: Identifier },
    /// Break at a particular function call with a particular number of arguments.
    AtFunctionCall { fn_name: Identifier, args: usize },
}

impl fmt::Display for BreakPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AtPosition { source, pos } => {
                if !source.is_empty() {
                    write!(f, "{} @ {:?}", source, pos)
                } else {
                    write!(f, "@ {:?}", pos)
                }
            }
            Self::AtFunctionName { fn_name } => write!(f, "{} (...)", fn_name),
            Self::AtFunctionCall { fn_name, args } => write!(
                f,
                "{} ({})",
                fn_name,
                std::iter::repeat("_")
                    .take(*args)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct CallStackFrame {
    pub fn_name: Identifier,
    pub args: StaticVec<Dynamic>,
    pub source: Identifier,
    pub pos: Position,
}

impl fmt::Display for CallStackFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fp = f.debug_tuple(&self.fn_name);

        for arg in &self.args {
            fp.field(arg);
        }

        fp.finish()?;

        if !self.pos.is_none() {
            if self.source.is_empty() {
                write!(f, " @ {:?}", self.pos)?;
            } else {
                write!(f, ": {} @ {:?}", self.source, self.pos)?;
            }
        }

        Ok(())
    }
}

/// A type providing debugging facilities.
#[derive(Debug, Clone, Hash)]
pub struct Debugger {
    active: bool,
    break_points: Vec<BreakPoint>,
    call_stack: Vec<CallStackFrame>,
}

impl Debugger {
    /// Create a new [`Debugger`].
    pub const fn new() -> Self {
        Self {
            active: false,
            break_points: Vec::new(),
            call_stack: Vec::new(),
        }
    }
    /// Get the function call stack depth.
    #[inline(always)]
    pub fn call_stack_len(&self) -> usize {
        self.call_stack.len()
    }
    /// Rewind the function call stack to a particular depth.
    #[inline(always)]
    pub fn rewind_call_stack(&mut self, len: usize) {
        self.call_stack.truncate(len);
    }
    /// Add a new frame to the function call stack.
    #[inline(always)]
    pub fn push_call_stack_frame(
        &mut self,
        fn_name: impl Into<Identifier>,
        args: StaticVec<Dynamic>,
        source: impl Into<Identifier>,
        pos: Position,
    ) {
        let fp = CallStackFrame {
            fn_name: fn_name.into(),
            args,
            source: source.into(),
            pos,
        };
        println!("{}", fp);
        self.call_stack.push(fp);
    }
    /// Is this [`Debugger`] currently active?
    #[inline(always)]
    #[must_use]
    pub fn is_active(&self) -> bool {
        self.active
    }
    /// Activate or deactivate this [`Debugger`].
    #[inline(always)]
    pub fn activate(&mut self, active: bool) {
        self.active = active;
    }
    /// Does a particular [`AST` Node][ASTNode] trigger a break-point?
    pub fn is_break_point(&self, src: &str, node: ASTNode) -> bool {
        self.iter_break_points().any(|bp| match bp {
            #[cfg(not(feature = "no_position"))]
            BreakPoint::AtPosition { source, pos } => node.position() == *pos && src == source,
            BreakPoint::AtFunctionName { fn_name } => match node {
                ASTNode::Expr(Expr::FnCall(x, _)) | ASTNode::Stmt(Stmt::FnCall(x, _)) => {
                    x.name == *fn_name
                }
                _ => false,
            },
            BreakPoint::AtFunctionCall { fn_name, args } => match node {
                ASTNode::Expr(Expr::FnCall(x, _)) | ASTNode::Stmt(Stmt::FnCall(x, _)) => {
                    x.args.len() == *args && x.name == *fn_name
                }
                _ => false,
            },
        })
    }
    /// Get a slice of all [`BreakPoint`]'s.
    #[inline(always)]
    #[must_use]
    pub fn break_points(&mut self) -> &[BreakPoint] {
        &self.break_points
    }
    /// Get the underlying [`Vec`] holding all [`BreakPoint`]'s.
    #[inline(always)]
    #[must_use]
    pub fn break_points_mut(&mut self) -> &mut Vec<BreakPoint> {
        &mut self.break_points
    }
    /// Get an iterator over all [`BreakPoint`]'s.
    #[inline(always)]
    #[must_use]
    pub fn iter_break_points(&self) -> impl Iterator<Item = &BreakPoint> {
        self.break_points.iter()
    }
    /// Get a mutable iterator over all [`BreakPoint`]'s.
    #[inline(always)]
    #[must_use]
    pub fn iter_break_points_mut(&mut self) -> impl Iterator<Item = &mut BreakPoint> {
        self.break_points.iter_mut()
    }
}

impl Engine {
    pub(crate) fn run_debugger(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        node: ASTNode,
        level: usize,
    ) -> bool {
        if let Some(ref on_debugger) = self.debugger {
            if global.debugger.active || global.debugger.is_break_point(&global.source, node) {
                let source = global.source.clone();
                let source = if source.is_empty() {
                    None
                } else {
                    Some(source.as_str())
                };
                let mut context = crate::EvalContext {
                    engine: self,
                    scope,
                    global,
                    state,
                    lib,
                    this_ptr,
                    level,
                };

                match on_debugger(&mut context, node, source, node.position()) {
                    DebuggerCommand::Continue => {
                        global.debugger.activate(false);
                        return false;
                    }
                    DebuggerCommand::StepInto => {
                        global.debugger.activate(true);
                        return true;
                    }
                    DebuggerCommand::StepOver => {
                        global.debugger.activate(false);
                        return true;
                    }
                }
            }
        }

        false
    }
}
