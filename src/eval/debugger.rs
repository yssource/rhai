//! Module defining the debugging interface.
#![cfg(feature = "debugging")]

use super::{EvalContext, EvalState, GlobalRuntimeState};
use crate::ast::{ASTNode, Expr, Stmt};
use crate::{Dynamic, Engine, Identifier, Module, Position, Scope};
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
    AtFunctionName { name: Identifier },
    /// Break at a particular function call with a particular number of arguments.
    AtFunctionCall { name: Identifier, args: usize },
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
            Self::AtFunctionName { name: fn_name } => write!(f, "{} (...)", fn_name),
            Self::AtFunctionCall {
                name: fn_name,
                args,
            } => write!(
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

/// A function call.
#[cfg(not(feature = "no_function"))]
#[derive(Debug, Clone, Hash)]
pub struct CallStackFrame {
    pub fn_name: Identifier,
    pub args: crate::StaticVec<Dynamic>,
    pub source: Identifier,
    pub pos: Position,
}

#[cfg(not(feature = "no_function"))]
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
    status: DebuggerCommand,
    break_points: Vec<BreakPoint>,
    #[cfg(not(feature = "no_function"))]
    call_stack: Vec<CallStackFrame>,
}

impl Debugger {
    /// Create a new [`Debugger`].
    pub const fn new() -> Self {
        Self {
            status: DebuggerCommand::Continue,
            break_points: Vec::new(),
            #[cfg(not(feature = "no_function"))]
            call_stack: Vec::new(),
        }
    }
    /// Get the function call stack depth.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn call_stack_len(&self) -> usize {
        self.call_stack.len()
    }
    /// Get the current call stack.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn call_stack(&self) -> &[CallStackFrame] {
        &self.call_stack
    }
    /// Rewind the function call stack to a particular depth.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub(crate) fn rewind_call_stack(&mut self, len: usize) {
        self.call_stack.truncate(len);
    }
    /// Add a new frame to the function call stack.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub(crate) fn push_call_stack_frame(
        &mut self,
        fn_name: impl Into<Identifier>,
        args: crate::StaticVec<Dynamic>,
        source: impl Into<Identifier>,
        pos: Position,
    ) {
        self.call_stack.push(CallStackFrame {
            fn_name: fn_name.into(),
            args,
            source: source.into(),
            pos,
        });
    }
    /// Get the current status of this [`Debugger`].
    #[inline(always)]
    #[must_use]
    pub fn status(&self) -> DebuggerCommand {
        self.status
    }
    /// Set the status of this [`Debugger`].
    #[inline(always)]
    pub fn set_status(&mut self, status: DebuggerCommand) {
        self.status = status;
    }
    /// Set the status of this [`Debugger`].
    #[inline(always)]
    pub fn reset_status(&mut self, status: Option<DebuggerCommand>) {
        if let Some(cmd) = status {
            self.status = cmd;
        }
    }
    /// Activate: set the status of this [`Debugger`] to [`DebuggerCommand::StepInto`].  
    /// Deactivate: set the status of this [`Debugger`] to [`DebuggerCommand::Continue`].
    #[inline(always)]
    pub fn activate(&mut self, active: bool) {
        if active {
            self.set_status(DebuggerCommand::StepInto);
        } else {
            self.set_status(DebuggerCommand::Continue);
        }
    }
    /// Does a particular [`AST` Node][ASTNode] trigger a break-point?
    pub fn is_break_point(&self, src: &str, node: ASTNode) -> bool {
        self.break_points().iter().any(|bp| match bp {
            #[cfg(not(feature = "no_position"))]
            BreakPoint::AtPosition { source, pos } => node.position() == *pos && src == source,
            BreakPoint::AtFunctionName { name } => match node {
                ASTNode::Expr(Expr::FnCall(x, _)) | ASTNode::Stmt(Stmt::FnCall(x, _)) => {
                    x.name == *name
                }
                _ => false,
            },
            BreakPoint::AtFunctionCall { name, args } => match node {
                ASTNode::Expr(Expr::FnCall(x, _)) | ASTNode::Stmt(Stmt::FnCall(x, _)) => {
                    x.args.len() == *args && x.name == *name
                }
                _ => false,
            },
        })
    }
    /// Get a slice of all [`BreakPoint`]'s.
    #[inline(always)]
    #[must_use]
    pub fn break_points(&self) -> &[BreakPoint] {
        &self.break_points
    }
    /// Get the underlying [`Vec`] holding all [`BreakPoint`]'s.
    #[inline(always)]
    #[must_use]
    pub fn break_points_mut(&mut self) -> &mut Vec<BreakPoint> {
        &mut self.break_points
    }
}

impl Engine {
    /// Run the debugger callback.
    #[inline(always)]
    pub(crate) fn run_debugger<'a>(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        node: impl Into<ASTNode<'a>>,
        level: usize,
    ) {
        if let Some(cmd) =
            self.run_debugger_with_reset(scope, global, state, lib, this_ptr, node, level)
        {
            global.debugger.set_status(cmd);
        }
    }
    /// Run the debugger callback.
    ///
    /// Returns `true` if the debugger needs to be reactivated at the end of the block, statement or
    /// function call.
    ///
    /// # Note
    ///
    /// When the debugger callback return [`DebuggerCommand::StepOver`], the debugger if temporarily
    /// disabled and `true` is returned.
    ///
    /// It is up to the [`Engine`] to reactivate the debugger.
    #[inline]
    #[must_use]
    pub(crate) fn run_debugger_with_reset<'a>(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        node: impl Into<ASTNode<'a>>,
        level: usize,
    ) -> Option<DebuggerCommand> {
        if let Some(ref on_debugger) = self.debugger {
            let node = node.into();

            let stop = match global.debugger.status {
                DebuggerCommand::Continue => false,
                DebuggerCommand::StepOver => matches!(node, ASTNode::Stmt(_)),
                DebuggerCommand::StepInto => true,
            };

            if !stop && !global.debugger.is_break_point(&global.source, node) {
                return None;
            }

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

            let command = on_debugger(&mut context, node, source, node.position());

            match command {
                DebuggerCommand::Continue => {
                    global.debugger.set_status(DebuggerCommand::Continue);
                    None
                }
                DebuggerCommand::StepInto => {
                    global.debugger.set_status(DebuggerCommand::StepInto);
                    None
                }
                DebuggerCommand::StepOver => {
                    global.debugger.set_status(DebuggerCommand::Continue);
                    Some(DebuggerCommand::StepOver)
                }
            }
        } else {
            None
        }
    }
}
