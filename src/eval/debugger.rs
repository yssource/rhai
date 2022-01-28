//! Module defining the debugging interface.
#![cfg(feature = "debugging")]

use super::{EvalContext, EvalState, GlobalRuntimeState};
use crate::ast::{ASTNode, Expr, Stmt};
use crate::{Dynamic, Engine, Identifier, Module, Position, RhaiResultOf, Scope};
use std::fmt;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// A standard callback function for debugging.
#[cfg(not(feature = "sync"))]
pub type OnDebuggerCallback =
    dyn Fn(&mut EvalContext, ASTNode, Option<&str>, Position) -> RhaiResultOf<DebuggerCommand>;
/// A standard callback function for debugging.
#[cfg(feature = "sync")]
pub type OnDebuggerCallback = dyn Fn(&mut EvalContext, ASTNode, Option<&str>, Position) -> RhaiResultOf<DebuggerCommand>
    + Send
    + Sync;

/// A command for the debugger on the next iteration.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum DebuggerCommand {
    // Continue normal execution.
    Continue,
    // Step into the next expression, diving into functions.
    StepInto,
    // Run to the next expression or statement, stepping over functions.
    StepOver,
    // Run to the next statement, skipping over functions.
    Next,
}

/// A break-point for debugging.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BreakPoint {
    /// Break at a particular position under a particular source.
    ///
    /// Not available under `no_position`.
    ///
    /// Source is empty if not available.
    #[cfg(not(feature = "no_position"))]
    AtPosition {
        source: Identifier,
        pos: Position,
        enabled: bool,
    },
    /// Break at a particular function call.
    AtFunctionName { name: Identifier, enabled: bool },
    /// Break at a particular function call with a particular number of arguments.
    AtFunctionCall {
        name: Identifier,
        args: usize,
        enabled: bool,
    },
    /// Break at a particular property .
    ///
    /// Not available under `no_object`.
    #[cfg(not(feature = "no_object"))]
    AtProperty { name: Identifier, enabled: bool },
}

impl fmt::Display for BreakPoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            #[cfg(not(feature = "no_position"))]
            Self::AtPosition {
                source,
                pos,
                enabled,
            } => {
                if !source.is_empty() {
                    write!(f, "{} @ {:?}", source, pos)?;
                } else {
                    write!(f, "@ {:?}", pos)?;
                }
                if !*enabled {
                    f.write_str(" (disabled)")?;
                }
                Ok(())
            }
            Self::AtFunctionName {
                name: fn_name,
                enabled,
            } => {
                write!(f, "{} (...)", fn_name)?;
                if !*enabled {
                    f.write_str(" (disabled)")?;
                }
                Ok(())
            }
            Self::AtFunctionCall {
                name: fn_name,
                args,
                enabled,
            } => {
                write!(
                    f,
                    "{} ({})",
                    fn_name,
                    std::iter::repeat("_")
                        .take(*args)
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                if !*enabled {
                    f.write_str(" (disabled)")?;
                }
                Ok(())
            }
            #[cfg(not(feature = "no_object"))]
            Self::AtProperty {
                name: prop,
                enabled,
            } => {
                write!(f, ".{}", prop)?;
                if !*enabled {
                    f.write_str(" (disabled)")?;
                }
                Ok(())
            }
        }
    }
}

impl BreakPoint {
    /// Is this [`BreakPoint`] enabled?
    #[inline(always)]
    pub fn is_enabled(&self) -> bool {
        match self {
            #[cfg(not(feature = "no_position"))]
            Self::AtPosition { enabled, .. } => *enabled,
            Self::AtFunctionName { enabled, .. } | Self::AtFunctionCall { enabled, .. } => *enabled,
            #[cfg(not(feature = "no_object"))]
            Self::AtProperty { enabled, .. } => *enabled,
        }
    }
    /// Enable/disable this [`BreakPoint`].
    #[inline(always)]
    pub fn enable(&mut self, value: bool) {
        match self {
            #[cfg(not(feature = "no_position"))]
            Self::AtPosition { enabled, .. } => *enabled = value,
            Self::AtFunctionName { enabled, .. } | Self::AtFunctionCall { enabled, .. } => {
                *enabled = value
            }
            #[cfg(not(feature = "no_object"))]
            Self::AtProperty { enabled, .. } => *enabled = value,
        }
    }
}

/// A function call.
#[cfg(not(feature = "no_function"))]
#[derive(Debug, Clone, Hash)]
pub struct CallStackFrame {
    /// Function name.
    pub fn_name: Identifier,
    /// Copies of function call arguments, if any.
    pub args: crate::StaticVec<Dynamic>,
    /// Source of the function, empty if none.
    pub source: Identifier,
    /// [Position][`Position`] of the function call.
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
    /// The current status command.
    status: DebuggerCommand,
    /// The current set of break-points.
    break_points: Vec<BreakPoint>,
    /// The current function call stack.
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
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn call_stack_len(&self) -> usize {
        self.call_stack.len()
    }
    /// Get the current call stack.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn call_stack(&self) -> &[CallStackFrame] {
        &self.call_stack
    }
    /// Rewind the function call stack to a particular depth.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub(crate) fn rewind_call_stack(&mut self, len: usize) {
        self.call_stack.truncate(len);
    }
    /// Add a new frame to the function call stack.
    ///
    /// Not available under `no_function`.
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
        let _src = src;

        self.break_points()
            .iter()
            .filter(|&bp| bp.is_enabled())
            .any(|bp| match bp {
                #[cfg(not(feature = "no_position"))]
                BreakPoint::AtPosition { pos, .. } if pos.is_none() => false,
                #[cfg(not(feature = "no_position"))]
                BreakPoint::AtPosition { source, pos, .. } if pos.is_beginning_of_line() => {
                    node.position().line().unwrap_or(0) == pos.line().unwrap() && _src == source
                }
                #[cfg(not(feature = "no_position"))]
                BreakPoint::AtPosition { source, pos, .. } => {
                    node.position() == *pos && _src == source
                }
                BreakPoint::AtFunctionName { name, .. } => match node {
                    ASTNode::Expr(Expr::FnCall(x, _)) | ASTNode::Stmt(Stmt::FnCall(x, _)) => {
                        x.name == *name
                    }
                    _ => false,
                },
                BreakPoint::AtFunctionCall { name, args, .. } => match node {
                    ASTNode::Expr(Expr::FnCall(x, _)) | ASTNode::Stmt(Stmt::FnCall(x, _)) => {
                        x.args.len() == *args && x.name == *name
                    }
                    _ => false,
                },
                #[cfg(not(feature = "no_object"))]
                BreakPoint::AtProperty { name, .. } => match node {
                    ASTNode::Expr(Expr::Property(x, _)) => x.2 == *name,
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
    ) -> RhaiResultOf<()> {
        if let Some(cmd) =
            self.run_debugger_with_reset(scope, global, state, lib, this_ptr, node, level)?
        {
            global.debugger.set_status(cmd);
        }

        Ok(())
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
    ) -> RhaiResultOf<Option<DebuggerCommand>> {
        if let Some(ref on_debugger) = self.debugger {
            let node = node.into();

            // Skip transitive nodes
            match node {
                ASTNode::Expr(Expr::Stmt(_)) | ASTNode::Stmt(Stmt::Expr(_)) => return Ok(None),
                _ => (),
            }

            let stop = match global.debugger.status {
                DebuggerCommand::Continue => false,
                DebuggerCommand::Next => matches!(node, ASTNode::Stmt(_)),
                DebuggerCommand::StepInto | DebuggerCommand::StepOver => true,
            };

            if !stop && !global.debugger.is_break_point(&global.source, node) {
                return Ok(None);
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

            let command = on_debugger(&mut context, node, source, node.position())?;

            match command {
                DebuggerCommand::Continue => {
                    global.debugger.set_status(DebuggerCommand::Continue);
                    Ok(None)
                }
                DebuggerCommand::Next => {
                    global.debugger.set_status(DebuggerCommand::Continue);
                    Ok(Some(DebuggerCommand::Next))
                }
                DebuggerCommand::StepInto => {
                    global.debugger.set_status(DebuggerCommand::StepInto);
                    Ok(None)
                }
                DebuggerCommand::StepOver => {
                    global.debugger.set_status(DebuggerCommand::Continue);
                    Ok(Some(DebuggerCommand::StepOver))
                }
            }
        } else {
            Ok(None)
        }
    }
}
