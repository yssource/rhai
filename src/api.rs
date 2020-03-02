use crate::any::{Any, AnyExt, Dynamic};
use crate::engine::{Engine, EvalAltResult, FnIntExt, FnSpec, Scope};
use crate::parser::{lex, parse, ParseError, Position, AST};
use std::sync::Arc;

impl Engine {
    /// Compile a string into an AST
    pub fn compile(input: &str) -> Result<AST, ParseError> {
        let tokens = lex(input);
        parse(&mut tokens.peekable())
    }

    /// Compile a file into an AST
    pub fn compile_file(filename: &str) -> Result<AST, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorCantOpenScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorCantOpenScriptFile(filename.into(), err))
            .and_then(|_| Self::compile(&contents).map_err(EvalAltResult::ErrorParsing))
    }

    /// Evaluate a file
    pub fn eval_file<T: Any + Clone>(&mut self, filename: &str) -> Result<T, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorCantOpenScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorCantOpenScriptFile(filename.into(), err))
            .and_then(|_| self.eval::<T>(&contents))
    }

    /// Evaluate a string
    pub fn eval<T: Any + Clone>(&mut self, input: &str) -> Result<T, EvalAltResult> {
        let mut scope = Scope::new();
        self.eval_with_scope(&mut scope, input)
    }

    /// Evaluate a string with own scope
    pub fn eval_with_scope<T: Any + Clone>(
        &mut self,
        scope: &mut Scope,
        input: &str,
    ) -> Result<T, EvalAltResult> {
        let ast = Self::compile(input).map_err(EvalAltResult::ErrorParsing)?;
        self.eval_ast_with_scope(scope, &ast)
    }

    /// Evaluate an AST
    pub fn eval_ast<T: Any + Clone>(&mut self, ast: &AST) -> Result<T, EvalAltResult> {
        let mut scope = Scope::new();
        self.eval_ast_with_scope(&mut scope, ast)
    }

    /// Evaluate an AST with own scope
    pub fn eval_ast_with_scope<T: Any + Clone>(
        &mut self,
        scope: &mut Scope,
        ast: &AST,
    ) -> Result<T, EvalAltResult> {
        let AST(os, fns) = ast;

        fns.iter().for_each(|f| {
            self.script_fns.insert(
                FnSpec {
                    ident: f.name.clone(),
                    args: None,
                },
                Arc::new(FnIntExt::Int(f.clone())),
            );
        });

        let result = os
            .iter()
            .try_fold(Box::new(()) as Dynamic, |_, o| self.eval_stmt(scope, o));

        self.script_fns.clear(); // Clean up engine

        match result {
            Err(EvalAltResult::Return(out, pos)) => Ok(*out
                .downcast::<T>()
                .map_err(|a| EvalAltResult::ErrorMismatchOutputType((*a).type_name(), pos))?),

            Ok(out) => Ok(*out.downcast::<T>().map_err(|a| {
                EvalAltResult::ErrorMismatchOutputType((*a).type_name(), Position::eof())
            })?),

            Err(err) => Err(err),
        }
    }

    /// Evaluate a file, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume_file(&mut self, filename: &str) -> Result<(), EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorCantOpenScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorCantOpenScriptFile(filename.into(), err))
            .and_then(|_| self.consume(&contents))
    }

    /// Evaluate a string, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume(&mut self, input: &str) -> Result<(), EvalAltResult> {
        self.consume_with_scope(&mut Scope::new(), input)
    }

    /// Evaluate a string with own scope, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume_with_scope(
        &mut self,
        scope: &mut Scope,
        input: &str,
    ) -> Result<(), EvalAltResult> {
        let tokens = lex(input);

        parse(&mut tokens.peekable())
            .map_err(|err| EvalAltResult::ErrorParsing(err))
            .and_then(|AST(ref os, ref fns)| {
                for f in fns {
                    // FIX - Why are functions limited to 6 parameters?
                    if f.params.len() > 6 {
                        return Ok(());
                    }

                    self.script_fns.insert(
                        FnSpec {
                            ident: f.name.clone(),
                            args: None,
                        },
                        Arc::new(FnIntExt::Int(f.clone())),
                    );
                }

                let val = os
                    .iter()
                    .try_fold(Box::new(()) as Dynamic, |_, o| self.eval_stmt(scope, o))
                    .map(|_| ());

                self.script_fns.clear(); // Clean up engine

                val
            })
    }

    /// Overrides `on_print`
    pub fn on_print(&mut self, callback: impl Fn(&str) + 'static) {
        self.on_print = Box::new(callback);
    }

    /// Overrides `on_debug`
    pub fn on_debug(&mut self, callback: impl Fn(&str) + 'static) {
        self.on_debug = Box::new(callback);
    }
}
