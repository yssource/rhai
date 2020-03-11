//! Module that defines the extern API of `Engine`.

use crate::any::{Any, AnyExt, Dynamic};
use crate::call::FuncArgs;
use crate::engine::{Engine, FnAny, FnCallArgs, FnSpec};
use crate::error::ParseError;
use crate::fn_register::RegisterFn;
use crate::parser::{lex, parse, Position, AST};
use crate::result::EvalAltResult;
use crate::scope::Scope;
use std::{
    any::{type_name, TypeId},
    fs::File,
    io::prelude::*,
};

impl<'e> Engine<'e> {
    pub(crate) fn register_fn_raw(
        &mut self,
        fn_name: &str,
        args: Option<Vec<TypeId>>,
        f: Box<FnAny>,
    ) {
        debug_println!(
            "Register function: {} with {}",
            fn_name,
            if let Some(a) = &args {
                format!(
                    "{} parameter{}",
                    a.len(),
                    if a.len() > 1 { "s" } else { "" }
                )
            } else {
                "no parameter".to_string()
            }
        );

        let spec = FnSpec {
            name: fn_name.to_string().into(),
            args,
        };

        self.ext_functions.insert(spec, f);
    }

    /// Register a custom type for use with the `Engine`.
    /// The type must be `Clone`.
    pub fn register_type<T: Any + Clone>(&mut self) {
        self.register_type_with_name::<T>(type_name::<T>());
    }

    /// Register a custom type for use with the `Engine` with a name for the `type_of` function.
    /// The type must be `Clone`.
    pub fn register_type_with_name<T: Any + Clone>(&mut self, name: &str) {
        // Add the pretty-print type name into the map
        self.type_names
            .insert(type_name::<T>().to_string(), name.to_string());
    }

    /// Register an iterator adapter for a type with the `Engine`.
    pub fn register_iterator<T: Any, F>(&mut self, f: F)
    where
        F: Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + 'static,
    {
        self.type_iterators.insert(TypeId::of::<T>(), Box::new(f));
    }

    /// Register a getter function for a member of a registered type with the `Engine`.
    pub fn register_get<T: Any + Clone, U: Any + Clone>(
        &mut self,
        name: &str,
        callback: impl Fn(&mut T) -> U + 'static,
    ) {
        let get_name = "get$".to_string() + name;
        self.register_fn(&get_name, callback);
    }

    /// Register a setter function for a member of a registered type with the `Engine`.
    pub fn register_set<T: Any + Clone, U: Any + Clone>(
        &mut self,
        name: &str,
        callback: impl Fn(&mut T, U) -> () + 'static,
    ) {
        let set_name = "set$".to_string() + name;
        self.register_fn(&set_name, callback);
    }

    /// Shorthand for registering both getter and setter functions
    /// of a registered type with the `Engine`.
    pub fn register_get_set<T: Any + Clone, U: Any + Clone>(
        &mut self,
        name: &str,
        get_fn: impl Fn(&mut T) -> U + 'static,
        set_fn: impl Fn(&mut T, U) -> () + 'static,
    ) {
        self.register_get(name, get_fn);
        self.register_set(name, set_fn);
    }

    /// Compile a string into an AST.
    pub fn compile(&self, input: &str) -> Result<AST, ParseError> {
        let tokens = lex(input);
        parse(&mut tokens.peekable(), self.optimize)
    }

    fn read_file(filename: &str) -> Result<String, EvalAltResult> {
        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))
            .map(|_| contents)
    }

    /// Compile a file into an AST.
    pub fn compile_file(&self, filename: &str) -> Result<AST, EvalAltResult> {
        Self::read_file(filename)
            .and_then(|contents| self.compile(&contents).map_err(|err| err.into()))
    }

    /// Evaluate a file.
    pub fn eval_file<T: Any + Clone>(&mut self, filename: &str) -> Result<T, EvalAltResult> {
        Self::read_file(filename).and_then(|contents| self.eval::<T>(&contents))
    }

    /// Evaluate a string.
    pub fn eval<T: Any + Clone>(&mut self, input: &str) -> Result<T, EvalAltResult> {
        let mut scope = Scope::new();
        self.eval_with_scope(&mut scope, false, input)
    }

    /// Evaluate a string with own scope.
    ///
    /// Note - if `retain_functions` is set to `true`, functions defined by previous scripts are _retained_
    ///        and not cleared from run to run.
    pub fn eval_with_scope<T: Any + Clone>(
        &mut self,
        scope: &mut Scope,
        retain_functions: bool,
        input: &str,
    ) -> Result<T, EvalAltResult> {
        let ast = self.compile(input).map_err(EvalAltResult::ErrorParsing)?;
        self.eval_ast_with_scope(scope, retain_functions, &ast)
    }

    /// Evaluate an AST.
    pub fn eval_ast<T: Any + Clone>(&mut self, ast: &AST) -> Result<T, EvalAltResult> {
        let mut scope = Scope::new();
        self.eval_ast_with_scope(&mut scope, false, ast)
    }

    /// Evaluate an AST with own scope.
    ///
    /// Note - if `retain_functions` is set to `true`, functions defined by previous scripts are _retained_
    ///        and not cleared from run to run.
    pub fn eval_ast_with_scope<T: Any + Clone>(
        &mut self,
        scope: &mut Scope,
        retain_functions: bool,
        ast: &AST,
    ) -> Result<T, EvalAltResult> {
        fn eval_ast_internal(
            engine: &mut Engine,
            scope: &mut Scope,
            retain_functions: bool,
            ast: &AST,
        ) -> Result<Dynamic, EvalAltResult> {
            #[cfg(feature = "no_function")]
            let AST(statements) = ast;

            #[cfg(not(feature = "no_function"))]
            let statements = {
                let AST(statements, functions) = ast;

                functions.iter().for_each(|f| {
                    engine.script_functions.push(f.clone());
                });

                statements
            };

            let result = statements
                .iter()
                .try_fold(().into_dynamic(), |_, stmt| engine.eval_stmt(scope, stmt));

            if !retain_functions {
                engine.clear_functions();
            }

            result
        }

        match eval_ast_internal(self, scope, retain_functions, ast) {
            Err(EvalAltResult::Return(out, pos)) => out.downcast::<T>().map(|v| *v).map_err(|a| {
                EvalAltResult::ErrorMismatchOutputType(
                    self.map_type_name((*a).type_name()).to_string(),
                    pos,
                )
            }),

            Ok(out) => out.downcast::<T>().map(|v| *v).map_err(|a| {
                EvalAltResult::ErrorMismatchOutputType(
                    self.map_type_name((*a).type_name()).to_string(),
                    Position::eof(),
                )
            }),

            Err(err) => Err(err),
        }
    }

    /// Evaluate a file, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    pub fn consume_file(&mut self, filename: &str) -> Result<(), EvalAltResult> {
        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))
            .and_then(|_| self.consume(&contents))
    }

    /// Evaluate a string, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    pub fn consume(&mut self, input: &str) -> Result<(), EvalAltResult> {
        self.consume_with_scope(&mut Scope::new(), false, input)
    }

    /// Evaluate a string, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need to keep track of possible errors.
    ///
    /// Note - if `retain_functions` is set to `true`, functions defined by previous scripts are _retained_
    ///        and not cleared from run to run.
    pub fn consume_with_scope(
        &mut self,
        scope: &mut Scope,
        retain_functions: bool,
        input: &str,
    ) -> Result<(), EvalAltResult> {
        let tokens = lex(input);

        parse(&mut tokens.peekable(), self.optimize)
            .map_err(|err| EvalAltResult::ErrorParsing(err))
            .and_then(|ast| {
                #[cfg(feature = "no_function")]
                let AST(statements) = ast;

                #[cfg(not(feature = "no_function"))]
                let statements = {
                    let AST(ref statements, ref functions) = ast;

                    functions.iter().for_each(|f| {
                        self.script_functions.push(f.clone());
                    });

                    statements
                };

                let val = statements
                    .iter()
                    .try_fold(().into_dynamic(), |_, o| self.eval_stmt(scope, o))
                    .map(|_| ());

                if !retain_functions {
                    self.clear_functions();
                }

                val
            })
    }

    /// Call a script function defined in a compiled AST.
    ///
    /// # Example
    ///
    /// ```rust
    /// # fn main() -> Result<(), rhai::EvalAltResult> {
    /// # #[cfg(not(feature = "no_stdlib"))]
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let mut engine = Engine::new();
    ///
    /// let ast = engine.compile("fn add(x, y) { x.len() + y }")?;
    ///
    /// let result: i64 = engine.call_fn("add", &ast, (String::from("abc"), 123_i64))?;
    ///
    /// assert_eq!(result, 126);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    pub fn call_fn<A: FuncArgs, T: Any + Clone>(
        &mut self,
        name: &str,
        ast: &AST,
        args: A,
    ) -> Result<T, EvalAltResult> {
        fn call_fn_internal(
            engine: &mut Engine,
            name: &str,
            ast: &AST,
            args: FnCallArgs,
        ) -> Result<Dynamic, EvalAltResult> {
            ast.1.iter().for_each(|f| {
                engine.script_functions.push(f.clone());
            });

            let result = engine.call_fn_raw(name, args, None, Position::none());

            engine.clear_functions();

            result
        }

        let mut arg_values = args.into_vec();

        call_fn_internal(
            self,
            name,
            ast,
            arg_values.iter_mut().map(|v| v.as_mut()).collect(),
        )
        .and_then(|b| {
            b.downcast().map(|b| *b).map_err(|a| {
                EvalAltResult::ErrorMismatchOutputType(
                    self.map_type_name((*a).type_name()).into(),
                    Position::none(),
                )
            })
        })
    }

    /// Override default action of `print` (print to stdout using `println!`)
    ///
    /// # Example
    ///
    /// ```rust
    /// # fn main() -> Result<(), rhai::EvalAltResult> {
    /// use rhai::Engine;
    ///
    /// let mut result = String::from("");
    /// {
    ///     let mut engine = Engine::new();
    ///
    ///     // Override action of 'print' function
    ///     engine.on_print(|s| result.push_str(s));
    ///     engine.consume("print(40 + 2);")?;
    /// }
    /// assert_eq!(result, "42");
    /// # Ok(())
    /// # }
    /// ```
    pub fn on_print(&mut self, callback: impl FnMut(&str) + 'e) {
        self.on_print = Box::new(callback);
    }

    /// Override default action of `debug` (print to stdout using `println!`)
    ///
    /// # Example
    ///
    /// ```rust
    /// # fn main() -> Result<(), rhai::EvalAltResult> {
    /// use rhai::Engine;
    ///
    /// let mut result = String::from("");
    /// {
    ///     let mut engine = Engine::new();
    ///
    ///     // Override action of 'debug' function
    ///     engine.on_debug(|s| result.push_str(s));
    ///     engine.consume(r#"debug("hello");"#)?;
    /// }
    /// assert_eq!(result, "\"hello\"");
    /// # Ok(())
    /// # }
    /// ```
    pub fn on_debug(&mut self, callback: impl FnMut(&str) + 'e) {
        self.on_debug = Box::new(callback);
    }
}
