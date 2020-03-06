use crate::any::{Any, AnyExt, Dynamic};
use crate::call::FuncArgs;
use crate::engine::{Engine, FnAny, FnIntExt, FnSpec};
use crate::error::ParseError;
use crate::fn_register::RegisterFn;
use crate::parser::{lex, parse, Position, AST};
use crate::result::EvalAltResult;
use crate::scope::Scope;
use std::any::TypeId;
use std::sync::Arc;

impl<'e> Engine<'e> {
    pub(crate) fn register_fn_raw(
        &mut self,
        fn_name: &str,
        args: Option<Vec<TypeId>>,
        f: Box<FnAny>,
    ) {
        debug_println!(
            "Register function: {} for {} parameter(s)",
            fn_name,
            if let Some(a) = &args {
                format!("{}", a.len())
            } else {
                "no".to_string()
            }
        );

        let spec = FnSpec {
            name: fn_name.to_string().into(),
            args,
        };

        self.external_functions
            .insert(spec, Arc::new(FnIntExt::Ext(f)));
    }

    /// Register a custom type for use with the `Engine`.
    /// The type must be `Clone`.
    pub fn register_type<T: Any + Clone>(&mut self) {
        self.register_type_with_name::<T>(std::any::type_name::<T>());
    }

    /// Register a custom type for use with the `Engine` with a name for the `type_of` function.
    /// The type must be `Clone`.
    pub fn register_type_with_name<T: Any + Clone>(&mut self, type_name: &str) {
        // Add the pretty-print type name into the map
        self.type_names.insert(
            std::any::type_name::<T>().to_string(),
            type_name.to_string(),
        );
    }

    /// Register an iterator adapter for a type with the `Engine`.
    pub fn register_iterator<T: Any, F>(&mut self, f: F)
    where
        F: Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + 'static,
    {
        self.type_iterators.insert(TypeId::of::<T>(), Arc::new(f));
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
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))
            .and_then(|_| Self::compile(&contents).map_err(EvalAltResult::ErrorParsing))
    }

    /// Evaluate a file
    pub fn eval_file<T: Any + Clone>(&mut self, filename: &str) -> Result<T, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))
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
        let AST(statements, functions) = ast;

        functions.iter().for_each(|f| {
            self.script_functions.insert(
                FnSpec {
                    name: f.name.clone().into(),
                    args: None,
                },
                Arc::new(FnIntExt::Int(f.clone())),
            );
        });

        let result = statements
            .iter()
            .try_fold(().into_dynamic(), |_, stmt| self.eval_stmt(scope, stmt));

        self.script_functions.clear(); // Clean up engine

        match result {
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
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume_file(&mut self, filename: &str) -> Result<(), EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        let mut f = File::open(filename)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))?;

        let mut contents = String::new();

        f.read_to_string(&mut contents)
            .map_err(|err| EvalAltResult::ErrorReadingScriptFile(filename.into(), err))
            .and_then(|_| self.consume(&contents))
    }

    /// Evaluate a string, but throw away the result and only return error (if any).
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume(&mut self, input: &str) -> Result<(), EvalAltResult> {
        self.consume_with_scope(&mut Scope::new(), input)
    }

    /// Evaluate a string with own scope, but throw away the result and only return error (if any).
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
            .and_then(|AST(ref statements, ref functions)| {
                for f in functions {
                    self.script_functions.insert(
                        FnSpec {
                            name: f.name.clone().into(),
                            args: None,
                        },
                        Arc::new(FnIntExt::Int(f.clone())),
                    );
                }

                let val = statements
                    .iter()
                    .try_fold(().into_dynamic(), |_, o| self.eval_stmt(scope, o))
                    .map(|_| ());

                self.script_functions.clear(); // Clean up engine

                val
            })
    }

    /// Call a script function defined in a compiled AST.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use rhai::{Engine, EvalAltResult};
    /// # fn main() -> Result<(), EvalAltResult> {
    /// let mut engine = Engine::new();
    ///
    /// let ast = Engine::compile("fn add(x, y) { x.len() + y }")?;
    ///
    /// let result: i64 = engine.call_fn("add", &ast, (&mut String::from("abc"), &mut 123_i64))?;
    ///
    /// assert_eq!(result, 126);
    /// # Ok(())
    /// # }
    /// ```
    pub fn call_fn<'f, A: FuncArgs<'f>, T: Any + Clone>(
        &mut self,
        name: &str,
        ast: &AST,
        args: A,
    ) -> Result<T, EvalAltResult> {
        let pos = Default::default();

        ast.1.iter().for_each(|f| {
            self.script_functions.insert(
                FnSpec {
                    name: f.name.clone().into(),
                    args: None,
                },
                Arc::new(FnIntExt::Int(f.clone())),
            );
        });

        let result = self
            .call_fn_raw(name, args.into_vec(), None, pos)
            .and_then(|b| {
                b.downcast().map(|b| *b).map_err(|a| {
                    EvalAltResult::ErrorMismatchOutputType(
                        self.map_type_name((*a).type_name()).into(),
                        pos,
                    )
                })
            });

        self.script_functions.clear(); // Clean up engine

        result
    }

    /// Override default action of `print` (print to stdout using `println!`)
    ///
    /// # Example
    ///
    /// ```rust
    /// # use rhai::Engine;
    /// let mut result = String::from("");
    /// {
    ///     let mut engine = Engine::new();
    ///
    ///     // Override action of 'print' function
    ///     engine.on_print(|s| result.push_str(s));
    ///     engine.consume("print(40 + 2);").unwrap();
    /// }
    /// assert_eq!(result, "42");
    /// ```
    pub fn on_print(&mut self, callback: impl FnMut(&str) + 'e) {
        self.on_print = Box::new(callback);
    }

    /// Override default action of `debug` (print to stdout using `println!`)
    ///
    /// # Example
    ///
    /// ```rust
    /// # use rhai::Engine;
    /// let mut result = String::from("");
    /// {
    ///     let mut engine = Engine::new();
    ///
    ///     // Override action of 'debug' function
    ///     engine.on_debug(|s| result.push_str(s));
    ///     engine.consume(r#"debug("hello");"#).unwrap();
    /// }
    /// assert_eq!(result, "\"hello\"");
    /// ```
    pub fn on_debug(&mut self, callback: impl FnMut(&str) + 'e) {
        self.on_debug = Box::new(callback);
    }
}
