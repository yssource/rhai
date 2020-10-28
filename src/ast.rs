//! Module defining the AST (abstract syntax tree).

use crate::fn_native::Shared;
use crate::module::Module;
use crate::parser::{FnAccess, ScriptFnDef, Stmt};

use crate::stdlib::{
    ops::{Add, AddAssign},
    vec,
    vec::Vec,
};

/// Compiled AST (abstract syntax tree) of a Rhai script.
///
/// # Thread Safety
///
/// Currently, `AST` is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
#[derive(Debug, Clone, Default)]
pub struct AST(
    /// Global statements.
    Vec<Stmt>,
    /// Script-defined functions.
    Module,
);

impl AST {
    /// Create a new `AST`.
    #[inline(always)]
    pub fn new(statements: Vec<Stmt>, lib: Module) -> Self {
        Self(statements, lib)
    }

    /// Get the statements.
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    pub(crate) fn statements(&self) -> &[Stmt] {
        &self.0
    }

    /// _[INTERNALS]_ Get the statements.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[deprecated(note = "this method is volatile and may change")]
    #[inline(always)]
    pub fn statements(&self) -> &[Stmt] {
        &self.0
    }

    /// Get a mutable reference to the statements.
    #[cfg(not(feature = "no_optimize"))]
    #[inline(always)]
    pub(crate) fn statements_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.0
    }

    /// Get the internal `Module` containing all script-defined functions.
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    pub(crate) fn lib(&self) -> &Module {
        &self.1
    }

    /// _[INTERNALS]_ Get the internal `Module` containing all script-defined functions.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[deprecated(note = "this method is volatile and may change")]
    #[inline(always)]
    pub fn lib(&self) -> &Module {
        &self.1
    }

    /// Clone the `AST`'s functions into a new `AST`.
    /// No statements are cloned.
    ///
    /// This operation is cheap because functions are shared.
    #[inline(always)]
    pub fn clone_functions_only(&self) -> Self {
        self.clone_functions_only_filtered(|_, _, _| true)
    }

    /// Clone the `AST`'s functions into a new `AST` based on a filter predicate.
    /// No statements are cloned.
    ///
    /// This operation is cheap because functions are shared.
    #[inline(always)]
    pub fn clone_functions_only_filtered(
        &self,
        mut filter: impl FnMut(FnAccess, &str, usize) -> bool,
    ) -> Self {
        let mut functions: Module = Default::default();
        functions.merge_filtered(&self.1, &mut filter);
        Self(Default::default(), functions)
    }

    /// Clone the `AST`'s script statements into a new `AST`.
    /// No functions are cloned.
    #[inline(always)]
    pub fn clone_statements_only(&self) -> Self {
        Self(self.0.clone(), Default::default())
    }

    /// Merge two `AST` into one.  Both `AST`'s are untouched and a new, merged, version
    /// is returned.
    ///
    /// Statements in the second `AST` are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first `AST` (if using expression-statement syntax) is buried.
    /// Of course, if the first `AST` uses a `return` statement at the end, then
    /// the second `AST` will essentially be dead code.
    ///
    /// All script-defined functions in the second `AST` overwrite similarly-named functions
    /// in the first `AST` with the same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let ast1 = engine.compile(r#"
    ///                 fn foo(x) { 42 + x }
    ///                 foo(1)
    ///             "#)?;
    ///
    /// let ast2 = engine.compile(r#"
    ///                 fn foo(n) { "hello" + n }
    ///                 foo("!")
    ///             "#)?;
    ///
    /// let ast = ast1.merge(&ast2);    // Merge 'ast2' into 'ast1'
    ///
    /// // Notice that using the '+' operator also works:
    /// // let ast = &ast1 + &ast2;
    ///
    /// // 'ast' is essentially:
    /// //
    /// //    fn foo(n) { "hello" + n } // <- definition of first 'foo' is overwritten
    /// //    foo(1)                    // <- notice this will be "hello1" instead of 43,
    /// //                              //    but it is no longer the return value
    /// //    foo("!")                  // returns "hello!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast)?, "hello!");
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn merge(&self, other: &Self) -> Self {
        self.merge_filtered(other, |_, _, _| true)
    }

    /// Combine one `AST` with another.  The second `AST` is consumed.
    ///
    /// Statements in the second `AST` are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first `AST` (if using expression-statement syntax) is buried.
    /// Of course, if the first `AST` uses a `return` statement at the end, then
    /// the second `AST` will essentially be dead code.
    ///
    /// All script-defined functions in the second `AST` overwrite similarly-named functions
    /// in the first `AST` with the same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let mut ast1 = engine.compile(r#"
    ///                     fn foo(x) { 42 + x }
    ///                     foo(1)
    ///                 "#)?;
    ///
    /// let ast2 = engine.compile(r#"
    ///                 fn foo(n) { "hello" + n }
    ///                 foo("!")
    ///             "#)?;
    ///
    /// ast1.combine(ast2);    // Combine 'ast2' into 'ast1'
    ///
    /// // Notice that using the '+=' operator also works:
    /// // ast1 += ast2;
    ///
    /// // 'ast1' is essentially:
    /// //
    /// //    fn foo(n) { "hello" + n } // <- definition of first 'foo' is overwritten
    /// //    foo(1)                    // <- notice this will be "hello1" instead of 43,
    /// //                              //    but it is no longer the return value
    /// //    foo("!")                  // returns "hello!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast1)?, "hello!");
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn combine(&mut self, other: Self) -> &mut Self {
        self.combine_filtered(other, |_, _, _| true)
    }

    /// Merge two `AST` into one.  Both `AST`'s are untouched and a new, merged, version
    /// is returned.
    ///
    /// Statements in the second `AST` are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first `AST` (if using expression-statement syntax) is buried.
    /// Of course, if the first `AST` uses a `return` statement at the end, then
    /// the second `AST` will essentially be dead code.
    ///
    /// All script-defined functions in the second `AST` are first selected based on a filter
    /// predicate, then overwrite similarly-named functions in the first `AST` with the
    /// same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let ast1 = engine.compile(r#"
    ///                 fn foo(x) { 42 + x }
    ///                 foo(1)
    ///             "#)?;
    ///
    /// let ast2 = engine.compile(r#"
    ///                 fn foo(n) { "hello" + n }
    ///                 fn error() { 0 }
    ///                 foo("!")
    ///             "#)?;
    ///
    /// // Merge 'ast2', picking only 'error()' but not 'foo(_)', into 'ast1'
    /// let ast = ast1.merge_filtered(&ast2, |_, name, params| name == "error" && params == 0);
    ///
    /// // 'ast' is essentially:
    /// //
    /// //    fn foo(n) { 42 + n }      // <- definition of 'ast1::foo' is not overwritten
    /// //                              //    because 'ast2::foo' is filtered away
    /// //    foo(1)                    // <- notice this will be 43 instead of "hello1",
    /// //                              //    but it is no longer the return value
    /// //    fn error() { 0 }          // <- this function passes the filter and is merged
    /// //    foo("!")                  // <- returns "42!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast)?, "42!");
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline]
    pub fn merge_filtered(
        &self,
        other: &Self,
        mut filter: impl FnMut(FnAccess, &str, usize) -> bool,
    ) -> Self {
        let Self(statements, functions) = self;

        let ast = match (statements.is_empty(), other.0.is_empty()) {
            (false, false) => {
                let mut statements = statements.clone();
                statements.extend(other.0.iter().cloned());
                statements
            }
            (false, true) => statements.clone(),
            (true, false) => other.0.clone(),
            (true, true) => vec![],
        };

        let mut functions = functions.clone();
        functions.merge_filtered(&other.1, &mut filter);

        Self::new(ast, functions)
    }

    /// Combine one `AST` with another.  The second `AST` is consumed.
    ///
    /// Statements in the second `AST` are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first `AST` (if using expression-statement syntax) is buried.
    /// Of course, if the first `AST` uses a `return` statement at the end, then
    /// the second `AST` will essentially be dead code.
    ///
    /// All script-defined functions in the second `AST` are first selected based on a filter
    /// predicate, then overwrite similarly-named functions in the first `AST` with the
    /// same number of parameters.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let mut ast1 = engine.compile(r#"
    ///                     fn foo(x) { 42 + x }
    ///                     foo(1)
    ///                 "#)?;
    ///
    /// let ast2 = engine.compile(r#"
    ///                 fn foo(n) { "hello" + n }
    ///                 fn error() { 0 }
    ///                 foo("!")
    ///             "#)?;
    ///
    /// // Combine 'ast2', picking only 'error()' but not 'foo(_)', into 'ast1'
    /// ast1.combine_filtered(ast2, |_, name, params| name == "error" && params == 0);
    ///
    /// // 'ast1' is essentially:
    /// //
    /// //    fn foo(n) { 42 + n }      // <- definition of 'ast1::foo' is not overwritten
    /// //                              //    because 'ast2::foo' is filtered away
    /// //    foo(1)                    // <- notice this will be 43 instead of "hello1",
    /// //                              //    but it is no longer the return value
    /// //    fn error() { 0 }          // <- this function passes the filter and is merged
    /// //    foo("!")                  // <- returns "42!"
    ///
    /// // Evaluate it
    /// assert_eq!(engine.eval_ast::<String>(&ast1)?, "42!");
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[inline(always)]
    pub fn combine_filtered(
        &mut self,
        other: Self,
        mut filter: impl FnMut(FnAccess, &str, usize) -> bool,
    ) -> &mut Self {
        let Self(ref mut statements, ref mut functions) = self;
        statements.extend(other.0.into_iter());
        functions.merge_filtered(&other.1, &mut filter);
        self
    }

    /// Filter out the functions, retaining only some based on a filter predicate.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// # #[cfg(not(feature = "no_function"))]
    /// # {
    /// use rhai::Engine;
    ///
    /// let engine = Engine::new();
    ///
    /// let mut ast = engine.compile(r#"
    ///                         fn foo(n) { n + 1 }
    ///                         fn bar() { print("hello"); }
    ///                     "#)?;
    ///
    /// // Remove all functions except 'foo(_)'
    /// ast.retain_functions(|_, name, params| name == "foo" && params == 1);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn retain_functions(&mut self, filter: impl FnMut(FnAccess, &str, usize) -> bool) {
        self.1.retain_functions(filter);
    }

    /// Iterate through all functions
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn iter_functions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FnAccess, &str, usize, Shared<ScriptFnDef>)> + 'a {
        self.1.iter_script_fn()
    }

    /// Clear all function definitions in the `AST`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn clear_functions(&mut self) {
        self.1 = Default::default();
    }

    /// Clear all statements in the `AST`, leaving only function definitions.
    #[inline(always)]
    pub fn clear_statements(&mut self) {
        self.0 = vec![];
    }
}

impl<A: AsRef<AST>> Add<A> for &AST {
    type Output = AST;

    #[inline(always)]
    fn add(self, rhs: A) -> Self::Output {
        self.merge(rhs.as_ref())
    }
}

impl<A: Into<AST>> AddAssign<A> for AST {
    #[inline(always)]
    fn add_assign(&mut self, rhs: A) {
        self.combine(rhs.into());
    }
}

impl AsRef<[Stmt]> for AST {
    #[inline(always)]
    fn as_ref(&self) -> &[Stmt] {
        self.statements()
    }
}

impl AsRef<Module> for AST {
    #[inline(always)]
    fn as_ref(&self) -> &Module {
        self.lib()
    }
}
