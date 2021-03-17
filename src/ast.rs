//! Module defining the AST (abstract syntax tree).

use crate::dynamic::{AccessMode, Union};
use crate::fn_native::shared_make_mut;
use crate::module::NamespaceRef;
use crate::stdlib::{
    borrow::Cow,
    boxed::Box,
    fmt,
    hash::Hash,
    num::NonZeroUsize,
    ops::{Add, AddAssign},
    string::String,
    vec,
    vec::Vec,
};
use crate::token::Token;
use crate::utils::{HashableHashMap, StraightHasherBuilder};
use crate::{
    Dynamic, FnNamespace, FnPtr, ImmutableString, Module, Position, Shared, StaticVec, INT,
};

#[cfg(not(feature = "no_float"))]
use crate::{stdlib::str::FromStr, FLOAT};

#[cfg(not(feature = "no_index"))]
use crate::Array;

#[cfg(not(feature = "no_object"))]
use crate::Map;

/// A type representing the access mode of a function.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum FnAccess {
    /// Public function.
    Public,
    /// Private function.
    Private,
}

/// _(INTERNALS)_ A type containing information on a scripted function.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone)]
pub struct ScriptFnDef {
    /// Function body.
    pub body: StmtBlock,
    /// Encapsulated running environment, if any.
    pub lib: Option<Shared<Module>>,
    /// Encapsulated imported modules.
    #[cfg(not(feature = "no_module"))]
    pub mods: crate::engine::Imports,
    /// Function name.
    pub name: ImmutableString,
    /// Function access mode.
    pub access: FnAccess,
    /// Names of function parameters.
    pub params: StaticVec<ImmutableString>,
    /// Access to external variables.
    #[cfg(not(feature = "no_closure"))]
    pub externals: StaticVec<ImmutableString>,
    /// Function doc-comments (if any).
    pub comments: StaticVec<String>,
}

impl fmt::Display for ScriptFnDef {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}({})",
            match self.access {
                FnAccess::Public => "",
                FnAccess::Private => "private ",
            },
            self.name,
            self.params
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// A type containing the metadata of a script-defined function.
///
/// Created by [`AST::iter_functions`].
#[cfg(not(feature = "no_function"))]
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ScriptFnMetadata<'a> {
    /// Function doc-comments (if any).
    ///
    /// Block doc-comments are kept in a single string slice with line-breaks within.
    ///
    /// Line doc-comments are kept in one string slice per line without the termination line-break.
    ///
    /// Leading white-spaces are stripped, and each string slice always starts with the corresponding
    /// doc-comment leader: `///` or `/**`.
    pub comments: Vec<&'a str>,
    /// Function access mode.
    pub access: FnAccess,
    /// Function name.
    pub name: &'a str,
    /// Function parameters (if any).
    pub params: Vec<&'a str>,
}

#[cfg(not(feature = "no_function"))]
impl fmt::Display for ScriptFnMetadata<'_> {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}({})",
            match self.access {
                FnAccess::Public => "",
                FnAccess::Private => "private ",
            },
            self.name,
            self.params.iter().cloned().collect::<Vec<_>>().join(", ")
        )
    }
}

#[cfg(not(feature = "no_function"))]
impl<'a> Into<ScriptFnMetadata<'a>> for &'a ScriptFnDef {
    #[inline(always)]
    fn into(self) -> ScriptFnMetadata<'a> {
        ScriptFnMetadata {
            comments: self.comments.iter().map(|s| s.as_str()).collect(),
            access: self.access,
            name: &self.name,
            params: self.params.iter().map(|s| s.as_str()).collect(),
        }
    }
}

/// Compiled AST (abstract syntax tree) of a Rhai script.
///
/// # Thread Safety
///
/// Currently, [`AST`] is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
#[derive(Debug, Clone)]
pub struct AST {
    /// Source of the [`AST`].
    source: Option<ImmutableString>,
    /// Global statements.
    body: StmtBlock,
    /// Script-defined functions.
    functions: Shared<Module>,
    /// Embedded module resolver, if any.
    #[cfg(not(feature = "no_module"))]
    resolver: Option<Shared<crate::module::resolvers::StaticModuleResolver>>,
}

impl Default for AST {
    #[inline(always)]
    fn default() -> Self {
        Self {
            source: None,
            body: Default::default(),
            functions: Default::default(),
            #[cfg(not(feature = "no_module"))]
            resolver: None,
        }
    }
}

impl AST {
    /// Create a new [`AST`].
    #[inline(always)]
    pub fn new(
        statements: impl IntoIterator<Item = Stmt>,
        functions: impl Into<Shared<Module>>,
    ) -> Self {
        Self {
            source: None,
            body: StmtBlock {
                statements: statements.into_iter().collect(),
                pos: Position::NONE,
            },
            functions: functions.into(),
            #[cfg(not(feature = "no_module"))]
            resolver: None,
        }
    }
    /// Create a new [`AST`] with a source name.
    #[inline(always)]
    pub fn new_with_source(
        statements: impl IntoIterator<Item = Stmt>,
        functions: impl Into<Shared<Module>>,
        source: impl Into<ImmutableString>,
    ) -> Self {
        Self {
            source: Some(source.into()),
            body: StmtBlock {
                statements: statements.into_iter().collect(),
                pos: Position::NONE,
            },
            functions: functions.into(),
            #[cfg(not(feature = "no_module"))]
            resolver: None,
        }
    }
    /// Get the source, if any.
    #[inline(always)]
    pub fn source(&self) -> Option<&str> {
        self.source.as_ref().map(|s| s.as_str())
    }
    /// Clone the source, if any.
    #[inline(always)]
    pub(crate) fn clone_source(&self) -> Option<ImmutableString> {
        self.source.clone()
    }
    /// Set the source.
    #[inline(always)]
    pub fn set_source(&mut self, source: impl Into<ImmutableString>) -> &mut Self {
        self.source = Some(source.into());

        if let Some(module) = Shared::get_mut(&mut self.functions) {
            module.set_id(self.source.clone());
        }

        self
    }
    /// Clear the source.
    #[inline(always)]
    pub fn clear_source(&mut self) -> &mut Self {
        self.source = None;
        self
    }
    /// Get the statements.
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    pub(crate) fn statements(&self) -> &[Stmt] {
        &self.body.statements
    }
    /// _(INTERNALS)_ Get the statements.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[deprecated = "this method is volatile and may change"]
    #[inline(always)]
    pub fn statements(&self) -> &[Stmt] {
        &self.body.statements
    }
    /// Get a mutable reference to the statements.
    #[cfg(not(feature = "no_optimize"))]
    #[inline(always)]
    pub(crate) fn statements_mut(&mut self) -> &mut StaticVec<Stmt> {
        &mut self.body.statements
    }
    /// Get the internal shared [`Module`] containing all script-defined functions.
    #[cfg(not(feature = "internals"))]
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub(crate) fn shared_lib(&self) -> Shared<Module> {
        self.functions.clone()
    }
    /// _(INTERNALS)_ Get the internal shared [`Module`] containing all script-defined functions.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[deprecated = "this method is volatile and may change"]
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn shared_lib(&self) -> Shared<Module> {
        self.functions.clone()
    }
    /// Get the internal [`Module`] containing all script-defined functions.
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    pub(crate) fn lib(&self) -> &Module {
        &self.functions
    }
    /// _(INTERNALS)_ Get the internal [`Module`] containing all script-defined functions.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[deprecated = "this method is volatile and may change"]
    #[inline(always)]
    pub fn lib(&self) -> &Module {
        &self.functions
    }
    /// Get the embedded [module resolver][`ModuleResolver`].
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "internals"))]
    #[inline(always)]
    pub(crate) fn resolver(
        &self,
    ) -> Option<Shared<crate::module::resolvers::StaticModuleResolver>> {
        self.resolver.clone()
    }
    /// _(INTERNALS)_ Get the embedded [module resolver][crate::ModuleResolver].
    /// Exported under the `internals` feature only.
    #[cfg(not(feature = "no_module"))]
    #[cfg(feature = "internals")]
    #[inline(always)]
    pub fn resolver(&self) -> Option<Shared<crate::module::resolvers::StaticModuleResolver>> {
        self.resolver.clone()
    }
    /// Set the embedded [module resolver][`ModuleResolver`].
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub(crate) fn set_resolver(
        &mut self,
        resolver: impl Into<Shared<crate::module::resolvers::StaticModuleResolver>>,
    ) -> &mut Self {
        self.resolver = Some(resolver.into());
        self
    }
    /// Clone the [`AST`]'s functions into a new [`AST`].
    /// No statements are cloned.
    ///
    /// This operation is cheap because functions are shared.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn clone_functions_only(&self) -> Self {
        self.clone_functions_only_filtered(|_, _, _, _, _| true)
    }
    /// Clone the [`AST`]'s functions into a new [`AST`] based on a filter predicate.
    /// No statements are cloned.
    ///
    /// This operation is cheap because functions are shared.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn clone_functions_only_filtered(
        &self,
        filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> Self {
        let mut functions: Module = Default::default();
        functions.merge_filtered(&self.functions, &filter);
        Self {
            source: self.source.clone(),
            body: Default::default(),
            functions: functions.into(),
            #[cfg(not(feature = "no_module"))]
            resolver: self.resolver.clone(),
        }
    }
    /// Clone the [`AST`]'s script statements into a new [`AST`].
    /// No functions are cloned.
    #[inline(always)]
    pub fn clone_statements_only(&self) -> Self {
        Self {
            source: self.source.clone(),
            body: self.body.clone(),
            functions: Default::default(),
            #[cfg(not(feature = "no_module"))]
            resolver: self.resolver.clone(),
        }
    }
    /// Merge two [`AST`] into one.  Both [`AST`]'s are untouched and a new, merged, version
    /// is returned.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] overwrite similarly-named functions
    /// in the first [`AST`] with the same number of parameters.
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
        self.merge_filtered(other, |_, _, _, _, _| true)
    }
    /// Combine one [`AST`] with another.  The second [`AST`] is consumed.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] overwrite similarly-named functions
    /// in the first [`AST`] with the same number of parameters.
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
        self.combine_filtered(other, |_, _, _, _, _| true)
    }
    /// Merge two [`AST`] into one.  Both [`AST`]'s are untouched and a new, merged, version
    /// is returned.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] are first selected based on a filter
    /// predicate, then overwrite similarly-named functions in the first [`AST`] with the
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
    /// let ast = ast1.merge_filtered(&ast2, |_, _, script, name, params|
    ///                                 script && name == "error" && params == 0);
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
        filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> Self {
        let Self {
            body, functions, ..
        } = self;

        let merged = match (body.is_empty(), other.body.is_empty()) {
            (false, false) => {
                let mut body = body.clone();
                body.statements
                    .extend(other.body.statements.iter().cloned());
                body
            }
            (false, true) => body.clone(),
            (true, false) => other.body.clone(),
            (true, true) => Default::default(),
        };

        let source = other.source.clone().or_else(|| self.source.clone());

        let mut functions = functions.as_ref().clone();
        functions.merge_filtered(&other.functions, &filter);

        if let Some(source) = source {
            Self::new_with_source(merged.statements, functions, source)
        } else {
            Self::new(merged.statements, functions)
        }
    }
    /// Combine one [`AST`] with another.  The second [`AST`] is consumed.
    ///
    /// Statements in the second [`AST`] are simply appended to the end of the first _without any processing_.
    /// Thus, the return value of the first [`AST`] (if using expression-statement syntax) is buried.
    /// Of course, if the first [`AST`] uses a `return` statement at the end, then
    /// the second [`AST`] will essentially be dead code.
    ///
    /// All script-defined functions in the second [`AST`] are first selected based on a filter
    /// predicate, then overwrite similarly-named functions in the first [`AST`] with the
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
    /// ast1.combine_filtered(ast2, |_, _, script, name, params|
    ///                                 script && name == "error" && params == 0);
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
        filter: impl Fn(FnNamespace, FnAccess, bool, &str, usize) -> bool,
    ) -> &mut Self {
        self.body
            .statements
            .extend(other.body.statements.into_iter());

        if !other.functions.is_empty() {
            shared_make_mut(&mut self.functions).merge_filtered(&other.functions, &filter);
        }
        self
    }
    /// Filter out the functions, retaining only some based on a filter predicate.
    ///
    /// Not available under `no_function`.
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
    /// ast.retain_functions(|_, _, name, params| name == "foo" && params == 1);
    /// # }
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn retain_functions(
        &mut self,
        filter: impl Fn(FnNamespace, FnAccess, &str, usize) -> bool,
    ) -> &mut Self {
        if !self.functions.is_empty() {
            shared_make_mut(&mut self.functions).retain_script_functions(filter);
        }
        self
    }
    /// Iterate through all function definitions.
    ///
    /// Not available under [`no_function`].
    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub(crate) fn iter_fn_def(&self) -> impl Iterator<Item = &ScriptFnDef> {
        self.functions
            .iter_script_fn()
            .map(|(_, _, _, _, fn_def)| fn_def.as_ref())
    }
    /// Iterate through all function definitions.
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn iter_functions<'a>(&'a self) -> impl Iterator<Item = ScriptFnMetadata> + 'a {
        self.functions
            .iter_script_fn()
            .map(|(_, _, _, _, fn_def)| fn_def.as_ref().into())
    }
    /// Clear all function definitions in the [`AST`].
    ///
    /// Not available under `no_function`.
    #[cfg(not(feature = "no_function"))]
    #[inline(always)]
    pub fn clear_functions(&mut self) {
        self.functions = Default::default();
    }
    /// Clear all statements in the [`AST`], leaving only function definitions.
    #[inline(always)]
    pub fn clear_statements(&mut self) {
        self.body = Default::default();
    }
    /// Recursively walk the [`AST`], including function bodies (if any).
    /// Return `false` from the callback to terminate the walk.
    #[cfg(not(feature = "internals"))]
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub(crate) fn walk(&self, on_node: &mut impl FnMut(&[ASTNode]) -> bool) -> bool {
        let path = &mut Default::default();

        for stmt in self.statements() {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }
        #[cfg(not(feature = "no_function"))]
        for stmt in self.iter_fn_def().flat_map(|f| f.body.statements.iter()) {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }

        true
    }
    /// _(INTERNALS)_ Recursively walk the [`AST`], including function bodies (if any).
    /// Return `false` from the callback to terminate the walk.
    /// Exported under the `internals` feature only.
    #[cfg(feature = "internals")]
    #[inline(always)]
    pub fn walk(&self, on_node: &mut impl FnMut(&[ASTNode]) -> bool) -> bool {
        let path = &mut Default::default();

        for stmt in self.statements() {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }
        #[cfg(not(feature = "no_function"))]
        for stmt in self.iter_fn_def().flat_map(|f| f.body.statements.iter()) {
            if !stmt.walk(path, on_node) {
                return false;
            }
        }

        true
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

/// _(INTERNALS)_ An identifier containing an [immutable string][ImmutableString] name and a [position][Position].
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    /// Identifier name.
    pub name: ImmutableString,
    /// Declaration position.
    pub pos: Position,
}

impl fmt::Debug for Ident {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ident({:?} @ {:?})", self.name, self.pos)
    }
}

/// _(INTERNALS)_ A type encapsulating the mode of a `return`/`throw` statement.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub enum ReturnType {
    /// `return` statement.
    Return,
    /// `throw` statement.
    Exception,
}

/// _(INTERNALS)_ An [`AST`] node, consisting of either an [`Expr`] or a [`Stmt`].
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Hash)]
pub enum ASTNode<'a> {
    Stmt(&'a Stmt),
    Expr(&'a Expr),
}

impl<'a> From<&'a Stmt> for ASTNode<'a> {
    fn from(stmt: &'a Stmt) -> Self {
        Self::Stmt(stmt)
    }
}

impl<'a> From<&'a Expr> for ASTNode<'a> {
    fn from(expr: &'a Expr) -> Self {
        Self::Expr(expr)
    }
}

/// _(INTERNALS)_ A statements block.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Clone, Hash, Default)]
pub struct StmtBlock {
    pub statements: StaticVec<Stmt>,
    pub pos: Position,
}

impl StmtBlock {
    /// Is this statements block empty?
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
    /// Number of statements in this statements block.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

impl fmt::Debug for StmtBlock {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.pos.is_none() {
            write!(f, "{} @ ", self.pos)?;
        }
        fmt::Debug::fmt(&self.statements, f)
    }
}

/// _(INTERNALS)_ A statement.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Hash)]
pub enum Stmt {
    /// No-op.
    Noop(Position),
    /// `if` expr `{` stmt `}` `else` `{` stmt `}`
    If(Expr, Box<(StmtBlock, StmtBlock)>, Position),
    /// `switch` expr `{` literal or _ `=>` stmt `,` ... `}`
    Switch(
        Expr,
        Box<(
            HashableHashMap<u64, StmtBlock, StraightHasherBuilder>,
            StmtBlock,
        )>,
        Position,
    ),
    /// `while` expr `{` stmt `}`
    While(Expr, Box<StmtBlock>, Position),
    /// `do` `{` stmt `}` `while`|`until` expr
    Do(Box<StmtBlock>, Expr, bool, Position),
    /// `for` id `in` expr `{` stmt `}`
    For(Expr, Box<(String, StmtBlock)>, Position),
    /// \[`export`\] `let` id `=` expr
    Let(Expr, Ident, bool, Position),
    /// \[`export`\] `const` id `=` expr
    Const(Expr, Ident, bool, Position),
    /// expr op`=` expr
    Assignment(Box<(Expr, Expr, Option<OpAssignment>)>, Position),
    /// `{` stmt`;` ... `}`
    Block(Vec<Stmt>, Position),
    /// `try` `{` stmt; ... `}` `catch` `(` var `)` `{` stmt; ... `}`
    TryCatch(
        Box<(StmtBlock, Option<Ident>, StmtBlock)>,
        Position,
        Position,
    ),
    /// [expression][Expr]
    Expr(Expr),
    /// `continue`
    Continue(Position),
    /// `break`
    Break(Position),
    /// `return`/`throw`
    Return(ReturnType, Option<Expr>, Position),
    /// `import` expr `as` var
    #[cfg(not(feature = "no_module"))]
    Import(Expr, Option<Ident>, Position),
    /// `export` var `as` var `,` ...
    #[cfg(not(feature = "no_module"))]
    Export(Vec<(Ident, Option<Ident>)>, Position),
    /// Convert a variable to shared.
    #[cfg(not(feature = "no_closure"))]
    Share(Ident),
}

impl Default for Stmt {
    #[inline(always)]
    fn default() -> Self {
        Self::Noop(Position::NONE)
    }
}

impl From<Stmt> for StmtBlock {
    #[inline(always)]
    fn from(stmt: Stmt) -> Self {
        match stmt {
            Stmt::Block(block, pos) => Self {
                statements: block.into(),
                pos,
            },
            Stmt::Noop(pos) => Self {
                statements: Default::default(),
                pos,
            },
            _ => {
                let pos = stmt.position();
                let statements = vec![stmt].into();
                Self { statements, pos }
            }
        }
    }
}

impl Stmt {
    /// Is this statement [`Noop`][Stmt::Noop]?
    #[inline(always)]
    pub fn is_noop(&self) -> bool {
        match self {
            Self::Noop(_) => true,
            _ => false,
        }
    }
    /// Get the [position][Position] of this statement.
    pub fn position(&self) -> Position {
        match self {
            Self::Noop(pos)
            | Self::Continue(pos)
            | Self::Break(pos)
            | Self::Block(_, pos)
            | Self::Assignment(_, pos)
            | Self::If(_, _, pos)
            | Self::Switch(_, _, pos)
            | Self::While(_, _, pos)
            | Self::Do(_, _, _, pos)
            | Self::For(_, _, pos)
            | Self::Return(_, _, pos)
            | Self::Let(_, _, _, pos)
            | Self::Const(_, _, _, pos)
            | Self::TryCatch(_, pos, _) => *pos,

            Self::Expr(x) => x.position(),

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, pos) => *pos,
            #[cfg(not(feature = "no_module"))]
            Self::Export(_, pos) => *pos,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(x) => x.pos,
        }
    }
    /// Override the [position][Position] of this statement.
    pub fn set_position(&mut self, new_pos: Position) -> &mut Self {
        match self {
            Self::Noop(pos)
            | Self::Continue(pos)
            | Self::Break(pos)
            | Self::Block(_, pos)
            | Self::Assignment(_, pos)
            | Self::If(_, _, pos)
            | Self::Switch(_, _, pos)
            | Self::While(_, _, pos)
            | Self::Do(_, _, _, pos)
            | Self::For(_, _, pos)
            | Self::Return(_, _, pos)
            | Self::Let(_, _, _, pos)
            | Self::Const(_, _, _, pos)
            | Self::TryCatch(_, pos, _) => *pos = new_pos,

            Self::Expr(x) => {
                x.set_position(new_pos);
            }

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, pos) => *pos = new_pos,
            #[cfg(not(feature = "no_module"))]
            Self::Export(_, pos) => *pos = new_pos,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(x) => x.pos = new_pos,
        }

        self
    }
    /// Does this statement return a value?
    pub fn returns_value(&self) -> bool {
        match self {
            Self::If(_, _, _) | Self::Switch(_, _, _) | Self::Block(_, _) | Self::Expr(_) => true,

            Self::Noop(_)
            | Self::While(_, _, _)
            | Self::Do(_, _, _, _)
            | Self::For(_, _, _)
            | Self::TryCatch(_, _, _) => false,

            Self::Let(_, _, _, _)
            | Self::Const(_, _, _, _)
            | Self::Assignment(_, _)
            | Self::Continue(_)
            | Self::Break(_)
            | Self::Return(_, _, _) => false,

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, _) | Self::Export(_, _) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => unreachable!("Stmt::Share should not be parsed"),
        }
    }
    /// Is this statement self-terminated (i.e. no need for a semicolon terminator)?
    pub fn is_self_terminated(&self) -> bool {
        match self {
            Self::If(_, _, _)
            | Self::Switch(_, _, _)
            | Self::While(_, _, _)
            | Self::For(_, _, _)
            | Self::Block(_, _)
            | Self::TryCatch(_, _, _) => true,

            // A No-op requires a semicolon in order to know it is an empty statement!
            Self::Noop(_) => false,

            Self::Let(_, _, _, _)
            | Self::Const(_, _, _, _)
            | Self::Assignment(_, _)
            | Self::Expr(_)
            | Self::Do(_, _, _, _)
            | Self::Continue(_)
            | Self::Break(_)
            | Self::Return(_, _, _) => false,

            #[cfg(not(feature = "no_module"))]
            Self::Import(_, _, _) | Self::Export(_, _) => false,

            #[cfg(not(feature = "no_closure"))]
            Self::Share(_) => unreachable!("Stmt::Share should not be parsed"),
        }
    }
    /// Is this statement _pure_?
    ///
    /// A pure statement has no side effects.
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Noop(_) => true,
            Self::Expr(expr) => expr.is_pure(),
            Self::If(condition, x, _) => {
                condition.is_pure()
                    && x.0.statements.iter().all(Stmt::is_pure)
                    && x.1.statements.iter().all(Stmt::is_pure)
            }
            Self::Switch(expr, x, _) => {
                expr.is_pure()
                    && x.0
                        .values()
                        .flat_map(|block| block.statements.iter())
                        .all(Stmt::is_pure)
                    && x.1.statements.iter().all(Stmt::is_pure)
            }
            Self::While(condition, block, _) | Self::Do(block, condition, _, _) => {
                condition.is_pure() && block.statements.iter().all(Stmt::is_pure)
            }
            Self::For(iterable, x, _) => {
                iterable.is_pure() && x.1.statements.iter().all(Stmt::is_pure)
            }
            Self::Let(_, _, _, _) | Self::Const(_, _, _, _) | Self::Assignment(_, _) => false,
            Self::Block(block, _) => block.iter().all(|stmt| stmt.is_pure()),
            Self::Continue(_) | Self::Break(_) | Self::Return(_, _, _) => false,
            Self::TryCatch(x, _, _) => {
                x.0.statements.iter().all(Stmt::is_pure) && x.2.statements.iter().all(Stmt::is_pure)
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
    /// Only variable declarations (i.e. `let` and `const`) and `import`/`export` statements
    /// are internally pure.
    #[inline(always)]
    pub fn is_internally_pure(&self) -> bool {
        match self {
            Self::Let(expr, _, _, _) | Self::Const(expr, _, _, _) => expr.is_pure(),

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
    #[inline(always)]
    pub fn is_control_flow_break(&self) -> bool {
        match self {
            Self::Return(_, _, _) | Self::Break(_) | Self::Continue(_) => true,
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
        path.push(self.into());

        if !on_node(path) {
            return false;
        }

        match self {
            Self::Let(e, _, _, _) | Self::Const(e, _, _, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
            }
            Self::If(e, x, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in &x.0.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in &x.1.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Switch(e, x, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in x.0.values().flat_map(|block| block.statements.iter()) {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in &x.1.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::While(e, s, _) | Self::Do(s, e, _, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in &s.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::For(e, x, _) => {
                if !e.walk(path, on_node) {
                    return false;
                }
                for s in &x.1.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Assignment(x, _) => {
                if !x.0.walk(path, on_node) {
                    return false;
                }
                if !x.1.walk(path, on_node) {
                    return false;
                }
            }
            Self::Block(x, _) => {
                for s in x {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::TryCatch(x, _, _) => {
                for s in &x.0.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
                for s in &x.2.statements {
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

        path.pop().unwrap();

        true
    }
}

/// _(INTERNALS)_ A custom syntax expression.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Hash)]
pub struct CustomExpr {
    /// List of keywords.
    pub keywords: StaticVec<Expr>,
    /// List of tokens actually parsed.
    pub tokens: Vec<ImmutableString>,
    /// Delta number of variables in the scope.
    pub scope_delta: isize,
}

/// _(INTERNALS)_ A binary expression.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Hash)]
pub struct BinaryExpr {
    /// LHS expression.
    pub lhs: Expr,
    /// RHS expression.
    pub rhs: Expr,
}

/// _(INTERNALS)_ An op-assignment operator.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct OpAssignment {
    pub hash_op_assign: u64,
    pub hash_op: u64,
    pub op: Cow<'static, str>,
}

/// _(INTERNALS)_ An set of function call hashes.
/// Exported under the `internals` feature only.
///
/// Two separate hashes are pre-calculated because of the following pattern:
///
/// ```rust,no_run
/// func(a, b, c);      // Native: func(a, b, c) - 3 parameters
///                     // Script: func(a, b, c) - 3 parameters
///
/// a.func(b, c);       // Native: func(&mut a, b, c) - 3 parameters
///                     // Script: func(b, c) - 2 parameters
/// ```
///
/// For normal function calls, the native hash equals the script hash.
/// For method-style calls, the script hash contains one fewer parameter.
///
/// Function call hashes are used in the following manner:
///
/// * First, the script hash is tried, which contains only the called function's name plus the
///   of parameters.
///
/// * Next, the actual types of arguments are hashed and _combined_ with the native hash, which is
///   then used to search for a native function.
///   In other words, a native function call hash always contains the called function's name plus
///   the types of the arguments.  This is to due to possible function overloading for different parameter types.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct FnCallHash {
    /// Pre-calculated hash for a script-defined function ([`None`] if native functions only).
    pub script: Option<u64>,
    /// Pre-calculated hash for a native Rust function with no parameter types.
    pub native: u64,
}

impl fmt::Debug for FnCallHash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(script) = self.script {
            if script == self.native {
                write!(f, "({}=={})", script, self.native)
            } else {
                write!(f, "({}, {})", script, self.native)
            }
        } else {
            write!(f, "{}", self.native)
        }
    }
}

impl FnCallHash {
    /// Create a [`FnCallHash`] with only the native Rust hash.
    #[inline(always)]
    pub fn from_native(hash: u64) -> Self {
        Self {
            script: None,
            native: hash,
        }
    }
    /// Create a [`FnCallHash`] with both native Rust and script function hashes set to the same value.
    #[inline(always)]
    pub fn from_script(hash: u64) -> Self {
        Self {
            script: Some(hash),
            native: hash,
        }
    }
    /// Create a [`FnCallHash`] with both native Rust and script function hashes.
    #[inline(always)]
    pub fn from_script_and_native(script: u64, native: u64) -> Self {
        Self {
            script: Some(script),
            native,
        }
    }
    /// Is this [`FnCallHash`] native Rust only?
    #[inline(always)]
    pub fn is_native_only(&self) -> bool {
        self.script.is_none()
    }
    /// Get the script function hash from this [`FnCallHash`].
    ///
    /// # Panics
    ///
    /// Panics if the [`FnCallHash`] is native Rust only.
    #[inline(always)]
    pub fn script_hash(&self) -> u64 {
        self.script.unwrap()
    }
    /// Get the naive Rust function hash from this [`FnCallHash`].
    #[inline(always)]
    pub fn native_hash(&self) -> u64 {
        self.native
    }
}

/// _(INTERNALS)_ A function call.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Default, Hash)]
pub struct FnCallExpr {
    /// Pre-calculated hash.
    pub hash: FnCallHash,
    /// Does this function call capture the parent scope?
    pub capture: bool,
    /// List of function call arguments.
    pub args: StaticVec<Expr>,
    /// Namespace of the function, if any. Boxed because it occurs rarely.
    pub namespace: Option<NamespaceRef>,
    /// Function name.
    /// Use [`Cow<'static, str>`][Cow] because a lot of operators (e.g. `==`, `>=`) are implemented as
    /// function calls and the function names are predictable, so no need to allocate a new [`String`].
    pub name: Cow<'static, str>,
}

/// A type that wraps a [`FLOAT`] and implements [`Hash`].
#[cfg(not(feature = "no_float"))]
#[derive(Clone, Copy, PartialEq, PartialOrd)]
pub struct FloatWrapper(FLOAT);

#[cfg(not(feature = "no_float"))]
impl Hash for FloatWrapper {
    #[inline(always)]
    fn hash<H: crate::stdlib::hash::Hasher>(&self, state: &mut H) {
        self.0.to_ne_bytes().hash(state);
    }
}

#[cfg(not(feature = "no_float"))]
impl AsRef<FLOAT> for FloatWrapper {
    #[inline(always)]
    fn as_ref(&self) -> &FLOAT {
        &self.0
    }
}

#[cfg(not(feature = "no_float"))]
impl AsMut<FLOAT> for FloatWrapper {
    #[inline(always)]
    fn as_mut(&mut self) -> &mut FLOAT {
        &mut self.0
    }
}

#[cfg(not(feature = "no_float"))]
impl crate::stdlib::ops::Deref for FloatWrapper {
    type Target = FLOAT;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(not(feature = "no_float"))]
impl crate::stdlib::ops::DerefMut for FloatWrapper {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(not(feature = "no_float"))]
impl fmt::Debug for FloatWrapper {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[cfg(not(feature = "no_float"))]
impl fmt::Display for FloatWrapper {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(feature = "no_std")]
        use num_traits::Float;

        let abs = self.0.abs();
        if abs > 10000000000000.0 || abs < 0.0000000000001 {
            write!(f, "{:e}", self.0)
        } else {
            self.0.fmt(f)
        }
    }
}

#[cfg(not(feature = "no_float"))]
impl From<FLOAT> for FloatWrapper {
    #[inline(always)]
    fn from(value: FLOAT) -> Self {
        Self::new(value)
    }
}

#[cfg(not(feature = "no_float"))]
impl FromStr for FloatWrapper {
    type Err = <FLOAT as FromStr>::Err;

    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        FLOAT::from_str(s).map(Into::<Self>::into)
    }
}

#[cfg(not(feature = "no_float"))]
impl FloatWrapper {
    #[inline(always)]
    pub const fn new(value: FLOAT) -> Self {
        Self(value)
    }
}

/// _(INTERNALS)_ An expression sub-tree.
/// Exported under the `internals` feature only.
///
/// # Volatile Data Structure
///
/// This type is volatile and may change.
#[derive(Debug, Clone, Hash)]
pub enum Expr {
    /// Dynamic constant.
    /// Used to hold either an [`Array`] or [`Map`] literal for quick cloning.
    /// All other primitive data types should use the appropriate variants for better speed.
    DynamicConstant(Box<Dynamic>, Position),
    /// Boolean constant.
    BoolConstant(bool, Position),
    /// Integer constant.
    IntegerConstant(INT, Position),
    /// Floating-point constant.
    #[cfg(not(feature = "no_float"))]
    FloatConstant(FloatWrapper, Position),
    /// Character constant.
    CharConstant(char, Position),
    /// [String][ImmutableString] constant.
    StringConstant(ImmutableString, Position),
    /// [`FnPtr`] constant.
    FnPointer(ImmutableString, Position),
    /// [ expr, ... ]
    Array(Box<StaticVec<Expr>>, Position),
    /// #{ name:expr, ... }
    Map(Box<StaticVec<(Ident, Expr)>>, Position),
    /// ()
    Unit(Position),
    /// Variable access - (optional index, optional (hash, modules), variable name)
    Variable(Box<(Option<NonZeroUsize>, Option<(u64, NamespaceRef)>, Ident)>),
    /// Property access - ((getter, hash), (setter, hash), prop)
    Property(Box<((ImmutableString, u64), (ImmutableString, u64), Ident)>),
    /// { [statement][Stmt] ... }
    Stmt(Box<StmtBlock>),
    /// func `(` expr `,` ... `)`
    FnCall(Box<FnCallExpr>, Position),
    /// lhs `.` rhs
    Dot(Box<BinaryExpr>, Position),
    /// expr `[` expr `]`
    Index(Box<BinaryExpr>, Position),
    /// lhs `&&` rhs
    And(Box<BinaryExpr>, Position),
    /// lhs `||` rhs
    Or(Box<BinaryExpr>, Position),
    /// Custom syntax
    Custom(Box<CustomExpr>, Position),
}

impl Default for Expr {
    #[inline(always)]
    fn default() -> Self {
        Self::Unit(Position::NONE)
    }
}

impl Expr {
    /// Get the [`Dynamic`] value of a constant expression.
    ///
    /// Returns [`None`] if the expression is not constant.
    #[inline]
    pub fn get_constant_value(&self) -> Option<Dynamic> {
        Some(match self {
            Self::DynamicConstant(x, _) => x.as_ref().clone(),
            Self::IntegerConstant(x, _) => (*x).into(),
            #[cfg(not(feature = "no_float"))]
            Self::FloatConstant(x, _) => (*x).into(),
            Self::CharConstant(x, _) => (*x).into(),
            Self::StringConstant(x, _) => x.clone().into(),
            Self::FnPointer(x, _) => Dynamic(Union::FnPtr(
                Box::new(FnPtr::new_unchecked(x.clone(), Default::default())),
                AccessMode::ReadOnly,
            )),
            Self::BoolConstant(x, _) => (*x).into(),
            Self::Unit(_) => Dynamic::UNIT,

            #[cfg(not(feature = "no_index"))]
            Self::Array(x, _) if self.is_constant() => {
                let mut arr = Array::with_capacity(crate::stdlib::cmp::max(
                    crate::engine::TYPICAL_ARRAY_SIZE,
                    x.len(),
                ));
                arr.extend(x.iter().map(|v| v.get_constant_value().unwrap()));
                Dynamic(Union::Array(Box::new(arr), AccessMode::ReadOnly))
            }

            #[cfg(not(feature = "no_object"))]
            Self::Map(x, _) if self.is_constant() => {
                let mut map = Map::with_capacity(crate::stdlib::cmp::max(
                    crate::engine::TYPICAL_MAP_SIZE,
                    x.len(),
                ));
                map.extend(
                    x.iter()
                        .map(|(k, v)| (k.name.clone(), v.get_constant_value().unwrap())),
                );
                Dynamic(Union::Map(Box::new(map), AccessMode::ReadOnly))
            }

            _ => return None,
        })
    }
    /// Return the variable name if the expression a simple variable access.
    #[inline(always)]
    pub(crate) fn get_variable_access(&self, non_qualified: bool) -> Option<&str> {
        match self {
            Self::Variable(x) if !non_qualified || x.1.is_none() => Some((x.2).name.as_str()),
            _ => None,
        }
    }
    /// Get the [position][Position] of the expression.
    #[inline]
    pub fn position(&self) -> Position {
        match self {
            #[cfg(not(feature = "no_float"))]
            Self::FloatConstant(_, pos) => *pos,

            Self::DynamicConstant(_, pos) => *pos,
            Self::BoolConstant(_, pos) => *pos,
            Self::IntegerConstant(_, pos) => *pos,
            Self::CharConstant(_, pos) => *pos,
            Self::StringConstant(_, pos) => *pos,
            Self::FnPointer(_, pos) => *pos,
            Self::Array(_, pos) => *pos,
            Self::Map(_, pos) => *pos,
            Self::Property(x) => (x.2).pos,
            Self::Stmt(x) => x.pos,
            Self::Variable(x) => (x.2).pos,
            Self::FnCall(_, pos) => *pos,

            Self::And(x, _) | Self::Or(x, _) => x.lhs.position(),

            Self::Unit(pos) => *pos,

            Self::Dot(x, _) | Self::Index(x, _) => x.lhs.position(),

            Self::Custom(_, pos) => *pos,
        }
    }
    /// Override the [position][Position] of the expression.
    #[inline]
    pub fn set_position(&mut self, new_pos: Position) -> &mut Self {
        match self {
            #[cfg(not(feature = "no_float"))]
            Self::FloatConstant(_, pos) => *pos = new_pos,

            Self::DynamicConstant(_, pos) => *pos = new_pos,
            Self::BoolConstant(_, pos) => *pos = new_pos,
            Self::IntegerConstant(_, pos) => *pos = new_pos,
            Self::CharConstant(_, pos) => *pos = new_pos,
            Self::StringConstant(_, pos) => *pos = new_pos,
            Self::FnPointer(_, pos) => *pos = new_pos,
            Self::Array(_, pos) => *pos = new_pos,
            Self::Map(_, pos) => *pos = new_pos,
            Self::Variable(x) => (x.2).pos = new_pos,
            Self::Property(x) => (x.2).pos = new_pos,
            Self::Stmt(x) => x.pos = new_pos,
            Self::FnCall(_, pos) => *pos = new_pos,
            Self::And(_, pos) | Self::Or(_, pos) => *pos = new_pos,
            Self::Unit(pos) => *pos = new_pos,
            Self::Dot(_, pos) | Self::Index(_, pos) => *pos = new_pos,
            Self::Custom(_, pos) => *pos = new_pos,
        }

        self
    }
    /// Is the expression pure?
    ///
    /// A pure expression has no side effects.
    #[inline]
    pub fn is_pure(&self) -> bool {
        match self {
            Self::Array(x, _) => x.iter().all(Self::is_pure),

            Self::Map(x, _) => x.iter().map(|(_, v)| v).all(Self::is_pure),

            Self::Index(x, _) | Self::And(x, _) | Self::Or(x, _) => {
                x.lhs.is_pure() && x.rhs.is_pure()
            }

            Self::Stmt(x) => x.statements.iter().all(Stmt::is_pure),

            Self::Variable(_) => true,

            _ => self.is_constant(),
        }
    }
    /// Is the expression the unit `()` literal?
    #[inline(always)]
    pub fn is_unit(&self) -> bool {
        match self {
            Self::Unit(_) => true,
            _ => false,
        }
    }
    /// Is the expression a constant?
    #[inline]
    pub fn is_constant(&self) -> bool {
        match self {
            #[cfg(not(feature = "no_float"))]
            Self::FloatConstant(_, _) => true,

            Self::DynamicConstant(_, _)
            | Self::BoolConstant(_, _)
            | Self::IntegerConstant(_, _)
            | Self::CharConstant(_, _)
            | Self::StringConstant(_, _)
            | Self::FnPointer(_, _)
            | Self::Unit(_) => true,

            // An array literal is constant if all items are constant
            Self::Array(x, _) => x.iter().all(Self::is_constant),

            // An map literal is constant if all items are constant
            Self::Map(x, _) => x.iter().map(|(_, expr)| expr).all(Self::is_constant),

            _ => false,
        }
    }
    /// Is a particular [token][Token] allowed as a postfix operator to this expression?
    #[inline]
    pub fn is_valid_postfix(&self, token: &Token) -> bool {
        match token {
            #[cfg(not(feature = "no_object"))]
            Token::Period => return true,
            _ => (),
        }

        match self {
            #[cfg(not(feature = "no_float"))]
            Self::FloatConstant(_, _) => false,

            Self::DynamicConstant(_, _)
            | Self::BoolConstant(_, _)
            | Self::IntegerConstant(_, _)
            | Self::CharConstant(_, _)
            | Self::FnPointer(_, _)
            | Self::And(_, _)
            | Self::Or(_, _)
            | Self::Unit(_) => false,

            Self::StringConstant(_, _)
            | Self::FnCall(_, _)
            | Self::Stmt(_)
            | Self::Dot(_, _)
            | Self::Index(_, _)
            | Self::Array(_, _)
            | Self::Map(_, _) => match token {
                #[cfg(not(feature = "no_index"))]
                Token::LeftBracket => true,
                _ => false,
            },

            Self::Variable(_) => match token {
                #[cfg(not(feature = "no_index"))]
                Token::LeftBracket => true,
                Token::LeftParen => true,
                Token::Bang => true,
                Token::DoubleColon => true,
                _ => false,
            },

            Self::Property(_) => match token {
                #[cfg(not(feature = "no_index"))]
                Token::LeftBracket => true,
                Token::LeftParen => true,
                _ => false,
            },

            Self::Custom(_, _) => false,
        }
    }
    /// Recursively walk this expression.
    /// Return `false` from the callback to terminate the walk.
    pub fn walk<'a>(
        &'a self,
        path: &mut Vec<ASTNode<'a>>,
        on_node: &mut impl FnMut(&[ASTNode]) -> bool,
    ) -> bool {
        path.push(self.into());

        if !on_node(path) {
            return false;
        }

        match self {
            Self::Stmt(x) => {
                for s in &x.statements {
                    if !s.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Array(x, _) => {
                for e in x.as_ref() {
                    if !e.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Map(x, _) => {
                for (_, e) in x.as_ref() {
                    if !e.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Index(x, _) | Self::Dot(x, _) | Expr::And(x, _) | Expr::Or(x, _) => {
                if !x.lhs.walk(path, on_node) {
                    return false;
                }
                if !x.rhs.walk(path, on_node) {
                    return false;
                }
            }
            Self::FnCall(x, _) => {
                for e in &x.args {
                    if !e.walk(path, on_node) {
                        return false;
                    }
                }
            }
            Self::Custom(x, _) => {
                for e in &x.keywords {
                    if !e.walk(path, on_node) {
                        return false;
                    }
                }
            }
            _ => (),
        }

        path.pop().unwrap();

        true
    }
}

#[cfg(test)]
mod tests {
    /// This test is to make sure no code changes increase the sizes of critical data structures.
    #[test]
    fn check_struct_sizes() {
        use crate::stdlib::mem::size_of;
        use crate::*;

        assert_eq!(size_of::<Dynamic>(), 16);
        assert_eq!(size_of::<Option<Dynamic>>(), 16);
        assert_eq!(size_of::<Position>(), 4);
        assert_eq!(size_of::<ast::Expr>(), 16);
        assert_eq!(size_of::<Option<ast::Expr>>(), 16);
        assert_eq!(size_of::<ast::Stmt>(), 40);
        assert_eq!(size_of::<Option<ast::Stmt>>(), 40);
        assert_eq!(size_of::<FnPtr>(), 32);
        assert_eq!(size_of::<Scope>(), 288);
        assert_eq!(size_of::<LexError>(), 56);
        assert_eq!(size_of::<ParseError>(), 16);
        assert_eq!(size_of::<EvalAltResult>(), 72);
    }
}
