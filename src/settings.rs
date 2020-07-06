use crate::engine::Engine;
use crate::module::ModuleResolver;
use crate::optimize::OptimizationLevel;
use crate::packages::PackageLibrary;
use crate::token::is_valid_identifier;

impl Engine {
    /// Load a new package into the `Engine`.
    ///
    /// When searching for functions, packages loaded later are preferred.
    /// In other words, loaded packages are searched in reverse order.
    pub fn load_package(&mut self, package: PackageLibrary) {
        // Push the package to the top - packages are searched in reverse order
        self.packages.push(package);
    }

    /// Load a new package into the `Engine`.
    ///
    /// When searching for functions, packages loaded later are preferred.
    /// In other words, loaded packages are searched in reverse order.
    pub fn load_packages(&mut self, package: PackageLibrary) {
        // Push the package to the top - packages are searched in reverse order
        self.packages.push(package);
    }

    /// Control whether and how the `Engine` will optimize an AST after compilation.
    ///
    /// Not available under the `no_optimize` feature.
    #[cfg(not(feature = "no_optimize"))]
    pub fn set_optimization_level(&mut self, optimization_level: OptimizationLevel) {
        self.optimization_level = optimization_level
    }

    /// The current optimization level.
    /// It controls whether and how the `Engine` will optimize an AST after compilation.
    ///
    /// Not available under the `no_optimize` feature.
    #[cfg(not(feature = "no_optimize"))]
    pub fn optimization_level(&self) -> OptimizationLevel {
        self.optimization_level
    }

    /// Set the maximum levels of function calls allowed for a script in order to avoid
    /// infinite recursion and stack overflows.
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_call_levels(&mut self, levels: usize) {
        self.max_call_stack_depth = levels
    }

    /// The maximum levels of function calls allowed for a script.
    #[cfg(not(feature = "unchecked"))]
    pub fn max_call_levels(&self) -> usize {
        self.max_call_stack_depth
    }

    /// Set the maximum number of operations allowed for a script to run to avoid
    /// consuming too much resources (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_operations(&mut self, operations: u64) {
        self.max_operations = if operations == u64::MAX {
            0
        } else {
            operations
        };
    }

    /// The maximum number of operations allowed for a script to run (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn max_operations(&self) -> u64 {
        self.max_operations
    }

    /// Set the maximum number of imported modules allowed for a script.
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_modules(&mut self, modules: usize) {
        self.max_modules = modules;
    }

    /// The maximum number of imported modules allowed for a script.
    #[cfg(not(feature = "unchecked"))]
    pub fn max_modules(&self) -> usize {
        self.max_modules
    }

    /// Set the depth limits for expressions (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_expr_depths(&mut self, max_expr_depth: usize, max_function_expr_depth: usize) {
        self.max_expr_depth = if max_expr_depth == usize::MAX {
            0
        } else {
            max_expr_depth
        };
        self.max_function_expr_depth = if max_function_expr_depth == usize::MAX {
            0
        } else {
            max_function_expr_depth
        };
    }

    /// The depth limit for expressions (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn max_expr_depth(&self) -> usize {
        self.max_expr_depth
    }

    /// The depth limit for expressions in functions (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn max_function_expr_depth(&self) -> usize {
        self.max_function_expr_depth
    }

    /// Set the maximum length of strings (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn set_max_string_size(&mut self, max_size: usize) {
        self.max_string_size = if max_size == usize::MAX { 0 } else { max_size };
    }

    /// The maximum length of strings (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    pub fn max_string_size(&self) -> usize {
        self.max_string_size
    }

    /// Set the maximum length of arrays (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    #[cfg(not(feature = "no_index"))]
    pub fn set_max_array_size(&mut self, max_size: usize) {
        self.max_array_size = if max_size == usize::MAX { 0 } else { max_size };
    }

    /// The maximum length of arrays (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    #[cfg(not(feature = "no_index"))]
    pub fn max_array_size(&self) -> usize {
        self.max_array_size
    }

    /// Set the maximum length of object maps (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    #[cfg(not(feature = "no_object"))]
    pub fn set_max_map_size(&mut self, max_size: usize) {
        self.max_map_size = if max_size == usize::MAX { 0 } else { max_size };
    }

    /// The maximum length of object maps (0 for unlimited).
    #[cfg(not(feature = "unchecked"))]
    #[cfg(not(feature = "no_object"))]
    pub fn max_map_size(&self) -> usize {
        self.max_map_size
    }

    /// Set the module resolution service used by the `Engine`.
    ///
    /// Not available under the `no_module` feature.
    #[cfg(not(feature = "no_module"))]
    pub fn set_module_resolver(&mut self, resolver: Option<impl ModuleResolver + 'static>) {
        self.module_resolver = resolver.map(|f| Box::new(f) as Box<dyn ModuleResolver>);
    }

    /// Disable a particular keyword or operator in the language.
    ///
    /// # Examples
    ///
    /// The following will raise an error during parsing because the `if` keyword is disabled
    /// and is recognized as a variable name!
    ///
    /// ```rust,should_panic
    /// # fn main() -> Result<(), rhai::ParseError> {
    /// use rhai::Engine;
    ///
    /// let mut engine = Engine::new();
    ///
    /// engine.disable_symbol("if");    // disable the 'if' keyword
    ///
    /// engine.compile("let x = if true { 42 } else { 0 };")?;
    /// //                      ^ 'if' is parsed as a variable name
    /// //                         ^ missing ';' after statement end
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// The following will raise an error during parsing because the `+=` operator is disabled.
    ///
    /// ```rust,should_panic
    /// # fn main() -> Result<(), rhai::ParseError> {
    /// use rhai::Engine;
    ///
    /// let mut engine = Engine::new();
    ///
    /// engine.disable_symbol("+=");    // disable the '+=' operator
    ///
    /// engine.compile("let x = 42; x += 1;")?;
    /// //                            ^ unknown operator
    /// # Ok(())
    /// # }
    /// ```
    pub fn disable_symbol(&mut self, symbol: &str) {
        if self.disabled_symbols.is_none() {
            self.disabled_symbols = Some(Default::default());
        }

        self.disabled_symbols
            .as_mut()
            .unwrap()
            .insert(symbol.into());
    }

    /// Register a custom operator into the language.
    ///
    /// The operator must be a valid identifier (i.e. it cannot be a symbol).
    ///
    /// # Examples
    ///
    /// ```rust
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// let mut engine = Engine::new();
    ///
    /// // Register a custom operator called 'foo' and give it
    /// // a precedence of 160 (i.e. between +|- and *|/).
    /// engine.register_custom_operator("foo", 160).unwrap();
    ///
    /// // Register a binary function named 'foo'
    /// engine.register_fn("foo", |x: i64, y: i64| (x * y) - (x + y));
    ///
    /// assert_eq!(
    ///     engine.eval_expression::<i64>("1 + 2 * 3 foo 4 - 5 / 6")?,
    ///     15
    /// );
    /// # Ok(())
    /// # }
    /// ```
    pub fn register_custom_operator(
        &mut self,
        keyword: &str,
        precedence: u8,
    ) -> Result<(), String> {
        if !is_valid_identifier(keyword.chars()) {
            return Err(format!("not a valid identifier: '{}'", keyword).into());
        }

        if self.custom_keywords.is_none() {
            self.custom_keywords = Some(Default::default());
        }

        self.custom_keywords
            .as_mut()
            .unwrap()
            .insert(keyword.into(), precedence);

        Ok(())
    }
}
