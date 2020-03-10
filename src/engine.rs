//! Main module defining the script evaluation `Engine`.

use crate::any::{Any, AnyExt, Dynamic, Variant};
use crate::parser::{Expr, FnDef, Position, Stmt};
use crate::result::EvalAltResult;
use crate::scope::Scope;
use std::{
    any::{type_name, TypeId},
    borrow::Cow,
    cmp::{PartialEq, PartialOrd},
    collections::HashMap,
    iter::once,
    sync::Arc,
};

/// An dynamic array of `Dynamic` values.
pub type Array = Vec<Dynamic>;

pub type FnCallArgs<'a> = Vec<&'a mut Variant>;

pub type FnAny = dyn Fn(FnCallArgs, Position) -> Result<Dynamic, EvalAltResult>;

type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

pub(crate) const KEYWORD_PRINT: &'static str = "print";
pub(crate) const KEYWORD_DEBUG: &'static str = "debug";
pub(crate) const KEYWORD_TYPE_OF: &'static str = "type_of";
pub(crate) const FUNC_GETTER: &'static str = "get$";
pub(crate) const FUNC_SETTER: &'static str = "set$";

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
enum IndexSourceType {
    Array,
    String,
    Expression,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FnSpec<'a> {
    pub name: Cow<'a, str>,
    pub args: Option<Vec<TypeId>>,
}

/// Rhai main scripting engine.
///
/// ```rust
/// # fn main() -> Result<(), rhai::EvalAltResult> {
/// use rhai::Engine;
///
/// let mut engine = Engine::new();
///
/// let result = engine.eval::<i64>("40 + 2")?;
///
/// println!("Answer: {}", result);  // prints 42
/// # Ok(())
/// # }
/// ```
pub struct Engine<'e> {
    /// Optimize the AST after compilation
    pub(crate) optimize: bool,
    /// A hashmap containing all compiled functions known to the engine
    pub(crate) ext_functions: HashMap<FnSpec<'e>, Arc<FnIntExt<'e>>>,
    /// A hashmap containing all script-defined functions
    pub(crate) script_functions: HashMap<FnSpec<'e>, Arc<FnIntExt<'e>>>,
    /// A hashmap containing all iterators known to the engine
    pub(crate) type_iterators: HashMap<TypeId, Arc<IteratorFn>>,
    pub(crate) type_names: HashMap<String, String>,

    // Closures for implementing the print/debug commands
    pub(crate) on_print: Box<dyn FnMut(&str) + 'e>,
    pub(crate) on_debug: Box<dyn FnMut(&str) + 'e>,
}

pub enum FnIntExt<'a> {
    Ext(Box<FnAny>),
    Int(FnDef<'a>),
}

impl Engine<'_> {
    /// Create a new `Engine`
    pub fn new() -> Self {
        // User-friendly names for built-in types
        let type_names = [
            (type_name::<String>(), "string"),
            (type_name::<Array>(), "array"),
            (type_name::<Dynamic>(), "dynamic"),
        ]
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();

        // Create the new scripting Engine
        let mut engine = Engine {
            optimize: true,
            ext_functions: HashMap::new(),
            script_functions: HashMap::new(),
            type_iterators: HashMap::new(),
            type_names,
            on_print: Box::new(default_print), // default print/debug implementations
            on_debug: Box::new(default_print),
        };

        engine.register_core_lib();

        #[cfg(not(feature = "no_stdlib"))]
        engine.register_stdlib(); // Register the standard library when no_stdlib is not set

        engine
    }

    /// Control whether the `Engine` will optimize an AST after compilation
    pub fn set_optimization(&mut self, optimize: bool) {
        self.optimize = optimize
    }

    /// Universal method for calling functions, that are either
    /// registered with the `Engine` or written in Rhai
    pub(crate) fn call_fn_raw(
        &mut self,
        fn_name: &str,
        args: FnCallArgs,
        def_val: Option<&Dynamic>,
        pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        debug_println!(
            "Calling function: {} ({})",
            fn_name,
            args.iter()
                .map(|x| (*x).type_name())
                .map(|name| self.map_type_name(name))
                .collect::<Vec<_>>()
                .join(", ")
        );

        let mut spec = FnSpec {
            name: fn_name.into(),
            args: None,
        };

        // First search in script-defined functions (can override built-in),
        // then built-in's and external functions
        let fn_def = self
            .script_functions
            .get(&spec)
            .or_else(|| {
                spec.args = Some(args.iter().map(|a| Any::type_id(&**a)).collect());
                self.ext_functions.get(&spec)
            })
            .map(|f| f.clone());

        if let Some(f) = fn_def {
            match *f {
                // Run external function
                FnIntExt::Ext(ref func) => {
                    let result = func(args, pos)?;

                    // See if the function match print/debug (which requires special processing)
                    let callback = match spec.name.as_ref() {
                        KEYWORD_PRINT => self.on_print.as_mut(),
                        KEYWORD_DEBUG => self.on_debug.as_mut(),
                        _ => return Ok(result),
                    };

                    let val = &result
                        .downcast::<String>()
                        .map(|s| *s)
                        .unwrap_or("error: not a string".into());

                    Ok(callback(val).into_dynamic())
                }

                // Run script-defined function
                FnIntExt::Int(ref func) => {
                    // First check number of parameters
                    if func.params.len() != args.len() {
                        return Err(EvalAltResult::ErrorFunctionArgsMismatch(
                            spec.name.into(),
                            func.params.len(),
                            args.len(),
                            pos,
                        ));
                    }

                    let mut scope = Scope::new();

                    scope.extend(
                        // Put arguments into scope as variables
                        func.params
                            .iter()
                            .cloned()
                            .zip(args.iter().map(|x| (*x).into_dynamic())),
                    );

                    // Evaluate
                    match self.eval_stmt(&mut scope, &func.body) {
                        // Convert return statement to return value
                        Err(EvalAltResult::Return(x, _)) => Ok(x),
                        other => other,
                    }
                }
            }
        } else if spec.name == KEYWORD_TYPE_OF && args.len() == 1 {
            // Handle `type_of` function
            Ok(self
                .map_type_name(args[0].type_name())
                .to_string()
                .into_dynamic())
        } else if spec.name.starts_with(FUNC_GETTER) {
            // Getter function not found
            Err(EvalAltResult::ErrorDotExpr(
                format!(
                    "- property '{}' unknown or write-only",
                    &spec.name[FUNC_GETTER.len()..]
                ),
                pos,
            ))
        } else if spec.name.starts_with(FUNC_SETTER) {
            // Setter function not found
            Err(EvalAltResult::ErrorDotExpr(
                format!(
                    "- property '{}' unknown or read-only",
                    &spec.name[FUNC_SETTER.len()..]
                ),
                pos,
            ))
        } else if let Some(val) = def_val {
            // Return default value
            Ok(val.clone())
        } else {
            // Raise error
            let types_list = args
                .iter()
                .map(|x| (*x).type_name())
                .map(|name| self.map_type_name(name))
                .collect::<Vec<_>>();

            Err(EvalAltResult::ErrorFunctionNotFound(
                format!("{} ({})", spec.name, types_list.join(", ")),
                pos,
            ))
        }
    }

    /// Chain-evaluate a dot setter
    fn get_dot_val_helper(
        &mut self,
        scope: &mut Scope,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            // xxx.fn_name(args)
            Expr::FunctionCall(fn_name, args, def_val, pos) => {
                let mut args: Array = args
                    .iter()
                    .map(|arg| self.eval_expr(scope, arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let args = once(this_ptr)
                    .chain(args.iter_mut().map(|b| b.as_mut()))
                    .collect();

                self.call_fn_raw(fn_name, args, def_val.as_ref(), *pos)
            }

            // xxx.id
            Expr::Identifier(id, pos) => {
                let get_fn_name = format!("{}{}", FUNC_GETTER, id);

                self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
            }

            // xxx.idx_lhs[idx_expr]
            Expr::Index(idx_lhs, idx_expr, idx_pos) => {
                let (expr, _) = match idx_lhs.as_ref() {
                    // xxx.id[idx_expr]
                    Expr::Identifier(id, pos) => {
                        let get_fn_name = format!("{}{}", FUNC_GETTER, id);
                        (
                            self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)?,
                            *pos,
                        )
                    }
                    // xxx.???[???][idx_expr]
                    Expr::Index(_, _, _) => {
                        (self.get_dot_val_helper(scope, this_ptr, idx_lhs)?, *idx_pos)
                    }
                    // Syntax error
                    _ => {
                        return Err(EvalAltResult::ErrorDotExpr(
                            "".to_string(),
                            dot_rhs.position(),
                        ))
                    }
                };

                let idx = self.eval_index_value(scope, idx_expr)?;
                self.get_indexed_value(expr, idx, idx_expr.position(), *idx_pos)
                    .map(|(v, _)| v)
            }

            // xxx.dot_lhs.rhs
            Expr::Dot(dot_lhs, rhs, _) => match dot_lhs.as_ref() {
                // xxx.id.rhs
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("{}{}", FUNC_GETTER, id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|mut v| self.get_dot_val_helper(scope, v.as_mut(), rhs))
                }
                // xxx.idx_lhs[idx_expr].rhs
                Expr::Index(idx_lhs, idx_expr, idx_pos) => {
                    let (expr, _) = match idx_lhs.as_ref() {
                        // xxx.id[idx_expr].rhs
                        Expr::Identifier(id, pos) => {
                            let get_fn_name = format!("{}{}", FUNC_GETTER, id);
                            (
                                self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)?,
                                *pos,
                            )
                        }
                        // xxx.???[???][idx_expr].rhs
                        Expr::Index(_, _, _) => {
                            (self.get_dot_val_helper(scope, this_ptr, idx_lhs)?, *idx_pos)
                        }
                        // Syntax error
                        _ => {
                            return Err(EvalAltResult::ErrorDotExpr(
                                "".to_string(),
                                dot_rhs.position(),
                            ))
                        }
                    };

                    let idx = self.eval_index_value(scope, idx_expr)?;
                    self.get_indexed_value(expr, idx, idx_expr.position(), *idx_pos)
                        .and_then(|(mut v, _)| self.get_dot_val_helper(scope, v.as_mut(), rhs))
                }
                // Syntax error
                _ => Err(EvalAltResult::ErrorDotExpr(
                    "".to_string(),
                    dot_lhs.position(),
                )),
            },

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(
                "".to_string(),
                dot_rhs.position(),
            )),
        }
    }

    /// Evaluate a dot chain getter
    fn get_dot_val(
        &mut self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            // id.???
            Expr::Identifier(id, pos) => {
                let (src_idx, mut target) = Self::search_scope(scope, id, Ok, *pos)?;
                let val = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                *scope.get_mut(id, src_idx) = target;

                val
            }

            // idx_lhs[idx_expr].???
            Expr::Index(idx_lhs, idx_expr, idx_pos) => {
                let (src_type, src, idx, mut target) =
                    self.eval_index_expr(scope, idx_lhs, idx_expr, *idx_pos)?;
                let val = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                if let Some((id, src_idx)) = src {
                    Self::update_indexed_var_in_scope(
                        src_type,
                        scope,
                        id,
                        src_idx,
                        idx,
                        target,
                        idx_lhs.position(),
                    )?;
                }

                val
            }

            // {expr}.???
            expr => {
                let mut target = self.eval_expr(scope, expr)?;
                self.get_dot_val_helper(scope, target.as_mut(), dot_rhs)
            }
        }
    }

    /// Search for a variable within the scope, returning its value and index inside the Scope
    fn search_scope<T>(
        scope: &Scope,
        id: &str,
        map: impl FnOnce(Dynamic) -> Result<T, EvalAltResult>,
        begin: Position,
    ) -> Result<(usize, T), EvalAltResult> {
        scope
            .get(id)
            .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.into(), begin))
            .and_then(move |(idx, _, val)| map(val).map(|v| (idx, v)))
    }

    /// Evaluate the value of an index (must evaluate to i64)
    fn eval_index_value(
        &mut self,
        scope: &mut Scope,
        idx_expr: &Expr,
    ) -> Result<i64, EvalAltResult> {
        self.eval_expr(scope, idx_expr)?
            .downcast::<i64>()
            .map(|v| *v)
            .map_err(|_| EvalAltResult::ErrorIndexExpr(idx_expr.position()))
    }

    /// Get the value at the indexed position of a base type
    fn get_indexed_value(
        &self,
        val: Dynamic,
        idx: i64,
        val_pos: Position,
        idx_pos: Position,
    ) -> Result<(Dynamic, IndexSourceType), EvalAltResult> {
        if val.is::<Array>() {
            // val_array[idx]
            let arr = val.downcast::<Array>().expect("array expected");

            if idx >= 0 {
                arr.get(idx as usize)
                    .cloned()
                    .map(|v| (v, IndexSourceType::Array))
                    .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx, val_pos))
            } else {
                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, val_pos))
            }
        } else if val.is::<String>() {
            // val_string[idx]
            let s = val.downcast::<String>().expect("string expected");

            if idx >= 0 {
                s.chars()
                    .nth(idx as usize)
                    .map(|ch| (ch.into_dynamic(), IndexSourceType::String))
                    .ok_or_else(|| {
                        EvalAltResult::ErrorStringBounds(s.chars().count(), idx, val_pos)
                    })
            } else {
                Err(EvalAltResult::ErrorStringBounds(
                    s.chars().count(),
                    idx,
                    val_pos,
                ))
            }
        } else {
            // Error - cannot be indexed
            Err(EvalAltResult::ErrorIndexingType(
                self.map_type_name(val.type_name()).to_string(),
                idx_pos,
            ))
        }
    }

    /// Evaluate an index expression
    fn eval_index_expr<'a>(
        &mut self,
        scope: &mut Scope,
        lhs: &'a Expr,
        idx_expr: &Expr,
        idx_pos: Position,
    ) -> Result<(IndexSourceType, Option<(&'a str, usize)>, usize, Dynamic), EvalAltResult> {
        let idx = self.eval_index_value(scope, idx_expr)?;

        match lhs {
            // id[idx_expr]
            Expr::Identifier(id, _) => Self::search_scope(
                scope,
                &id,
                |val| self.get_indexed_value(val, idx, idx_expr.position(), idx_pos),
                lhs.position(),
            )
            .map(|(src_idx, (val, src_type))| {
                (src_type, Some((id.as_str(), src_idx)), idx as usize, val)
            }),

            // (expr)[idx_expr]
            expr => {
                let val = self.eval_expr(scope, expr)?;

                self.get_indexed_value(val, idx, idx_expr.position(), idx_pos)
                    .map(|(v, _)| (IndexSourceType::Expression, None, idx as usize, v))
            }
        }
    }

    /// Replace a character at an index position in a mutable string
    fn str_replace_char(s: &mut String, idx: usize, new_ch: char) {
        let mut chars: Vec<char> = s.chars().collect();
        let ch = *chars.get(idx).expect("string index out of bounds");

        // See if changed - if so, update the String
        if ch != new_ch {
            chars[idx] = new_ch;
            s.clear();
            chars.iter().for_each(|&ch| s.push(ch));
        }
    }

    /// Update the value at an index position in a variable inside the scope
    fn update_indexed_var_in_scope(
        src_type: IndexSourceType,
        scope: &mut Scope,
        id: &str,
        src_idx: usize,
        idx: usize,
        new_val: Dynamic,
        val_pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        match src_type {
            // array_id[idx] = val
            IndexSourceType::Array => {
                let arr = scope.get_mut_by_type::<Array>(id, src_idx);
                Ok((arr[idx as usize] = new_val).into_dynamic())
            }

            // string_id[idx] = val
            IndexSourceType::String => {
                let s = scope.get_mut_by_type::<String>(id, src_idx);
                // Value must be a character
                let ch = *new_val
                    .downcast::<char>()
                    .map_err(|_| EvalAltResult::ErrorCharMismatch(val_pos))?;
                Ok(Self::str_replace_char(s, idx as usize, ch).into_dynamic())
            }

            // All other variable types should be an error
            _ => panic!("array or string source type expected for indexing"),
        }
    }

    /// Update the value at an index position
    fn update_indexed_value(
        mut target: Dynamic,
        idx: usize,
        new_val: Dynamic,
        pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        if target.is::<Array>() {
            let arr = target.downcast_mut::<Array>().expect("array expected");
            arr[idx as usize] = new_val;
        } else if target.is::<String>() {
            let s = target.downcast_mut::<String>().expect("string expected");
            // Value must be a character
            let ch = *new_val
                .downcast::<char>()
                .map_err(|_| EvalAltResult::ErrorCharMismatch(pos))?;
            Self::str_replace_char(s, idx as usize, ch);
        } else {
            // All other variable types should be an error
            panic!("array or string source type expected for indexing")
        }

        Ok(target)
    }

    /// Chain-evaluate a dot setter
    fn set_dot_val_helper(
        &mut self,
        scope: &mut Scope,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
        mut new_val: Dynamic,
        val_pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            // xxx.id
            Expr::Identifier(id, pos) => {
                let set_fn_name = format!("{}{}", FUNC_SETTER, id);

                self.call_fn_raw(&set_fn_name, vec![this_ptr, new_val.as_mut()], None, *pos)
            }

            // xxx.lhs[idx_expr]
            // TODO - Allow chaining of indexing!
            Expr::Index(lhs, idx_expr, idx_pos) => match lhs.as_ref() {
                // xxx.id[idx_expr]
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("{}{}", FUNC_GETTER, id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|v| {
                            let idx = self.eval_index_value(scope, idx_expr)?;
                            Self::update_indexed_value(v, idx as usize, new_val, val_pos)
                        })
                        .and_then(|mut v| {
                            let set_fn_name = format!("{}{}", FUNC_SETTER, id);
                            self.call_fn_raw(&set_fn_name, vec![this_ptr, v.as_mut()], None, *pos)
                        })
                }

                // All others - syntax error for setters chain
                _ => Err(EvalAltResult::ErrorDotExpr(
                    "for assignment".to_string(),
                    *idx_pos,
                )),
            },

            // xxx.lhs.{...}
            Expr::Dot(lhs, rhs, _) => match lhs.as_ref() {
                // xxx.id.rhs
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("{}{}", FUNC_GETTER, id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|mut v| {
                            self.set_dot_val_helper(scope, v.as_mut(), rhs, new_val, val_pos)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = format!("{}{}", FUNC_SETTER, id);

                            self.call_fn_raw(&set_fn_name, vec![this_ptr, v.as_mut()], None, *pos)
                        })
                }

                // xxx.lhs[idx_expr].rhs
                // TODO - Allow chaining of indexing!
                Expr::Index(lhs, idx_expr, idx_pos) => match lhs.as_ref() {
                    // xxx.id[idx_expr].rhs
                    Expr::Identifier(id, pos) => {
                        let get_fn_name = format!("{}{}", FUNC_GETTER, id);

                        self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                            .and_then(|v| {
                                let idx = self.eval_index_value(scope, idx_expr)?;
                                let (mut target, _) = self.get_indexed_value(
                                    v.clone(), // TODO - Avoid cloning this
                                    idx,
                                    idx_expr.position(),
                                    *idx_pos,
                                )?;

                                self.set_dot_val_helper(
                                    scope,
                                    target.as_mut(),
                                    rhs,
                                    new_val,
                                    val_pos,
                                )?;

                                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                                Self::update_indexed_value(v, idx as usize, target, val_pos)
                            })
                            .and_then(|mut v| {
                                let set_fn_name = format!("{}{}", FUNC_SETTER, id);

                                self.call_fn_raw(
                                    &set_fn_name,
                                    vec![this_ptr, v.as_mut()],
                                    None,
                                    *pos,
                                )
                            })
                    }

                    // All others - syntax error for setters chain
                    _ => Err(EvalAltResult::ErrorDotExpr(
                        "for assignment".to_string(),
                        *idx_pos,
                    )),
                },

                // All others - syntax error for setters chain
                _ => Err(EvalAltResult::ErrorDotExpr(
                    "for assignment".to_string(),
                    lhs.position(),
                )),
            },

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(
                "for assignment".to_string(),
                dot_rhs.position(),
            )),
        }
    }

    // Evaluate a dot chain setter
    fn set_dot_val(
        &mut self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        new_val: Dynamic,
        val_pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            // id.???
            Expr::Identifier(id, pos) => {
                let (src_idx, mut target) = Self::search_scope(scope, id, Ok, *pos)?;
                let val =
                    self.set_dot_val_helper(scope, target.as_mut(), dot_rhs, new_val, val_pos);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                *scope.get_mut(id, src_idx) = target;

                val
            }

            // lhs[idx_expr].???
            // TODO - Allow chaining of indexing!
            Expr::Index(lhs, idx_expr, idx_pos) => {
                let (src_type, src, idx, mut target) =
                    self.eval_index_expr(scope, lhs, idx_expr, *idx_pos)?;
                let val =
                    self.set_dot_val_helper(scope, target.as_mut(), dot_rhs, new_val, val_pos);

                // In case the expression mutated `target`, we need to update it back into the scope because it is cloned.
                if let Some((id, src_idx)) = src {
                    Self::update_indexed_var_in_scope(
                        src_type,
                        scope,
                        id,
                        src_idx,
                        idx,
                        target,
                        lhs.position(),
                    )?;
                }

                val
            }

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(
                "for assignment".to_string(),
                dot_lhs.position(),
            )),
        }
    }

    /// Evaluate an expression
    fn eval_expr(&mut self, scope: &mut Scope, expr: &Expr) -> Result<Dynamic, EvalAltResult> {
        match expr {
            Expr::IntegerConstant(i, _) => Ok(i.into_dynamic()),
            Expr::FloatConstant(f, _) => Ok(f.into_dynamic()),
            Expr::StringConstant(s, _) => Ok(s.into_dynamic()),
            Expr::CharConstant(c, _) => Ok(c.into_dynamic()),
            Expr::Identifier(id, pos) => {
                Self::search_scope(scope, id, Ok, *pos).map(|(_, val)| val)
            }

            // lhs[idx_expr]
            Expr::Index(lhs, idx_expr, idx_pos) => self
                .eval_index_expr(scope, lhs, idx_expr, *idx_pos)
                .map(|(_, _, _, x)| x),

            // Statement block
            Expr::Stmt(stmt, _) => self.eval_stmt(scope, stmt),

            // lhs = rhs
            Expr::Assignment(lhs, rhs, _) => {
                let rhs_val = self.eval_expr(scope, rhs)?;

                match lhs.as_ref() {
                    // name = rhs
                    Expr::Identifier(name, pos) => {
                        if let Some((idx, _, _)) = scope.get(name) {
                            *scope.get_mut(name, idx) = rhs_val;
                            Ok(().into_dynamic())
                        } else {
                            Err(EvalAltResult::ErrorVariableNotFound(name.clone(), *pos))
                        }
                    }

                    // idx_lhs[idx_expr] = rhs
                    Expr::Index(idx_lhs, idx_expr, idx_pos) => {
                        let (src_type, src, idx, _) =
                            self.eval_index_expr(scope, idx_lhs, idx_expr, *idx_pos)?;

                        if let Some((id, src_idx)) = src {
                            Ok(Self::update_indexed_var_in_scope(
                                src_type,
                                scope,
                                &id,
                                src_idx,
                                idx,
                                rhs_val,
                                rhs.position(),
                            )?)
                        } else {
                            Err(EvalAltResult::ErrorAssignmentToUnknownLHS(
                                idx_lhs.position(),
                            ))
                        }
                    }

                    // dot_lhs.dot_rhs = rhs
                    Expr::Dot(dot_lhs, dot_rhs, _) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val, rhs.position())
                    }

                    // Syntax error
                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS(lhs.position())),
                }
            }

            Expr::Dot(lhs, rhs, _) => self.get_dot_val(scope, lhs, rhs),

            Expr::Array(contents, _) => {
                let mut arr = Vec::new();

                contents
                    .iter()
                    .try_for_each::<_, Result<_, EvalAltResult>>(|item| {
                        let arg = self.eval_expr(scope, item)?;
                        arr.push(arg);
                        Ok(())
                    })?;

                Ok(Box::new(arr))
            }

            Expr::FunctionCall(fn_name, args, def_val, pos) => {
                let mut args = args
                    .iter()
                    .map(|expr| self.eval_expr(scope, expr))
                    .collect::<Result<Array, _>>()?;

                self.call_fn_raw(
                    fn_name,
                    args.iter_mut().map(|b| b.as_mut()).collect(),
                    def_val.as_ref(),
                    *pos,
                )
            }

            Expr::And(lhs, rhs) => Ok(Box::new(
                *self
                    .eval_expr(scope, &*lhs)?
                    .downcast::<bool>()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), lhs.position())
                    })?
                    && // Short-circuit using &&
                *self
                    .eval_expr(scope, &*rhs)?
                    .downcast::<bool>()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("AND".into(), rhs.position())
                    })?,
            )),

            Expr::Or(lhs, rhs) => Ok(Box::new(
                *self
                    .eval_expr(scope, &*lhs)?
                    .downcast::<bool>()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), lhs.position())
                    })?
                    || // Short-circuit using ||
                *self
                    .eval_expr(scope, &*rhs)?
                    .downcast::<bool>()
                    .map_err(|_| {
                        EvalAltResult::ErrorBooleanArgMismatch("OR".into(), rhs.position())
                    })?,
            )),

            Expr::True(_) => Ok(true.into_dynamic()),
            Expr::False(_) => Ok(false.into_dynamic()),
            Expr::Unit(_) => Ok(().into_dynamic()),
        }
    }

    /// Evaluate a statement
    pub(crate) fn eval_stmt(
        &mut self,
        scope: &mut Scope,
        stmt: &Stmt,
    ) -> Result<Dynamic, EvalAltResult> {
        match stmt {
            // No-op
            Stmt::Noop(_) => Ok(().into_dynamic()),

            // Expression as statement
            Stmt::Expr(expr) => self.eval_expr(scope, expr),

            // Block scope
            Stmt::Block(block, _) => {
                let prev_len = scope.len();
                let mut last_result: Result<Dynamic, EvalAltResult> = Ok(().into_dynamic());

                for block_stmt in block.iter() {
                    last_result = self.eval_stmt(scope, block_stmt);

                    if let Err(x) = last_result {
                        last_result = Err(x);
                        break;
                    }
                }

                scope.rewind(prev_len);

                last_result
            }

            // If-else statement
            Stmt::IfElse(guard, body, else_body) => self
                .eval_expr(scope, guard)?
                .downcast::<bool>()
                .map_err(|_| EvalAltResult::ErrorIfGuard(guard.position()))
                .and_then(|guard_val| {
                    if *guard_val {
                        self.eval_stmt(scope, body)
                    } else if let Some(stmt) = else_body {
                        self.eval_stmt(scope, stmt.as_ref())
                    } else {
                        Ok(().into_dynamic())
                    }
                }),

            // While loop
            Stmt::While(guard, body) => loop {
                match self.eval_expr(scope, guard)?.downcast::<bool>() {
                    Ok(guard_val) => {
                        if *guard_val {
                            match self.eval_stmt(scope, body) {
                                Err(EvalAltResult::LoopBreak) => return Ok(().into_dynamic()),
                                Err(x) => return Err(x),
                                _ => (),
                            }
                        } else {
                            return Ok(().into_dynamic());
                        }
                    }
                    Err(_) => return Err(EvalAltResult::ErrorIfGuard(guard.position())),
                }
            },

            // Loop statement
            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, body) {
                    Err(EvalAltResult::LoopBreak) => return Ok(().into_dynamic()),
                    Err(x) => return Err(x),
                    _ => (),
                }
            },

            // For loop
            Stmt::For(name, expr, body) => {
                let arr = self.eval_expr(scope, expr)?;
                let tid = Any::type_id(&*arr);

                if let Some(iter_fn) = self.type_iterators.get(&tid) {
                    scope.push(name.clone(), ());
                    let idx = scope.len() - 1;

                    for a in iter_fn(&arr) {
                        *scope.get_mut(name, idx) = a;

                        match self.eval_stmt(scope, body) {
                            Err(EvalAltResult::LoopBreak) => break,
                            Err(x) => return Err(x),
                            _ => (),
                        }
                    }
                    scope.pop();
                    Ok(().into_dynamic())
                } else {
                    return Err(EvalAltResult::ErrorFor(expr.position()));
                }
            }

            // Break statement
            Stmt::Break(_) => Err(EvalAltResult::LoopBreak),

            // Empty return
            Stmt::ReturnWithVal(None, true, pos) => {
                Err(EvalAltResult::Return(().into_dynamic(), *pos))
            }

            // Return value
            Stmt::ReturnWithVal(Some(a), true, pos) => {
                Err(EvalAltResult::Return(self.eval_expr(scope, a)?, *pos))
            }

            // Empty throw
            Stmt::ReturnWithVal(None, false, pos) => {
                Err(EvalAltResult::ErrorRuntime("".into(), *pos))
            }

            // Throw value
            Stmt::ReturnWithVal(Some(a), false, pos) => {
                let val = self.eval_expr(scope, a)?;
                Err(EvalAltResult::ErrorRuntime(
                    val.downcast::<String>()
                        .map(|s| *s)
                        .unwrap_or("".to_string()),
                    *pos,
                ))
            }

            // Let statement
            Stmt::Let(name, init, _) => {
                if let Some(v) = init {
                    let val = self.eval_expr(scope, v)?;
                    scope.push_dynamic(name.clone(), val);
                } else {
                    scope.push(name.clone(), ());
                }
                Ok(().into_dynamic())
            }
        }
    }

    /// Map a type_name into a pretty-print name
    pub(crate) fn map_type_name<'a>(&'a self, name: &'a str) -> &'a str {
        self.type_names
            .get(name)
            .map(|s| s.as_str())
            .unwrap_or(name)
    }

    /// Clean up all script-defined functions within the `Engine`.
    pub fn clear_functions(&mut self) {
        self.script_functions.clear();
    }
}

/// Print/debug to stdout
#[cfg(not(feature = "no_stdlib"))]
fn default_print(s: &str) {
    println!("{}", s);
}

/// No-op
#[cfg(feature = "no_stdlib")]
fn default_print(_: &str) {}
