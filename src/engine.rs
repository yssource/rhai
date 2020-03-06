use std::any::TypeId;
use std::borrow::Cow;
use std::cmp::{PartialEq, PartialOrd};
use std::collections::HashMap;
use std::sync::Arc;

use crate::any::{Any, AnyExt, Dynamic, Variant};
use crate::parser::{Expr, FnDef, Position, Stmt};
use crate::result::EvalAltResult;
use crate::scope::Scope;

/// An dynamic array of `Dynamic` values.
pub type Array = Vec<Dynamic>;

pub type FnCallArgs<'a> = Vec<&'a mut Variant>;

const KEYWORD_PRINT: &'static str = "print";
const KEYWORD_DEBUG: &'static str = "debug";
const KEYWORD_TYPE_OF: &'static str = "type_of";

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
enum VariableType {
    Array,
    String,
    Expression,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FnSpec<'a> {
    pub name: Cow<'a, str>,
    pub args: Option<Vec<TypeId>>,
}

type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

/// Rhai main scripting engine.
///
/// ```rust
/// use rhai::Engine;
///
/// fn main() {
///     let mut engine = Engine::new();
///
///     if let Ok(result) = engine.eval::<i64>("40 + 2") {
///         println!("Answer: {}", result);  // prints 42
///     }
/// }
/// ```
pub struct Engine<'a> {
    /// A hashmap containing all compiled functions known to the engine
    pub(crate) external_functions: HashMap<FnSpec<'a>, Arc<FnIntExt>>,
    /// A hashmap containing all script-defined functions
    pub(crate) script_functions: HashMap<FnSpec<'a>, Arc<FnIntExt>>,
    /// A hashmap containing all iterators known to the engine
    pub(crate) type_iterators: HashMap<TypeId, Arc<IteratorFn>>,
    pub(crate) type_names: HashMap<String, String>,

    pub(crate) on_print: Box<dyn FnMut(&str) + 'a>,
    pub(crate) on_debug: Box<dyn FnMut(&str) + 'a>,
}

pub enum FnIntExt {
    Ext(Box<FnAny>),
    Int(FnDef),
}

pub type FnAny = dyn Fn(FnCallArgs, Position) -> Result<Dynamic, EvalAltResult>;

impl Engine<'_> {
    /// Universal method for calling functions, that are either
    /// registered with the `Engine` or written in Rhai
    pub(crate) fn call_fn_raw(
        &mut self,
        fn_name: &str,
        args: FnCallArgs,
        def_value: Option<&Dynamic>,
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
        // then in built-in's
        let fn_def = self
            .script_functions
            .get(&spec)
            .or_else(|| {
                spec.args = Some(args.iter().map(|a| Any::type_id(&**a)).collect());
                self.external_functions.get(&spec)
            })
            .map(|f| f.clone());

        if let Some(f) = fn_def {
            match *f {
                FnIntExt::Ext(ref f) => {
                    let r = f(args, pos)?;

                    let callback = match spec.name.as_ref() {
                        KEYWORD_PRINT => self.on_print.as_mut(),
                        KEYWORD_DEBUG => self.on_debug.as_mut(),
                        _ => return Ok(r),
                    };

                    Ok(callback(
                        &r.downcast::<String>()
                            .map(|s| *s)
                            .unwrap_or("error: not a string".into()),
                    )
                    .into_dynamic())
                }
                FnIntExt::Int(ref f) => {
                    if f.params.len() != args.len() {
                        return Err(EvalAltResult::ErrorFunctionArgsMismatch(
                            spec.name.into(),
                            f.params.len(),
                            args.len(),
                            pos,
                        ));
                    }

                    let mut scope = Scope::new();

                    scope.extend(
                        f.params
                            .iter()
                            .cloned()
                            .zip(args.iter().map(|x| (*x).into_dynamic())),
                    );

                    match self.eval_stmt(&mut scope, &*f.body) {
                        Err(EvalAltResult::Return(x, _)) => Ok(x),
                        other => other,
                    }
                }
            }
        } else if spec.name == KEYWORD_TYPE_OF && args.len() == 1 {
            Ok(self
                .map_type_name(args[0].type_name())
                .to_string()
                .into_dynamic())
        } else if let Some(val) = def_value {
            // Return default value
            Ok(val.clone())
        } else {
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
        use std::iter::once;

        match dot_rhs {
            // xxx.fn_name(args)
            Expr::FunctionCall(fn_name, args, def_value, pos) => {
                let mut args: Array = args
                    .iter()
                    .map(|arg| self.eval_expr(scope, arg))
                    .collect::<Result<Vec<_>, _>>()?;

                let args = once(this_ptr)
                    .chain(args.iter_mut().map(|b| b.as_mut()))
                    .collect();

                self.call_fn_raw(fn_name, args, def_value.as_ref(), *pos)
            }

            // xxx.id
            Expr::Identifier(id, pos) => {
                let get_fn_name = format!("get${}", id);

                self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
            }

            // xxx.lhs[idx_expr]
            Expr::Index(lhs, idx_expr) => {
                let idx = self.eval_index_value(scope, idx_expr)?;

                let (lhs_value, _) = match lhs.as_ref() {
                    Expr::Identifier(id, pos) => {
                        let get_fn_name = format!("get${}", id);
                        (
                            self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)?,
                            *pos,
                        )
                    }
                    expr => return Err(EvalAltResult::ErrorDotExpr(expr.position())),
                };

                Self::get_indexed_value(lhs_value, idx, idx_expr.position()).map(|(v, _)| v)
            }

            // xxx.lhs.rhs
            Expr::Dot(lhs, rhs) => match lhs.as_ref() {
                // xxx.id.rhs
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("get${}", id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|mut v| self.get_dot_val_helper(scope, v.as_mut(), rhs))
                }
                // xxx.lhs[idx_expr].rhs
                Expr::Index(lhs, idx_expr) => {
                    let idx = self.eval_index_value(scope, idx_expr)?;

                    let (lhs_value, _) = match lhs.as_ref() {
                        Expr::Identifier(id, pos) => {
                            let get_fn_name = format!("get${}", id);
                            (
                                self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)?,
                                *pos,
                            )
                        }
                        expr => return Err(EvalAltResult::ErrorDotExpr(expr.position())),
                    };

                    Self::get_indexed_value(lhs_value, idx, idx_expr.position()).and_then(
                        |(mut value, _)| self.get_dot_val_helper(scope, value.as_mut(), rhs),
                    )
                }
                // Syntax error
                _ => Err(EvalAltResult::ErrorDotExpr(lhs.position())),
            },

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(dot_rhs.position())),
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
        val: Dynamic,
        idx: i64,
        pos: Position,
    ) -> Result<(Dynamic, VariableType), EvalAltResult> {
        if val.is::<Array>() {
            let arr = val.downcast::<Array>().expect("array expected");

            if idx >= 0 {
                arr.get(idx as usize)
                    .cloned()
                    .map(|v| (v, VariableType::Array))
                    .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx, pos))
            } else {
                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, pos))
            }
        } else if val.is::<String>() {
            let s = val.downcast::<String>().expect("string expected");

            if idx >= 0 {
                s.chars()
                    .nth(idx as usize)
                    .map(|ch| (ch.into_dynamic(), VariableType::String))
                    .ok_or_else(|| EvalAltResult::ErrorStringBounds(s.chars().count(), idx, pos))
            } else {
                Err(EvalAltResult::ErrorStringBounds(
                    s.chars().count(),
                    idx,
                    pos,
                ))
            }
        } else {
            Err(EvalAltResult::ErrorIndexingType(pos))
        }
    }

    /// Evaluate an index expression
    fn eval_index_expr(
        &mut self,
        scope: &mut Scope,
        lhs: &Expr,
        idx_expr: &Expr,
    ) -> Result<(VariableType, Option<(String, usize)>, usize, Dynamic), EvalAltResult> {
        let idx = self.eval_index_value(scope, idx_expr)?;

        match lhs {
            // id[idx_expr]
            Expr::Identifier(id, _) => Self::search_scope(
                scope,
                &id,
                |val| Self::get_indexed_value(val, idx, idx_expr.position()),
                lhs.position(),
            )
            .map(|(src_idx, (val, source_type))| {
                (source_type, Some((id.clone(), src_idx)), idx as usize, val)
            }),

            // (expr)[idx_expr]
            expr => Self::get_indexed_value(self.eval_expr(scope, expr)?, idx, idx_expr.position())
                .map(|(val, _)| (VariableType::Expression, None, idx as usize, val)),
        }
    }

    /// Replace a character at an index position in a mutable string
    fn str_replace_char(s: &mut String, idx: usize, new_ch: char) {
        // The new character
        let ch = s.chars().nth(idx).expect("string index out of bounds");

        // See if changed - if so, update the String
        if ch == new_ch {
            return;
        }

        // Collect all the characters after the index
        let mut chars: Vec<char> = s.chars().collect();
        chars[idx] = new_ch;
        s.clear();
        chars.iter().for_each(|&ch| s.push(ch));
    }

    /// Update the value at an index position in a variable inside the scope
    fn update_indexed_variable_in_scope(
        source_type: VariableType,
        scope: &mut Scope,
        id: &str,
        src_idx: usize,
        idx: usize,
        val: Dynamic,
    ) -> Option<Dynamic> {
        match source_type {
            VariableType::Array => {
                let arr = scope.get_mut_by_type::<Array>(id, src_idx);
                Some((arr[idx as usize] = val).into_dynamic())
            }

            VariableType::String => {
                let s = scope.get_mut_by_type::<String>(id, src_idx);
                // Value must be a character
                let ch = *val
                    .downcast::<char>()
                    .expect("char value expected to update an index position in a string");
                Some(Self::str_replace_char(s, idx as usize, ch).into_dynamic())
            }

            _ => None,
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
            // xxx.???
            Expr::Identifier(id, pos) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, Ok, *pos)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                *scope.get_mut(id, sc_idx) = target;

                value
            }

            // lhs[idx_expr].???
            Expr::Index(lhs, idx_expr) => {
                let (source_type, src, idx, mut target) =
                    self.eval_index_expr(scope, lhs, idx_expr)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                if let Some((id, src_idx)) = src {
                    Self::update_indexed_variable_in_scope(
                        source_type,
                        scope,
                        &id,
                        src_idx,
                        idx,
                        target,
                    )
                    .expect("array or string source type expected for indexing");
                }

                value
            }

            // {expr}.???
            expr => {
                let mut target = self.eval_expr(scope, expr)?;
                self.get_dot_val_helper(scope, target.as_mut(), dot_rhs)
            }
            // Syntax error
            //_ => Err(EvalAltResult::ErrorDotExpr(dot_lhs.position())),
        }
    }

    /// Chain-evaluate a dot setter
    fn set_dot_val_helper(
        &mut self,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
        mut source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            // xxx.id
            Expr::Identifier(id, pos) => {
                let set_fn_name = format!("set${}", id);

                self.call_fn_raw(
                    &set_fn_name,
                    vec![this_ptr, source_val.as_mut()],
                    None,
                    *pos,
                )
            }

            // xxx.lhs.rhs
            Expr::Dot(lhs, rhs) => match lhs.as_ref() {
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("get${}", id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|mut v| {
                            self.set_dot_val_helper(v.as_mut(), rhs, source_val)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = format!("set${}", id);

                            self.call_fn_raw(&set_fn_name, vec![this_ptr, v.as_mut()], None, *pos)
                        })
                }
                _ => Err(EvalAltResult::ErrorDotExpr(lhs.position())),
            },

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(dot_rhs.position())),
        }
    }

    // Evaluate a dot chain setter
    fn set_dot_val(
        &mut self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            // id.???
            Expr::Identifier(id, pos) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, Ok, *pos)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                *scope.get_mut(id, sc_idx) = target;

                value
            }

            // lhs[idx_expr].???
            Expr::Index(lhs, idx_expr) => {
                let (source_type, src, idx, mut target) =
                    self.eval_index_expr(scope, lhs, idx_expr)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.

                if let Some((id, src_idx)) = src {
                    Self::update_indexed_variable_in_scope(
                        source_type,
                        scope,
                        &id,
                        src_idx,
                        idx,
                        target,
                    )
                    .expect("array or string source_type expected for indexing");
                }

                value
            }

            // Syntax error
            _ => Err(EvalAltResult::ErrorDotExpr(dot_lhs.position())),
        }
    }

    /// Evaluate an expression
    fn eval_expr(&mut self, scope: &mut Scope, expr: &Expr) -> Result<Dynamic, EvalAltResult> {
        match expr {
            Expr::IntegerConstant(i, _) => Ok((*i).into_dynamic()),
            Expr::FloatConstant(i, _) => Ok((*i).into_dynamic()),
            Expr::StringConstant(s, _) => Ok(s.into_dynamic()),
            Expr::CharConstant(c, _) => Ok((*c).into_dynamic()),
            Expr::Identifier(id, pos) => {
                Self::search_scope(scope, id, Ok, *pos).map(|(_, val)| val)
            }
            Expr::Index(lhs, idx_expr) => self
                .eval_index_expr(scope, lhs, idx_expr)
                .map(|(_, _, _, x)| x),

            // lhs = rhs
            Expr::Assignment(lhs, rhs) => {
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
                    Expr::Index(idx_lhs, idx_expr) => {
                        let (source_type, src, idx, _) =
                            self.eval_index_expr(scope, idx_lhs, idx_expr)?;

                        if let Some((id, src_idx)) = src {
                            Self::update_indexed_variable_in_scope(
                                source_type,
                                scope,
                                &id,
                                src_idx,
                                idx,
                                rhs_val,
                            )
                        } else {
                            None
                        }
                        .ok_or_else(|| {
                            EvalAltResult::ErrorAssignmentToUnknownLHS(idx_lhs.position())
                        })
                    }

                    // dot_lhs.dot_rhs = rhs
                    Expr::Dot(dot_lhs, dot_rhs) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val)
                    }

                    // Syntax error
                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS(lhs.position())),
                }
            }

            Expr::Dot(lhs, rhs) => self.get_dot_val(scope, lhs, rhs),

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

            Expr::FunctionCall(fn_name, args, def_value, pos) => {
                let mut args = args
                    .iter()
                    .map(|expr| self.eval_expr(scope, expr))
                    .collect::<Result<Array, _>>()?;

                self.call_fn_raw(
                    fn_name,
                    args.iter_mut().map(|b| b.as_mut()).collect(),
                    def_value.as_ref(),
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
            Stmt::Expr(expr) => self.eval_expr(scope, expr),

            Stmt::Block(block) => {
                let prev_len = scope.len();
                let mut last_result: Result<Dynamic, EvalAltResult> = Ok(().into_dynamic());

                for block_stmt in block.iter() {
                    last_result = self.eval_stmt(scope, block_stmt);

                    if let Err(x) = last_result {
                        last_result = Err(x);
                        break;
                    }
                }

                while scope.len() > prev_len {
                    scope.pop();
                }

                last_result
            }

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

            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, body) {
                    Err(EvalAltResult::LoopBreak) => return Ok(().into_dynamic()),
                    Err(x) => return Err(x),
                    _ => (),
                }
            },

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

    /// Make a new engine
    pub fn new<'a>() -> Engine<'a> {
        use std::any::type_name;

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
            external_functions: HashMap::new(),
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
}

/// Print/debug to stdout
#[cfg(not(feature = "no_stdlib"))]
fn default_print(s: &str) {
    println!("{}", s);
}

/// No-op
#[cfg(feature = "no_stdlib")]
fn default_print(_: &str) {}
