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
                    let r = f(args, pos);

                    if r.is_err() {
                        return r;
                    }

                    let callback = match spec.name.as_ref() {
                        KEYWORD_PRINT => self.on_print.as_mut(),
                        KEYWORD_DEBUG => self.on_debug.as_mut(),
                        _ => return r,
                    };

                    Ok(callback(
                        &r.unwrap()
                            .downcast::<String>()
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

    fn get_dot_val_helper(
        &mut self,
        scope: &mut Scope,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
    ) -> Result<Dynamic, EvalAltResult> {
        use std::iter::once;

        match dot_rhs {
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

            Expr::Identifier(id, pos) => {
                let get_fn_name = format!("get${}", id);

                self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
            }

            Expr::Index(id, idx_expr, pos) => {
                let idx = *self
                    .eval_expr(scope, idx_expr)?
                    .downcast::<i64>()
                    .map_err(|_| EvalAltResult::ErrorIndexExpr(idx_expr.position()))?;

                let get_fn_name = format!("get${}", id);
                let val = self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)?;
                Self::get_indexed_value(val, idx, *pos).map(|(v, _)| v)
            }

            Expr::Dot(inner_lhs, inner_rhs) => match inner_lhs.as_ref() {
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("get${}", id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|mut v| self.get_dot_val_helper(scope, v.as_mut(), inner_rhs))
                }
                Expr::Index(id, idx_expr, pos) => {
                    let idx = *self
                        .eval_expr(scope, idx_expr)?
                        .downcast::<i64>()
                        .map_err(|_| EvalAltResult::ErrorIndexExpr(idx_expr.position()))?;

                    let get_fn_name = format!("get${}", id);
                    let val = self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)?;
                    Self::get_indexed_value(val, idx, *pos).and_then(|(mut v, _)| {
                        self.get_dot_val_helper(scope, v.as_mut(), inner_rhs)
                    })
                }
                _ => Err(EvalAltResult::ErrorDotExpr(inner_lhs.position())),
            },

            _ => Err(EvalAltResult::ErrorDotExpr(dot_rhs.position())),
        }
    }

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

    fn get_indexed_value(
        val: Dynamic,
        idx: i64,
        pos: Position,
    ) -> Result<(Dynamic, bool), EvalAltResult> {
        if val.is::<Array>() {
            let arr = val.downcast::<Array>().unwrap();

            if idx >= 0 {
                arr.get(idx as usize)
                    .cloned()
                    .map(|v| (v, true))
                    .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx, pos))
            } else {
                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, pos))
            }
        } else if val.is::<String>() {
            let s = val.downcast::<String>().unwrap();

            if idx >= 0 {
                s.chars()
                    .nth(idx as usize)
                    .map(|ch| (ch.into_dynamic(), false))
                    .ok_or_else(|| EvalAltResult::ErrorStringBounds(s.chars().count(), idx, pos))
            } else {
                Err(EvalAltResult::ErrorStringBounds(
                    s.chars().count(),
                    idx,
                    pos,
                ))
            }
        } else {
            Err(EvalAltResult::ErrorIndexing(pos))
        }
    }

    fn eval_index_expr(
        &mut self,
        scope: &mut Scope,
        id: &str,
        idx: &Expr,
        begin: Position,
    ) -> Result<(bool, usize, usize, Dynamic), EvalAltResult> {
        let idx = *self
            .eval_expr(scope, idx)?
            .downcast::<i64>()
            .map_err(|_| EvalAltResult::ErrorIndexExpr(idx.position()))?;

        Self::search_scope(
            scope,
            id,
            |val| Self::get_indexed_value(val, idx, begin),
            begin,
        )
        .map(|(idx_sc, (val, is_array))| (is_array, idx_sc, idx as usize, val))
    }

    fn str_replace_char(s: &mut String, idx: usize, new_ch: char) {
        // The new character
        let ch = s.chars().nth(idx).unwrap();

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

    fn get_dot_val(
        &mut self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            Expr::Identifier(id, pos) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, Ok, *pos)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                *scope.get_mut(id, sc_idx) = target;

                value
            }

            Expr::Index(id, idx_expr, pos) => {
                let (is_array, sc_idx, idx, mut target) =
                    self.eval_index_expr(scope, id, idx_expr, *pos)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.

                if is_array {
                    scope.get_mut(id, sc_idx).downcast_mut::<Array>().unwrap()[idx] = target;
                } else {
                    Self::str_replace_char(
                        scope.get_mut(id, sc_idx).downcast_mut::<String>().unwrap(), // Root is a string
                        idx,
                        *target.downcast::<char>().unwrap(), // Target should be a char
                    );
                }

                value
            }

            _ => Err(EvalAltResult::ErrorDotExpr(dot_lhs.position())),
        }
    }

    fn set_dot_val_helper(
        &mut self,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
        mut source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            Expr::Identifier(id, pos) => {
                let set_fn_name = format!("set${}", id);

                self.call_fn_raw(
                    &set_fn_name,
                    vec![this_ptr, source_val.as_mut()],
                    None,
                    *pos,
                )
            }

            Expr::Dot(inner_lhs, inner_rhs) => match inner_lhs.as_ref() {
                Expr::Identifier(id, pos) => {
                    let get_fn_name = format!("get${}", id);

                    self.call_fn_raw(&get_fn_name, vec![this_ptr], None, *pos)
                        .and_then(|mut v| {
                            self.set_dot_val_helper(v.as_mut(), inner_rhs, source_val)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = format!("set${}", id);

                            self.call_fn_raw(&set_fn_name, vec![this_ptr, v.as_mut()], None, *pos)
                        })
                }
                _ => Err(EvalAltResult::ErrorDotExpr(inner_lhs.position())),
            },

            _ => Err(EvalAltResult::ErrorDotExpr(dot_rhs.position())),
        }
    }

    fn set_dot_val(
        &mut self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            Expr::Identifier(id, pos) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, Ok, *pos)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                *scope.get_mut(id, sc_idx) = target;

                value
            }

            Expr::Index(id, iex_expr, pos) => {
                let (is_array, sc_idx, idx, mut target) =
                    self.eval_index_expr(scope, id, iex_expr, *pos)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                if is_array {
                    scope.get_mut(id, sc_idx).downcast_mut::<Array>().unwrap()[idx] = target;
                } else {
                    Self::str_replace_char(
                        scope.get_mut(id, sc_idx).downcast_mut::<String>().unwrap(), // Root is a string
                        idx,
                        *target.downcast::<char>().unwrap(), // Target should be a char
                    );
                }

                value
            }

            _ => Err(EvalAltResult::ErrorDotExpr(dot_lhs.position())),
        }
    }

    fn eval_expr(&mut self, scope: &mut Scope, expr: &Expr) -> Result<Dynamic, EvalAltResult> {
        match expr {
            Expr::IntegerConstant(i, _) => Ok((*i).into_dynamic()),
            Expr::FloatConstant(i, _) => Ok((*i).into_dynamic()),
            Expr::StringConstant(s, _) => Ok(s.into_dynamic()),
            Expr::CharConstant(c, _) => Ok((*c).into_dynamic()),

            Expr::Identifier(id, pos) => scope
                .get(id)
                .map(|(_, _, val)| val)
                .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.clone(), *pos)),

            Expr::Index(id, idx_expr, pos) => self
                .eval_index_expr(scope, id, idx_expr, *pos)
                .map(|(_, _, _, x)| x),

            Expr::Assignment(ref id, rhs) => {
                let rhs_val = self.eval_expr(scope, rhs)?;

                match id.as_ref() {
                    Expr::Identifier(name, pos) => {
                        if let Some((idx, _, _)) = scope.get(name) {
                            *scope.get_mut(name, idx) = rhs_val;
                            Ok(().into_dynamic())
                        } else {
                            Err(EvalAltResult::ErrorVariableNotFound(name.clone(), *pos))
                        }
                    }
                    Expr::Index(id, idx_expr, pos) => {
                        let idx_pos = idx_expr.position();

                        let idx = *match self.eval_expr(scope, &idx_expr)?.downcast::<i64>() {
                            Ok(x) => x,
                            _ => return Err(EvalAltResult::ErrorIndexExpr(idx_pos)),
                        };

                        let val = match scope.get(id) {
                            Some((idx, _, _)) => scope.get_mut(id, idx),
                            _ => {
                                return Err(EvalAltResult::ErrorVariableNotFound(id.clone(), *pos))
                            }
                        };

                        if let Some(arr) = val.downcast_mut() as Option<&mut Array> {
                            if idx < 0 {
                                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, idx_pos))
                            } else if idx as usize >= arr.len() {
                                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, idx_pos))
                            } else {
                                arr[idx as usize] = rhs_val;
                                Ok(().into_dynamic())
                            }
                        } else if let Some(s) = val.downcast_mut() as Option<&mut String> {
                            let s_len = s.chars().count();

                            if idx < 0 {
                                Err(EvalAltResult::ErrorStringBounds(s_len, idx, idx_pos))
                            } else if idx as usize >= s_len {
                                Err(EvalAltResult::ErrorStringBounds(s_len, idx, idx_pos))
                            } else {
                                Self::str_replace_char(
                                    s,
                                    idx as usize,
                                    *rhs_val.downcast::<char>().unwrap(),
                                );
                                Ok(().into_dynamic())
                            }
                        } else {
                            Err(EvalAltResult::ErrorIndexExpr(idx_pos))
                        }
                    }

                    Expr::Dot(dot_lhs, dot_rhs) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val)
                    }

                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS(id.position())),
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
                    && *self
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
                    || *self
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
                    } else if else_body.is_some() {
                        self.eval_stmt(scope, else_body.as_ref().unwrap())
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
