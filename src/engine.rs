use std::any::TypeId;
use std::cmp::{PartialEq, PartialOrd};
use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;

use crate::any::{Any, AnyExt, Dynamic, Variant};
use crate::call::FunArgs;
use crate::fn_register::RegisterFn;
use crate::parser::{Expr, FnDef, ParseError, Position, Stmt};
use crate::scope::Scope;

pub type Array = Vec<Dynamic>;
pub type FnCallArgs<'a> = Vec<&'a mut Variant>;

const KEYWORD_PRINT: &'static str = "print";
const KEYWORD_DEBUG: &'static str = "debug";
const KEYWORD_TYPE_OF: &'static str = "type_of";

#[derive(Debug)]
pub enum EvalAltResult {
    ErrorParsing(ParseError),
    ErrorFunctionNotFound(String, Position),
    ErrorFunctionArgsMismatch(String, usize, Position),
    ErrorBooleanArgMismatch(String, Position),
    ErrorArrayBounds(usize, i64, Position),
    ErrorStringBounds(usize, i64, Position),
    ErrorIndexing(Position),
    ErrorIndexExpr(Position),
    ErrorIfGuard(Position),
    ErrorFor(Position),
    ErrorVariableNotFound(String, Position),
    ErrorAssignmentToUnknownLHS(Position),
    ErrorMismatchOutputType(String, Position),
    ErrorCantOpenScriptFile(String, std::io::Error),
    ErrorDotExpr(Position),
    ErrorArithmetic(String, Position),
    ErrorRuntime(String, Position),
    LoopBreak,
    Return(Dynamic, Position),
}

impl Error for EvalAltResult {
    fn description(&self) -> &str {
        match self {
            Self::ErrorParsing(p) => p.description(),
            Self::ErrorFunctionNotFound(_, _) => "Function not found",
            Self::ErrorFunctionArgsMismatch(_, _, _) => {
                "Function call with wrong number of arguments"
            }
            Self::ErrorBooleanArgMismatch(_, _) => "Boolean operator expects boolean operands",
            Self::ErrorIndexExpr(_) => "Indexing into an array or string expects an integer index",
            Self::ErrorIndexing(_) => "Indexing can only be performed on an array or a string",
            Self::ErrorArrayBounds(_, index, _) if *index < 0 => {
                "Array access expects non-negative index"
            }
            Self::ErrorArrayBounds(max, _, _) if *max == 0 => "Access of empty array",
            Self::ErrorArrayBounds(_, _, _) => "Array index out of bounds",
            Self::ErrorStringBounds(_, index, _) if *index < 0 => {
                "Indexing a string expects a non-negative index"
            }
            Self::ErrorStringBounds(max, _, _) if *max == 0 => "Indexing of empty string",
            Self::ErrorStringBounds(_, _, _) => "String index out of bounds",
            Self::ErrorIfGuard(_) => "If guard expects boolean expression",
            Self::ErrorFor(_) => "For loop expects array or range",
            Self::ErrorVariableNotFound(_, _) => "Variable not found",
            Self::ErrorAssignmentToUnknownLHS(_) => {
                "Assignment to an unsupported left-hand side expression"
            }
            Self::ErrorMismatchOutputType(_, _) => "Output type is incorrect",
            Self::ErrorCantOpenScriptFile(_, _) => "Cannot open script file",
            Self::ErrorDotExpr(_) => "Malformed dot expression",
            Self::ErrorArithmetic(_, _) => "Arithmetic error",
            Self::ErrorRuntime(_, _) => "Runtime error",
            Self::LoopBreak => "[Not Error] Breaks out of loop",
            Self::Return(_, _) => "[Not Error] Function returns value",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl std::fmt::Display for EvalAltResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let desc = self.description();

        match self {
            Self::ErrorFunctionNotFound(s, pos) => write!(f, "{}: '{}' ({})", desc, s, pos),
            Self::ErrorVariableNotFound(s, pos) => write!(f, "{}: '{}' ({})", desc, s, pos),
            Self::ErrorIndexing(pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorIndexExpr(pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorIfGuard(pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorFor(pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorAssignmentToUnknownLHS(pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorMismatchOutputType(s, pos) => write!(f, "{}: {} ({})", desc, s, pos),
            Self::ErrorDotExpr(pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorArithmetic(s, pos) => write!(f, "{}: {} ({})", desc, s, pos),
            Self::ErrorRuntime(s, pos) if s.is_empty() => write!(f, "{} ({})", desc, pos),
            Self::ErrorRuntime(s, pos) => write!(f, "{}: {} ({})", desc, s, pos),
            Self::LoopBreak => write!(f, "{}", desc),
            Self::Return(_, pos) => write!(f, "{} ({})", desc, pos),
            Self::ErrorCantOpenScriptFile(filename, err) => {
                write!(f, "{} '{}': {}", desc, filename, err)
            }
            Self::ErrorParsing(p) => write!(f, "Syntax error: {}", p),
            Self::ErrorFunctionArgsMismatch(fun, n, pos) => {
                write!(f, "Function '{}' expects {} argument(s) ({})", fun, n, pos)
            }
            Self::ErrorBooleanArgMismatch(op, pos) => {
                write!(f, "{} operator expects boolean operands ({})", op, pos)
            }
            Self::ErrorArrayBounds(_, index, pos) if *index < 0 => {
                write!(f, "{}: {} < 0 ({})", desc, index, pos)
            }
            Self::ErrorArrayBounds(max, _, pos) if *max == 0 => write!(f, "{} ({})", desc, pos),
            Self::ErrorArrayBounds(max, index, pos) => {
                write!(f, "{} (max {}): {} ({})", desc, max - 1, index, pos)
            }
            Self::ErrorStringBounds(_, index, pos) if *index < 0 => {
                write!(f, "{}: {} < 0 ({})", desc, index, pos)
            }
            Self::ErrorStringBounds(max, _, pos) if *max == 0 => write!(f, "{} ({})", desc, pos),
            Self::ErrorStringBounds(max, index, pos) => {
                write!(f, "{} (max {}): {} ({})", desc, max - 1, index, pos)
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FnSpec {
    pub ident: String,
    pub args: Option<Vec<TypeId>>,
}

type IteratorFn = dyn Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>>;

/// Rhai's engine type. This is what you use to run Rhai scripts
///
/// ```rust
/// extern crate rhai;
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
pub struct Engine {
    /// A hashmap containing all compiled functions known to the engine
    fns: HashMap<FnSpec, Arc<FnIntExt>>,
    /// A hashmap containing all script-defined functions
    pub(crate) script_fns: HashMap<FnSpec, Arc<FnIntExt>>,
    /// A hashmap containing all iterators known to the engine
    type_iterators: HashMap<TypeId, Arc<IteratorFn>>,
    type_names: HashMap<String, String>,

    pub(crate) on_print: Box<dyn Fn(&str)>,
    pub(crate) on_debug: Box<dyn Fn(&str)>,
}

pub enum FnIntExt {
    Ext(Box<FnAny>),
    Int(FnDef),
}

pub type FnAny = dyn Fn(FnCallArgs, Position) -> Result<Dynamic, EvalAltResult>;

impl Engine {
    pub fn call_fn<'a, I, A, T>(&self, ident: I, args: A) -> Result<T, EvalAltResult>
    where
        I: Into<String>,
        A: FunArgs<'a>,
        T: Any + Clone,
    {
        let pos = Position::new();

        self.call_fn_raw(ident.into(), args.into_vec(), None, pos)
            .and_then(|b| {
                b.downcast().map(|b| *b).map_err(|a| {
                    EvalAltResult::ErrorMismatchOutputType(
                        self.map_type_name((*a).type_name()).into(),
                        pos,
                    )
                })
            })
    }

    /// Universal method for calling functions, that are either
    /// registered with the `Engine` or written in Rhai
    fn call_fn_raw(
        &self,
        ident: String,
        args: FnCallArgs,
        def_value: Option<&Dynamic>,
        pos: Position,
    ) -> Result<Dynamic, EvalAltResult> {
        debug_println!(
            "Calling {}({})",
            ident,
            args.iter()
                .map(|x| (*x).type_name())
                .map(|name| self.map_type_name(name))
                .collect::<Vec<_>>()
                .join(", ")
        );

        let mut spec = FnSpec { ident, args: None };

        // First search in script-defined functions (can override built-in),
        // then in built-in's
        let fn_def = self.script_fns.get(&spec).or_else(|| {
            spec.args = Some(args.iter().map(|a| Any::type_id(&**a)).collect());
            self.fns.get(&spec)
        });

        if let Some(f) = fn_def {
            match **f {
                FnIntExt::Ext(ref f) => {
                    let r = f(args, pos);

                    if r.is_err() {
                        return r;
                    }

                    let callback = match spec.ident.as_str() {
                        KEYWORD_PRINT => &self.on_print,
                        KEYWORD_DEBUG => &self.on_debug,
                        _ => return r,
                    };

                    Ok(callback(
                        r.unwrap()
                            .downcast::<String>()
                            .map(|x| *x)
                            .unwrap_or("error: not a string".into())
                            .as_str(),
                    )
                    .into_dynamic())
                }
                FnIntExt::Int(ref f) => {
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
        } else if spec.ident == KEYWORD_TYPE_OF && args.len() == 1 {
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
                format!("{} ({})", spec.ident, types_list.join(", ")),
                pos,
            ))
        }
    }

    pub(crate) fn register_fn_raw(
        &mut self,
        ident: String,
        args: Option<Vec<TypeId>>,
        f: Box<FnAny>,
    ) {
        debug_println!("Register; {:?} with args {:?}", ident, args);

        let spec = FnSpec { ident, args };

        self.fns.insert(spec, Arc::new(FnIntExt::Ext(f)));
    }

    /// Register a type for use with Engine. Keep in mind that
    /// your type must implement Clone.
    pub fn register_type<T: Any>(&mut self) {
        // currently a no-op, exists for future extensibility
    }

    /// Register an iterator adapter for a type.
    pub fn register_iterator<T: Any, F>(&mut self, f: F)
    where
        F: Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>> + 'static,
    {
        self.type_iterators.insert(TypeId::of::<T>(), Arc::new(f));
    }

    /// Register a get function for a member of a registered type
    pub fn register_get<T: Any + Clone, U: Any + Clone>(
        &mut self,
        name: &str,
        get_fn: impl Fn(&mut T) -> U + 'static,
    ) {
        let get_name = "get$".to_string() + name;
        self.register_fn(&get_name, get_fn);
    }

    /// Register a set function for a member of a registered type
    pub fn register_set<T: Any + Clone, U: Any + Clone>(
        &mut self,
        name: &str,
        set_fn: impl Fn(&mut T, U) -> () + 'static,
    ) {
        let set_name = "set$".to_string() + name;
        self.register_fn(&set_name, set_fn);
    }

    /// Shorthand for registering both getters and setters
    pub fn register_get_set<T: Any + Clone, U: Any + Clone>(
        &mut self,
        name: &str,
        get_fn: impl Fn(&mut T) -> U + 'static,
        set_fn: impl Fn(&mut T, U) -> () + 'static,
    ) {
        self.register_get(name, get_fn);
        self.register_set(name, set_fn);
    }

    fn get_dot_val_helper(
        &self,
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

                self.call_fn_raw(fn_name.into(), args, def_value.as_ref(), *pos)
            }

            Expr::Identifier(id, pos) => {
                let get_fn_name = "get$".to_string() + id;

                self.call_fn_raw(get_fn_name, vec![this_ptr], None, *pos)
            }

            Expr::Index(id, idx_raw, pos) => {
                let idx = self
                    .eval_expr(scope, idx_raw)?
                    .downcast_ref::<i64>()
                    .map(|i| *i)
                    .ok_or(EvalAltResult::ErrorIndexExpr(idx_raw.position()))?;

                let get_fn_name = "get$".to_string() + id;

                let mut val = self.call_fn_raw(get_fn_name, vec![this_ptr], None, *pos)?;

                if let Some(arr) = val.downcast_mut() as Option<&mut Array> {
                    if idx >= 0 {
                        arr.get(idx as usize)
                            .cloned()
                            .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx, *pos))
                    } else {
                        Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, *pos))
                    }
                } else if let Some(s) = val.downcast_mut() as Option<&mut String> {
                    if idx >= 0 {
                        s.chars()
                            .nth(idx as usize)
                            .map(|ch| ch.into_dynamic())
                            .ok_or_else(|| {
                                EvalAltResult::ErrorStringBounds(s.chars().count(), idx, *pos)
                            })
                    } else {
                        Err(EvalAltResult::ErrorStringBounds(
                            s.chars().count(),
                            idx,
                            *pos,
                        ))
                    }
                } else {
                    Err(EvalAltResult::ErrorIndexing(*pos))
                }
            }

            Expr::Dot(inner_lhs, inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id, pos) => {
                    let get_fn_name = "get$".to_string() + id;
                    let value = self
                        .call_fn_raw(get_fn_name, vec![this_ptr], None, pos)
                        .and_then(|mut v| self.get_dot_val_helper(scope, v.as_mut(), inner_rhs))?;

                    // TODO - Should propagate changes back in this scenario:
                    //
                    //     fn update(p) { p = something_else; }
                    //     obj.prop.update();
                    //
                    // Right now, a copy of the object's property value is mutated, but not propagated
                    // back to the property via $set.

                    Ok(value)
                }
                Expr::Index(_, _, pos) => {
                    // TODO - Handle Expr::Index for these scenarios:
                    //
                    //    let x = obj.prop[2].x;
                    //    obj.prop[3] = 42;
                    //
                    Err(EvalAltResult::ErrorDotExpr(pos))
                }
                _ => Err(EvalAltResult::ErrorDotExpr(inner_lhs.position())),
            },

            _ => Err(EvalAltResult::ErrorDotExpr(dot_rhs.position())),
        }
    }

    fn search_scope<T>(
        scope: &Scope,
        id: &str,
        map: impl FnOnce(&Variant) -> Result<T, EvalAltResult>,
        begin: Position,
    ) -> Result<(usize, T), EvalAltResult> {
        scope
            .get(id)
            .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.into(), begin))
            .and_then(move |(idx, _, val)| map(val.as_ref()).map(|v| (idx, v)))
    }

    fn indexed_value(
        &self,
        scope: &mut Scope,
        id: &str,
        idx: &Expr,
        begin: Position,
    ) -> Result<(bool, usize, usize, Dynamic), EvalAltResult> {
        let idx = *self
            .eval_expr(scope, idx)?
            .downcast::<i64>()
            .map_err(|_| EvalAltResult::ErrorIndexExpr(idx.position()))?;

        let mut is_array = false;

        Self::search_scope(
            scope,
            id,
            |val| {
                if let Some(arr) = val.downcast_ref() as Option<&Array> {
                    is_array = true;

                    if idx >= 0 {
                        arr.get(idx as usize)
                            .cloned()
                            .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx, begin))
                    } else {
                        Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx, begin))
                    }
                } else if let Some(s) = val.downcast_ref() as Option<&String> {
                    is_array = false;

                    if idx >= 0 {
                        s.chars()
                            .nth(idx as usize)
                            .map(|ch| ch.into_dynamic())
                            .ok_or_else(|| {
                                EvalAltResult::ErrorStringBounds(s.chars().count(), idx, begin)
                            })
                    } else {
                        Err(EvalAltResult::ErrorStringBounds(
                            s.chars().count(),
                            idx,
                            begin,
                        ))
                    }
                } else {
                    Err(EvalAltResult::ErrorIndexing(begin))
                }
            },
            begin,
        )
        .map(|(idx_sc, val)| (is_array, idx_sc, idx as usize, val))
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
        &self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            Expr::Identifier(id, pos) => {
                let (sc_idx, mut target) =
                    Self::search_scope(scope, id, |x| Ok(x.into_dynamic()), *pos)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                *scope.get_mut(id, sc_idx) = target;

                value
            }

            Expr::Index(id, idx_raw, pos) => {
                let (is_array, sc_idx, idx, mut target) =
                    self.indexed_value(scope, id, idx_raw, *pos)?;
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
        &self,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
        mut source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            Expr::Identifier(id, pos) => {
                let set_fn_name = "set$".to_string() + id;

                self.call_fn_raw(set_fn_name, vec![this_ptr, source_val.as_mut()], None, *pos)
            }

            Expr::Dot(inner_lhs, inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id, pos) => {
                    let get_fn_name = "get$".to_string() + id;

                    self.call_fn_raw(get_fn_name, vec![this_ptr], None, pos)
                        .and_then(|mut v| {
                            self.set_dot_val_helper(v.as_mut(), inner_rhs, source_val)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = "set$".to_string() + id;

                            self.call_fn_raw(set_fn_name, vec![this_ptr, v.as_mut()], None, pos)
                        })
                }
                _ => Err(EvalAltResult::ErrorDotExpr(inner_lhs.position())),
            },

            _ => Err(EvalAltResult::ErrorDotExpr(dot_rhs.position())),
        }
    }

    fn set_dot_val(
        &self,
        scope: &mut Scope,
        dot_lhs: &Expr,
        dot_rhs: &Expr,
        source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_lhs {
            Expr::Identifier(id, pos) => {
                let (sc_idx, mut target) =
                    Self::search_scope(scope, id, |x| Ok(x.into_dynamic()), *pos)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                *scope.get_mut(id, sc_idx) = target;

                value
            }

            Expr::Index(id, idx_raw, pos) => {
                let (is_array, sc_idx, idx, mut target) =
                    self.indexed_value(scope, id, idx_raw, *pos)?;
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

    fn eval_expr(&self, scope: &mut Scope, expr: &Expr) -> Result<Dynamic, EvalAltResult> {
        match expr {
            Expr::IntegerConstant(i, _) => Ok((*i).into_dynamic()),
            Expr::FloatConstant(i, _) => Ok((*i).into_dynamic()),
            Expr::StringConstant(s, _) => Ok(s.into_dynamic()),
            Expr::CharConstant(c, _) => Ok((*c).into_dynamic()),

            Expr::Identifier(id, pos) => scope
                .get(id)
                .map(|(_, _, val)| val)
                .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.clone(), *pos)),

            Expr::Index(id, idx_raw, pos) => self
                .indexed_value(scope, id, idx_raw, *pos)
                .map(|(_, _, _, x)| x),

            Expr::Assignment(ref id, rhs) => {
                let rhs_val = self.eval_expr(scope, rhs)?;

                match **id {
                    Expr::Identifier(ref name, pos) => {
                        if let Some((idx, _, _)) = scope.get(name) {
                            *scope.get_mut(name, idx) = rhs_val;
                            Ok(().into_dynamic())
                        } else {
                            Err(EvalAltResult::ErrorVariableNotFound(name.clone(), pos))
                        }
                    }
                    Expr::Index(ref id, ref idx_raw, pos) => {
                        let idx_pos = idx_raw.position();

                        let idx = *match self.eval_expr(scope, &idx_raw)?.downcast_ref::<i64>() {
                            Some(x) => x,
                            _ => return Err(EvalAltResult::ErrorIndexExpr(idx_pos)),
                        };

                        let variable = &mut scope
                            .iter_mut()
                            .rev()
                            .filter(|(name, _)| id == name)
                            .map(|(_, val)| val)
                            .next();

                        let val = match variable {
                            Some(v) => v,
                            _ => return Err(EvalAltResult::ErrorVariableNotFound(id.clone(), pos)),
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

                    Expr::Dot(ref dot_lhs, ref dot_rhs) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val)
                    }

                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS(id.position())),
                }
            }

            Expr::Dot(lhs, rhs) => self.get_dot_val(scope, lhs, rhs),

            Expr::Array(contents, _) => {
                let mut arr = Vec::new();

                contents.iter().try_for_each(|item| {
                    let arg = self.eval_expr(scope, item)?;
                    arr.push(arg);
                    Ok(())
                })?;

                Ok(Box::new(arr))
            }

            Expr::FunctionCall(fn_name, args, def_value, pos) => self.call_fn_raw(
                fn_name.into(),
                args.iter()
                    .map(|expr| self.eval_expr(scope, expr))
                    .collect::<Result<Array, _>>()?
                    .iter_mut()
                    .map(|b| b.as_mut())
                    .collect(),
                def_value.as_ref(),
                *pos,
            ),

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
        &self,
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
                    (val.downcast_ref() as Option<&String>)
                        .map(|s| s.as_ref())
                        .unwrap_or("")
                        .to_string(),
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
    pub fn new() -> Engine {
        // User-friendly names for built-in types
        let type_names = [
            ("alloc::string::String", "string"),
            (
                "alloc::vec::Vec<alloc::boxed::Box<dyn rhai::any::Any>>",
                "array",
            ),
            ("alloc::boxed::Box<dyn rhai::any::Any>", "dynamic"),
        ]
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect();

        let mut engine = Engine {
            fns: HashMap::new(),
            script_fns: HashMap::new(),
            type_iterators: HashMap::new(),
            type_names,
            on_print: Box::new(|x: &str| println!("{}", x)),
            on_debug: Box::new(|x: &str| println!("{}", x)),
        };

        engine.register_builtins();

        engine
    }
}
