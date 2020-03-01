use std::any::TypeId;
use std::cmp::{PartialEq, PartialOrd};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::sync::Arc;

use crate::any::{Any, AnyExt, Dynamic, Variant};
use crate::call::FunArgs;
use crate::fn_register::RegisterFn;
use crate::parser::{lex, parse, Expr, FnDef, ParseError, Stmt, AST};
use fmt::Debug;

pub type Array = Vec<Dynamic>;
pub type FnCallArgs<'a> = Vec<&'a mut Variant>;

#[derive(Debug, Clone)]
pub enum EvalAltResult {
    ErrorParseError(ParseError),
    ErrorFunctionNotFound(String),
    ErrorFunctionArgMismatch,
    ErrorArrayBounds(usize, i64),
    ErrorStringBounds(usize, i64),
    ErrorIndexing,
    ErrorIndexExpr,
    ErrorIfGuard,
    ErrorFor,
    ErrorVariableNotFound(String),
    ErrorAssignmentToUnknownLHS,
    ErrorMismatchOutputType(String),
    ErrorCantOpenScriptFile(String),
    ErrorDotExpr,
    LoopBreak,
    Return(Dynamic),
}

impl EvalAltResult {
    fn as_str(&self) -> Option<&str> {
        Some(match self {
            EvalAltResult::ErrorCantOpenScriptFile(ref s)
            | EvalAltResult::ErrorVariableNotFound(ref s)
            | EvalAltResult::ErrorFunctionNotFound(ref s)
            | EvalAltResult::ErrorMismatchOutputType(ref s) => s,
            _ => return None,
        })
    }
}

impl PartialEq for EvalAltResult {
    fn eq(&self, other: &Self) -> bool {
        use EvalAltResult::*;

        match (self, other) {
            (ErrorParseError(ref a), ErrorParseError(ref b)) => a == b,
            (ErrorFunctionNotFound(ref a), ErrorFunctionNotFound(ref b)) => a == b,
            (ErrorFunctionArgMismatch, ErrorFunctionArgMismatch) => true,
            (ErrorIndexExpr, ErrorIndexExpr) => true,
            (ErrorIndexing, ErrorIndexing) => true,
            (ErrorArrayBounds(max1, index1), ErrorArrayBounds(max2, index2)) => {
                max1 == max2 && index1 == index2
            }
            (ErrorStringBounds(max1, index1), ErrorStringBounds(max2, index2)) => {
                max1 == max2 && index1 == index2
            }
            (ErrorIfGuard, ErrorIfGuard) => true,
            (ErrorFor, ErrorFor) => true,
            (ErrorVariableNotFound(ref a), ErrorVariableNotFound(ref b)) => a == b,
            (ErrorAssignmentToUnknownLHS, ErrorAssignmentToUnknownLHS) => true,
            (ErrorMismatchOutputType(ref a), ErrorMismatchOutputType(ref b)) => a == b,
            (ErrorCantOpenScriptFile(ref a), ErrorCantOpenScriptFile(ref b)) => a == b,
            (ErrorDotExpr, ErrorDotExpr) => true,
            (LoopBreak, LoopBreak) => true,
            _ => false,
        }
    }
}

impl Error for EvalAltResult {
    fn description(&self) -> &str {
        match self {
            Self::ErrorParseError(ref p) => p.description(),
            Self::ErrorFunctionNotFound(_) => "Function not found",
            Self::ErrorFunctionArgMismatch => "Function argument types do not match",
            Self::ErrorIndexExpr => "Indexing into an array or string expects an integer index",
            Self::ErrorIndexing => "Indexing can only be performed on an array or a string",
            Self::ErrorArrayBounds(_, ref index) if *index < 0 => {
                "Array access expects non-negative index"
            }
            Self::ErrorArrayBounds(ref max, _) if *max == 0 => "Access of empty array",
            Self::ErrorArrayBounds(_, _) => "Array index out of bounds",
            Self::ErrorStringBounds(_, ref index) if *index < 0 => {
                "Indexing a string expects a non-negative index"
            }
            Self::ErrorStringBounds(ref max, _) if *max == 0 => "Indexing of empty string",
            Self::ErrorStringBounds(_, _) => "String index out of bounds",
            Self::ErrorIfGuard => "If guards expect boolean expression",
            Self::ErrorFor => "For loops expect array",
            Self::ErrorVariableNotFound(_) => "Variable not found",
            Self::ErrorAssignmentToUnknownLHS => {
                "Assignment to an unsupported left-hand side expression"
            }
            Self::ErrorMismatchOutputType(_) => "Output type is incorrect",
            Self::ErrorCantOpenScriptFile(_) => "Cannot open script file",
            Self::ErrorDotExpr => "Malformed dot expression",
            Self::LoopBreak => "[Not Error] Breaks out of loop",
            Self::Return(_) => "[Not Error] Function returns value",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl fmt::Display for EvalAltResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(s) = self.as_str() {
            write!(f, "{}: {}", self.description(), s)
        } else {
            match self {
                EvalAltResult::ErrorParseError(ref p) => write!(f, "Syntax error: {}", p),
                EvalAltResult::ErrorArrayBounds(_, index) if *index < 0 => {
                    write!(f, "{}: {} < 0", self.description(), index)
                }
                EvalAltResult::ErrorArrayBounds(max, _) if *max == 0 => {
                    write!(f, "{}", self.description())
                }
                EvalAltResult::ErrorArrayBounds(max, index) => {
                    write!(f, "{} (max {}): {}", self.description(), max - 1, index)
                }
                EvalAltResult::ErrorStringBounds(_, index) if *index < 0 => {
                    write!(f, "{}: {} < 0", self.description(), index)
                }
                EvalAltResult::ErrorStringBounds(max, _) if *max == 0 => {
                    write!(f, "{}", self.description())
                }
                EvalAltResult::ErrorStringBounds(max, index) => {
                    write!(f, "{} (max {}): {}", self.description(), max - 1, index)
                }
                err => write!(f, "{}", err.description()),
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FnSpec {
    ident: String,
    args: Option<Vec<TypeId>>,
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
    /// A hashmap containing all functions known to the engine
    pub fns: HashMap<FnSpec, Arc<FnIntExt>>,
    pub type_iterators: HashMap<TypeId, Arc<IteratorFn>>,
    on_print: Box<dyn Fn(&str)>,
    on_debug: Box<dyn Fn(&str)>,
}

pub enum FnIntExt {
    Ext(Box<FnAny>),
    Int(FnDef),
}

pub type FnAny = dyn Fn(FnCallArgs) -> Result<Dynamic, EvalAltResult>;

/// A type containing information about current scope.
/// Useful for keeping state between `Engine` runs
///
/// ```rust
/// use rhai::{Engine, Scope};
///
/// let mut engine = Engine::new();
/// let mut my_scope = Scope::new();
///
/// assert!(engine.eval_with_scope::<()>(&mut my_scope, "let x = 5;").is_ok());
/// assert_eq!(engine.eval_with_scope::<i64>(&mut my_scope, "x + 1").unwrap(), 6);
/// ```
///
/// Between runs, `Engine` only remembers functions when not using own `Scope`.
pub type Scope = Vec<(String, Dynamic)>;

impl Engine {
    pub fn call_fn<'a, I, A, T>(&self, ident: I, args: A) -> Result<T, EvalAltResult>
    where
        I: Into<String>,
        A: FunArgs<'a>,
        T: Any + Clone,
    {
        self.call_fn_raw(ident.into(), args.into_vec())
            .and_then(|b| {
                b.downcast()
                    .map(|b| *b)
                    .map_err(|a| EvalAltResult::ErrorMismatchOutputType((*a).type_name()))
            })
    }

    /// Universal method for calling functions, that are either
    /// registered with the `Engine` or written in Rhai
    fn call_fn_raw(&self, ident: String, args: FnCallArgs) -> Result<Dynamic, EvalAltResult> {
        debug_println!(
            "Trying to call function {:?} with args {:?}",
            ident,
            args.iter()
                .map(|x| Any::type_name(&**x))
                .collect::<Vec<_>>()
        );

        let spec = FnSpec {
            ident: ident.clone(),
            args: Some(args.iter().map(|a| Any::type_id(&**a)).collect()),
        };

        self.fns
            .get(&spec)
            .or_else(|| {
                let spec1 = FnSpec {
                    ident: ident.clone(),
                    args: None,
                };
                self.fns.get(&spec1)
            })
            .ok_or_else(|| {
                let type_names = args
                    .iter()
                    .map(|x| (*(&**x).into_dynamic()).type_name())
                    .collect::<Vec<_>>();
                EvalAltResult::ErrorFunctionNotFound(format!(
                    "{} ({})",
                    ident,
                    type_names.join(", ")
                ))
            })
            .and_then(move |f| match **f {
                FnIntExt::Ext(ref f) => {
                    let r = f(args);
                    if r.is_err() {
                        return r;
                    }

                    let callback = match ident.as_str() {
                        "print" => &self.on_print,
                        "debug" => &self.on_debug,
                        _ => return r,
                    };

                    Ok(Box::new(callback(
                        r.unwrap()
                            .downcast::<String>()
                            .map(|x| *x)
                            .unwrap_or("error: not a string".into())
                            .as_str(),
                    )))
                }
                FnIntExt::Int(ref f) => {
                    let mut scope = Scope::new();
                    scope.extend(
                        f.params
                            .iter()
                            .cloned()
                            .zip(args.iter().map(|x| (&**x).into_dynamic())),
                    );

                    match self.eval_stmt(&mut scope, &*f.body) {
                        Err(EvalAltResult::Return(x)) => Ok(x),
                        other => other,
                    }
                }
            })
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
        F: 'static + Fn(&Dynamic) -> Box<dyn Iterator<Item = Dynamic>>,
    {
        self.type_iterators.insert(TypeId::of::<T>(), Arc::new(f));
    }

    /// Register a get function for a member of a registered type
    pub fn register_get<T: Any + Clone, U: Any + Clone, F>(&mut self, name: &str, get_fn: F)
    where
        F: 'static + Fn(&mut T) -> U,
    {
        let get_name = "get$".to_string() + name;
        self.register_fn(&get_name, get_fn);
    }

    /// Register a set function for a member of a registered type
    pub fn register_set<T: Any + Clone, U: Any + Clone, F>(&mut self, name: &str, set_fn: F)
    where
        F: 'static + Fn(&mut T, U) -> (),
    {
        let set_name = "set$".to_string() + name;
        self.register_fn(&set_name, set_fn);
    }

    /// Shorthand for registering both getters and setters
    pub fn register_get_set<T: Any + Clone, U: Any + Clone, F, G>(
        &mut self,
        name: &str,
        get_fn: F,
        set_fn: G,
    ) where
        F: 'static + Fn(&mut T) -> U,
        G: 'static + Fn(&mut T, U) -> (),
    {
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
            Expr::FunctionCall(fn_name, args) => {
                let mut args: Array = args
                    .iter()
                    .map(|arg| self.eval_expr(scope, arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let args = once(this_ptr)
                    .chain(args.iter_mut().map(|b| b.as_mut()))
                    .collect();

                self.call_fn_raw(fn_name.to_owned(), args)
            }
            Expr::Identifier(id) => {
                let get_fn_name = "get$".to_string() + id;

                self.call_fn_raw(get_fn_name, vec![this_ptr])
            }
            Expr::Index(id, idx_raw) => {
                let idx = self
                    .eval_expr(scope, idx_raw)?
                    .downcast_ref::<i64>()
                    .map(|i| *i)
                    .ok_or(EvalAltResult::ErrorIndexExpr)?;

                let get_fn_name = "get$".to_string() + id;

                let mut val = self.call_fn_raw(get_fn_name, vec![this_ptr])?;

                if let Some(arr) = (*val).downcast_mut() as Option<&mut Array> {
                    if idx < 0 {
                        Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx))
                    } else {
                        arr.get(idx as usize)
                            .cloned()
                            .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx))
                    }
                } else if let Some(s) = (*val).downcast_mut() as Option<&mut String> {
                    if idx < 0 {
                        Err(EvalAltResult::ErrorStringBounds(s.chars().count(), idx))
                    } else {
                        s.chars()
                            .nth(idx as usize)
                            .map(|ch| Box::new(ch) as Dynamic)
                            .ok_or_else(|| EvalAltResult::ErrorStringBounds(s.chars().count(), idx))
                    }
                } else {
                    Err(EvalAltResult::ErrorIndexing)
                }
            }
            Expr::Dot(inner_lhs, inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id) => {
                    let get_fn_name = "get$".to_string() + id;
                    let value = self
                        .call_fn_raw(get_fn_name, vec![this_ptr])
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
                Expr::Index(_, _) => {
                    // TODO - Handle Expr::Index for these scenarios:
                    //
                    //    let x = obj.prop[2].x;
                    //    obj.prop[3] = 42;
                    //
                    Err(EvalAltResult::ErrorDotExpr)
                }
                _ => Err(EvalAltResult::ErrorDotExpr),
            },
            _ => Err(EvalAltResult::ErrorDotExpr),
        }
    }

    fn search_scope<'a, F, T>(
        scope: &'a mut Scope,
        id: &str,
        map: F,
    ) -> Result<(usize, T), EvalAltResult>
    where
        F: FnOnce(&'a mut Variant) -> Result<T, EvalAltResult>,
    {
        scope
            .iter_mut()
            .enumerate()
            .rev()
            .find(|&(_, &mut (ref name, _))| id == name)
            .ok_or_else(|| EvalAltResult::ErrorVariableNotFound(id.to_owned()))
            .and_then(move |(idx, &mut (_, ref mut val))| map(val.as_mut()).map(|val| (idx, val)))
    }

    fn indexed_value(
        &self,
        scope: &mut Scope,
        id: &str,
        idx: &Expr,
    ) -> Result<(bool, usize, usize, Dynamic), EvalAltResult> {
        let idx = *self
            .eval_expr(scope, idx)?
            .downcast::<i64>()
            .map_err(|_| EvalAltResult::ErrorIndexExpr)?;

        let mut is_array = false;

        Self::search_scope(scope, id, |val| {
            if let Some(arr) = (*val).downcast_mut() as Option<&mut Array> {
                is_array = true;

                return if idx < 0 {
                    Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx))
                } else {
                    arr.get(idx as usize)
                        .cloned()
                        .ok_or_else(|| EvalAltResult::ErrorArrayBounds(arr.len(), idx))
                };
            }

            if let Some(s) = (*val).downcast_mut() as Option<&mut String> {
                is_array = false;

                return if idx < 0 {
                    Err(EvalAltResult::ErrorStringBounds(s.chars().count(), idx))
                } else {
                    s.chars()
                        .nth(idx as usize)
                        .map(|ch| Box::new(ch) as Dynamic)
                        .ok_or_else(|| EvalAltResult::ErrorStringBounds(s.chars().count(), idx))
                };
            }

            Err(EvalAltResult::ErrorIndexing)
        })
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
            Expr::Identifier(id) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, |x| Ok(x.into_dynamic()))?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1 = target;

                value
            }
            Expr::Index(id, idx_raw) => {
                let (is_array, sc_idx, idx, mut target) = self.indexed_value(scope, id, idx_raw)?;
                let value = self.get_dot_val_helper(scope, target.as_mut(), dot_rhs);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.

                if is_array {
                    scope[sc_idx].1.downcast_mut::<Array>().unwrap()[idx] = target;
                } else {
                    // Target should be a char
                    let new_ch = *target.downcast::<char>().unwrap();

                    // Root should be a String
                    let s = scope[sc_idx].1.downcast_mut::<String>().unwrap();

                    Self::str_replace_char(s, idx, new_ch);
                }

                value
            }
            _ => Err(EvalAltResult::ErrorDotExpr),
        }
    }

    fn set_dot_val_helper(
        &self,
        this_ptr: &mut Variant,
        dot_rhs: &Expr,
        mut source_val: Dynamic,
    ) -> Result<Dynamic, EvalAltResult> {
        match dot_rhs {
            Expr::Identifier(id) => {
                let set_fn_name = "set$".to_string() + id;
                self.call_fn_raw(set_fn_name, vec![this_ptr, source_val.as_mut()])
            }
            Expr::Dot(inner_lhs, inner_rhs) => match **inner_lhs {
                Expr::Identifier(ref id) => {
                    let get_fn_name = "get$".to_string() + id;
                    self.call_fn_raw(get_fn_name, vec![this_ptr])
                        .and_then(|mut v| {
                            self.set_dot_val_helper(v.as_mut(), inner_rhs, source_val)
                                .map(|_| v) // Discard Ok return value
                        })
                        .and_then(|mut v| {
                            let set_fn_name = "set$".to_string() + id;

                            self.call_fn_raw(set_fn_name, vec![this_ptr, v.as_mut()])
                        })
                }
                _ => Err(EvalAltResult::ErrorDotExpr),
            },
            _ => Err(EvalAltResult::ErrorDotExpr),
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
            Expr::Identifier(id) => {
                let (sc_idx, mut target) = Self::search_scope(scope, id, |x| Ok(x.into_dynamic()))?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                scope[sc_idx].1 = target;

                value
            }
            Expr::Index(id, idx_raw) => {
                let (is_array, sc_idx, idx, mut target) = self.indexed_value(scope, id, idx_raw)?;
                let value = self.set_dot_val_helper(target.as_mut(), dot_rhs, source_val);

                // In case the expression mutated `target`, we need to reassign it because
                // of the above `clone`.
                if is_array {
                    scope[sc_idx].1.downcast_mut::<Array>().unwrap()[idx] = target;
                } else {
                    // Target should be a char
                    let new_ch = *target.downcast::<char>().unwrap();

                    // Root should be a String
                    let s = scope[sc_idx].1.downcast_mut::<String>().unwrap();

                    Self::str_replace_char(s, idx, new_ch);
                }

                value
            }
            _ => Err(EvalAltResult::ErrorDotExpr),
        }
    }

    fn eval_expr(&self, scope: &mut Scope, expr: &Expr) -> Result<Dynamic, EvalAltResult> {
        match expr {
            Expr::IntegerConstant(i) => Ok(Box::new(*i)),
            Expr::FloatConstant(i) => Ok(Box::new(*i)),
            Expr::StringConstant(s) => Ok(Box::new(s.clone())),
            Expr::CharConstant(c) => Ok(Box::new(*c)),
            Expr::Identifier(id) => {
                match scope.iter().rev().filter(|(name, _)| id == name).next() {
                    Some((_, val)) => Ok(val.clone()),
                    _ => Err(EvalAltResult::ErrorVariableNotFound(id.clone())),
                }
            }
            Expr::Index(id, idx_raw) => {
                self.indexed_value(scope, id, idx_raw).map(|(_, _, _, x)| x)
            }
            Expr::Assignment(ref id, rhs) => {
                let rhs_val = self.eval_expr(scope, rhs)?;

                match **id {
                    Expr::Identifier(ref n) => {
                        match scope.iter_mut().rev().filter(|(name, _)| n == name).next() {
                            Some((_, val)) => {
                                *val = rhs_val;
                                Ok(Box::new(()))
                            }
                            _ => Err(EvalAltResult::ErrorVariableNotFound(n.clone())),
                        }
                    }
                    Expr::Index(ref id, ref idx_raw) => {
                        let idx = *match self.eval_expr(scope, &idx_raw)?.downcast_ref::<i64>() {
                            Some(x) => x,
                            _ => return Err(EvalAltResult::ErrorIndexExpr),
                        };

                        let variable = &mut scope
                            .iter_mut()
                            .rev()
                            .filter(|(name, _)| id == name)
                            .map(|(_, val)| val)
                            .next();

                        let val = match variable {
                            Some(v) => v,
                            _ => return Err(EvalAltResult::ErrorVariableNotFound(id.clone())),
                        };

                        if let Some(arr) = val.downcast_mut() as Option<&mut Array> {
                            return if idx < 0 {
                                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx))
                            } else if idx as usize >= arr.len() {
                                Err(EvalAltResult::ErrorArrayBounds(arr.len(), idx))
                            } else {
                                arr[idx as usize] = rhs_val;
                                Ok(Box::new(()))
                            };
                        }

                        if let Some(s) = val.downcast_mut() as Option<&mut String> {
                            let s_len = s.chars().count();

                            return if idx < 0 {
                                Err(EvalAltResult::ErrorStringBounds(s_len, idx))
                            } else if idx as usize >= s_len {
                                Err(EvalAltResult::ErrorStringBounds(s_len, idx))
                            } else {
                                // Should be a char
                                let new_ch = *rhs_val.downcast::<char>().unwrap();
                                Self::str_replace_char(s, idx as usize, new_ch);
                                Ok(Box::new(()))
                            };
                        }

                        return Err(EvalAltResult::ErrorIndexExpr);
                    }
                    Expr::Dot(ref dot_lhs, ref dot_rhs) => {
                        self.set_dot_val(scope, dot_lhs, dot_rhs, rhs_val)
                    }
                    _ => Err(EvalAltResult::ErrorAssignmentToUnknownLHS),
                }
            }
            Expr::Dot(lhs, rhs) => self.get_dot_val(scope, lhs, rhs),
            Expr::Array(contents) => {
                let mut arr = Vec::new();

                contents.iter().try_for_each(|item| {
                    let arg = self.eval_expr(scope, item)?;
                    arr.push(arg);
                    Ok(())
                })?;

                Ok(Box::new(arr))
            }
            Expr::FunctionCall(fn_name, args) => self.call_fn_raw(
                fn_name.to_owned(),
                args.iter()
                    .map(|ex| self.eval_expr(scope, ex))
                    .collect::<Result<Array, _>>()?
                    .iter_mut()
                    .map(|b| b.as_mut())
                    .collect(),
            ),
            Expr::True => Ok(Box::new(true)),
            Expr::False => Ok(Box::new(false)),
            Expr::Unit => Ok(Box::new(())),
        }
    }

    fn eval_stmt(&self, scope: &mut Scope, stmt: &Stmt) -> Result<Dynamic, EvalAltResult> {
        match stmt {
            Stmt::Expr(expr) => self.eval_expr(scope, expr),
            Stmt::Block(block) => {
                let prev_len = scope.len();
                let mut last_result: Result<Dynamic, EvalAltResult> = Ok(Box::new(()));

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
            Stmt::If(guard, body) => self
                .eval_expr(scope, guard)?
                .downcast::<bool>()
                .map_err(|_| EvalAltResult::ErrorIfGuard)
                .and_then(|guard_val| {
                    if *guard_val {
                        self.eval_stmt(scope, body)
                    } else {
                        Ok(Box::new(()))
                    }
                }),
            Stmt::IfElse(guard, body, else_body) => self
                .eval_expr(scope, guard)?
                .downcast::<bool>()
                .map_err(|_| EvalAltResult::ErrorIfGuard)
                .and_then(|guard_val| {
                    if *guard_val {
                        self.eval_stmt(scope, body)
                    } else {
                        self.eval_stmt(scope, else_body)
                    }
                }),
            Stmt::While(guard, body) => loop {
                match self.eval_expr(scope, guard)?.downcast::<bool>() {
                    Ok(guard_val) => {
                        if *guard_val {
                            match self.eval_stmt(scope, body) {
                                Err(EvalAltResult::LoopBreak) => return Ok(Box::new(())),
                                Err(x) => return Err(x),
                                _ => (),
                            }
                        } else {
                            return Ok(Box::new(()));
                        }
                    }
                    Err(_) => return Err(EvalAltResult::ErrorIfGuard),
                }
            },
            Stmt::Loop(body) => loop {
                match self.eval_stmt(scope, body) {
                    Err(EvalAltResult::LoopBreak) => return Ok(Box::new(())),
                    Err(x) => return Err(x),
                    _ => (),
                }
            },
            Stmt::For(name, expr, body) => {
                let arr = self.eval_expr(scope, expr)?;
                let tid = Any::type_id(&*arr);
                if let Some(iter_fn) = self.type_iterators.get(&tid) {
                    scope.push((name.clone(), Box::new(())));
                    let idx = scope.len() - 1;
                    for a in iter_fn(&arr) {
                        scope[idx].1 = a;
                        match self.eval_stmt(scope, body) {
                            Err(EvalAltResult::LoopBreak) => break,
                            Err(x) => return Err(x),
                            _ => (),
                        }
                    }
                    scope.remove(idx);
                    Ok(Box::new(()))
                } else {
                    return Err(EvalAltResult::ErrorFor);
                }
            }
            Stmt::Break => Err(EvalAltResult::LoopBreak),
            Stmt::Return => Err(EvalAltResult::Return(Box::new(()))),
            Stmt::ReturnWithVal(a) => {
                let result = self.eval_expr(scope, a)?;
                Err(EvalAltResult::Return(result))
            }
            Stmt::Let(name, init) => {
                if let Some(v) = init {
                    let i = self.eval_expr(scope, v)?;
                    scope.push((name.clone(), i));
                } else {
                    scope.push((name.clone(), Box::new(())));
                }
                Ok(Box::new(()))
            }
        }
    }

    /// Compile a string into an AST
    pub fn compile(input: &str) -> Result<AST, ParseError> {
        let tokens = lex(input);

        let mut peekables = tokens.peekable();
        let tree = parse(&mut peekables);

        tree
    }

    /// Compile a file into an AST
    pub fn compile_file(filename: &str) -> Result<AST, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        if let Ok(mut f) = File::open(filename) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                Self::compile(&contents).map_err(|err| EvalAltResult::ErrorParseError(err))
            } else {
                Err(EvalAltResult::ErrorCantOpenScriptFile(filename.to_owned()))
            }
        } else {
            Err(EvalAltResult::ErrorCantOpenScriptFile(filename.to_owned()))
        }
    }

    /// Evaluate a file
    pub fn eval_file<T: Any + Clone>(&mut self, filename: &str) -> Result<T, EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        if let Ok(mut f) = File::open(filename) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                self.eval::<T>(&contents)
            } else {
                Err(EvalAltResult::ErrorCantOpenScriptFile(filename.to_owned()))
            }
        } else {
            Err(EvalAltResult::ErrorCantOpenScriptFile(filename.to_owned()))
        }
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
        let ast = Self::compile(input).map_err(|err| EvalAltResult::ErrorParseError(err))?;
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
        let mut x: Result<Dynamic, EvalAltResult> = Ok(Box::new(()));

        for f in fns {
            let name = f.name.clone();
            let local_f = f.clone();

            let spec = FnSpec {
                ident: name,
                args: None,
            };

            self.fns.insert(spec, Arc::new(FnIntExt::Int(local_f)));
        }

        for o in os {
            x = match self.eval_stmt(scope, o) {
                Ok(v) => Ok(v),
                Err(e) => return Err(e),
            }
        }

        let x = x?;

        match x.downcast::<T>() {
            Ok(out) => Ok(*out),
            Err(a) => Err(EvalAltResult::ErrorMismatchOutputType((*a).type_name())),
        }
    }

    /// Evaluate a file, but only return errors, if there are any.
    /// Useful for when you don't need the result, but still need
    /// to keep track of possible errors
    pub fn consume_file(&mut self, filename: &str) -> Result<(), EvalAltResult> {
        use std::fs::File;
        use std::io::prelude::*;

        if let Ok(mut f) = File::open(filename) {
            let mut contents = String::new();

            if f.read_to_string(&mut contents).is_ok() {
                if let e @ Err(_) = self.consume(&contents) {
                    e
                } else {
                    Ok(())
                }
            } else {
                Err(EvalAltResult::ErrorCantOpenScriptFile(filename.to_owned()))
            }
        } else {
            Err(EvalAltResult::ErrorCantOpenScriptFile(filename.to_owned()))
        }
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

        let mut peekables = tokens.peekable();
        let tree = parse(&mut peekables);

        match tree {
            Ok(AST(ref os, ref fns)) => {
                for f in fns {
                    if f.params.len() > 6 {
                        return Ok(());
                    }
                    let name = f.name.clone();
                    let local_f = f.clone();

                    let spec = FnSpec {
                        ident: name,
                        args: None,
                    };

                    self.fns.insert(spec, Arc::new(FnIntExt::Int(local_f)));
                }

                for o in os {
                    if let Err(e) = self.eval_stmt(scope, o) {
                        return Err(e);
                    }
                }

                Ok(())
            }
            Err(_) => Err(EvalAltResult::ErrorFunctionArgMismatch),
        }
    }

    /// Make a new engine
    pub fn new() -> Engine {
        let mut engine = Engine {
            fns: HashMap::new(),
            type_iterators: HashMap::new(),
            on_print: Box::new(|x: &str| println!("{}", x)),
            on_debug: Box::new(|x: &str| println!("{}", x)),
        };

        engine.register_builtins();

        engine
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
