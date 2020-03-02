use crate::{any::Any, Array, Dynamic, Engine, RegisterDynamicFn, RegisterFn};
use std::fmt::{Debug, Display};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Range, Rem, Shl, Shr, Sub};

macro_rules! reg_op {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y, y: $y)->$y);
        )*
    )
}

macro_rules! reg_un {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y)->$y);
        )*
    )
}

macro_rules! reg_cmp {
    ($self:expr, $x:expr, $op:expr, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y, y: $y)->bool);
        )*
    )
}

macro_rules! reg_func1 {
    ($self:expr, $x:expr, $op:expr, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $y)->$r);
        )*
    )
}

macro_rules! reg_func2x {
    ($self:expr, $x:expr, $op:expr, $v:ty, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $v, y: $y)->$r);
        )*
    )
}

macro_rules! reg_func2y {
    ($self:expr, $x:expr, $op:expr, $v:ty, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(y: $y, x: $v)->$r);
        )*
    )
}

macro_rules! reg_func3 {
    ($self:expr, $x:expr, $op:expr, $v:ty, $w:ty, $r:ty, $( $y:ty ),*) => (
        $(
            $self.register_fn($x, $op as fn(x: $v, y: $w, z: $y)->$r);
        )*
    )
}

impl Engine {
    /// Register the built-in library.
    pub(crate) fn register_builtins(&mut self) {
        fn add<T: Add>(x: T, y: T) -> <T as Add>::Output {
            x + y
        }
        fn sub<T: Sub>(x: T, y: T) -> <T as Sub>::Output {
            x - y
        }
        fn mul<T: Mul>(x: T, y: T) -> <T as Mul>::Output {
            x * y
        }
        fn div<T: Div>(x: T, y: T) -> <T as Div>::Output {
            x / y
        }
        fn neg<T: Neg>(x: T) -> <T as Neg>::Output {
            -x
        }
        fn lt<T: PartialOrd>(x: T, y: T) -> bool {
            x < y
        }
        fn lte<T: PartialOrd>(x: T, y: T) -> bool {
            x <= y
        }
        fn gt<T: PartialOrd>(x: T, y: T) -> bool {
            x > y
        }
        fn gte<T: PartialOrd>(x: T, y: T) -> bool {
            x >= y
        }
        fn eq<T: PartialEq>(x: T, y: T) -> bool {
            x == y
        }
        fn ne<T: PartialEq>(x: T, y: T) -> bool {
            x != y
        }
        fn and(x: bool, y: bool) -> bool {
            x && y
        }
        fn or(x: bool, y: bool) -> bool {
            x || y
        }
        fn not(x: bool) -> bool {
            !x
        }
        fn concat(x: String, y: String) -> String {
            x + &y
        }
        fn binary_and<T: BitAnd>(x: T, y: T) -> <T as BitAnd>::Output {
            x & y
        }
        fn binary_or<T: BitOr>(x: T, y: T) -> <T as BitOr>::Output {
            x | y
        }
        fn binary_xor<T: BitXor>(x: T, y: T) -> <T as BitXor>::Output {
            x ^ y
        }
        fn left_shift<T: Shl<T>>(x: T, y: T) -> <T as Shl<T>>::Output {
            x.shl(y)
        }
        fn right_shift<T: Shr<T>>(x: T, y: T) -> <T as Shr<T>>::Output {
            x.shr(y)
        }
        fn modulo<T: Rem<T>>(x: T, y: T) -> <T as Rem<T>>::Output {
            x % y
        }
        fn pow_i64_i64(x: i64, y: i64) -> i64 {
            x.pow(y as u32)
        }
        fn pow_f64_f64(x: f64, y: f64) -> f64 {
            x.powf(y)
        }
        fn pow_f64_i64(x: f64, y: i64) -> f64 {
            x.powi(y as i32)
        }
        fn unit_eq(_a: (), _b: ()) -> bool {
            true
        }

        reg_op!(self, "+", add, i32, i64, u32, u64, f32, f64);
        reg_op!(self, "-", sub, i32, i64, u32, u64, f32, f64);
        reg_op!(self, "*", mul, i32, i64, u32, u64, f32, f64);
        reg_op!(self, "/", div, i32, i64, u32, u64, f32, f64);

        reg_cmp!(self, "<", lt, i32, i64, u32, u64, String, char, f32, f64);
        reg_cmp!(self, "<=", lte, i32, i64, u32, u64, String, char, f32, f64);
        reg_cmp!(self, ">", gt, i32, i64, u32, u64, String, char, f32, f64);
        reg_cmp!(self, ">=", gte, i32, i64, u32, u64, String, char, f32, f64);
        reg_cmp!(self, "==", eq, i32, i64, u32, u64, bool, String, char, f32, f64);
        reg_cmp!(self, "!=", ne, i32, i64, u32, u64, bool, String, char, f32, f64);

        //reg_op!(self, "||", or, bool);
        //reg_op!(self, "&&", and, bool);
        reg_op!(self, "|", binary_or, i32, i64, u32, u64);
        reg_op!(self, "|", or, bool);
        reg_op!(self, "&", binary_and, i32, i64, u32, u64);
        reg_op!(self, "&", and, bool);
        reg_op!(self, "^", binary_xor, i32, i64, u32, u64);
        reg_op!(self, "<<", left_shift, i32, i64, u32, u64);
        reg_op!(self, ">>", right_shift, i32, i64, u32, u64);
        reg_op!(self, "%", modulo, i32, i64, u32, u64);
        self.register_fn("~", pow_i64_i64);
        self.register_fn("~", pow_f64_f64);
        self.register_fn("~", pow_f64_i64);

        reg_un!(self, "-", neg, i32, i64, f32, f64);
        reg_un!(self, "!", not, bool);

        self.register_fn("+", concat);
        self.register_fn("==", unit_eq);

        // self.register_fn("[]", idx);
        // FIXME?  Registering array lookups are a special case because we want to return boxes
        // directly let ent = self.fns.entry("[]".to_string()).or_insert_with(Vec::new);
        // (*ent).push(FnType::ExternalFn2(Box::new(idx)));

        // Register conversion functions
        self.register_fn("to_float", |x: i32| x as f64);
        self.register_fn("to_float", |x: u32| x as f64);
        self.register_fn("to_float", |x: i64| x as f64);
        self.register_fn("to_float", |x: u64| x as f64);
        self.register_fn("to_float", |x: f32| x as f64);

        self.register_fn("to_int", |x: i32| x as i64);
        self.register_fn("to_int", |x: u32| x as i64);
        self.register_fn("to_int", |x: u64| x as i64);
        self.register_fn("to_int", |x: f32| x as i64);
        self.register_fn("to_int", |x: f64| x as i64);
        self.register_fn("to_int", |ch: char| ch as i64);

        // Register print and debug
        fn print_debug<T: Debug>(x: T) -> String {
            format!("{:?}", x)
        }
        fn print<T: Display>(x: T) -> String {
            format!("{}", x)
        }
        fn println() -> String {
            "\n".to_string()
        }

        reg_func1!(self, "print", print, String, i32, i64, u32, u64);
        reg_func1!(self, "print", print, String, f32, f64, bool, char, String);
        reg_func1!(self, "print", print_debug, String, Array);
        self.register_fn("print", println);

        reg_func1!(self, "debug", print_debug, String, i32, i64, u32, u64);
        reg_func1!(self, "debug", print_debug, String, f32, f64, bool, char);
        reg_func1!(self, "debug", print_debug, String, String, Array, ());

        // Register array utility functions
        fn push<T: Any>(list: &mut Array, item: T) {
            list.push(Box::new(item));
        }
        fn pad<T: Any + Clone>(list: &mut Array, len: i64, item: T) {
            if len >= 0 {
                while list.len() < len as usize {
                    push(list, item.clone());
                }
            }
        }

        reg_func2x!(self, "push", push, &mut Array, (), i32, i64, u32, u64);
        reg_func2x!(self, "push", push, &mut Array, (), f32, f64, bool, char);
        reg_func2x!(self, "push", push, &mut Array, (), String, Array, ());
        reg_func3!(self, "pad", pad, &mut Array, i64, (), i32, u32, f32);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), i64, u64, f64);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), bool, char);
        reg_func3!(self, "pad", pad, &mut Array, i64, (), String, Array, ());

        self.register_dynamic_fn("pop", |list: &mut Array| list.pop().unwrap_or(Box::new(())));
        self.register_dynamic_fn("shift", |list: &mut Array| {
            if list.len() > 0 {
                list.remove(0)
            } else {
                Box::new(())
            }
        });
        self.register_fn("len", |list: &mut Array| list.len() as i64);
        self.register_fn("clear", |list: &mut Array| list.clear());
        self.register_fn("truncate", |list: &mut Array, len: i64| {
            if len >= 0 {
                list.truncate(len as usize);
            }
        });

        // Register string concatenate functions
        fn prepend<T: Display>(x: T, y: String) -> String {
            format!("{}{}", x, y)
        }
        fn append<T: Display>(x: String, y: T) -> String {
            format!("{}{}", x, y)
        }

        reg_func2x!(self, "+", append, String, String, i32, i64, u32, u64, f32, f64, bool, char);
        self.register_fn("+", |x: String, y: Array| format!("{}{:?}", x, y));
        self.register_fn("+", |x: String, _: ()| format!("{}", x));

        reg_func2y!(self, "+", prepend, String, String, i32, i64, u32, u64, f32, f64, bool, char);
        self.register_fn("+", |x: Array, y: String| format!("{:?}{}", x, y));
        self.register_fn("+", |_: (), y: String| format!("{}", y));

        // Register string utility functions
        self.register_fn("len", |s: &mut String| s.chars().count() as i64);
        self.register_fn("contains", |s: &mut String, ch: char| s.contains(ch));
        self.register_fn("contains", |s: &mut String, find: String| s.contains(&find));
        self.register_fn("clear", |s: &mut String| s.clear());
        self.register_fn("truncate", |s: &mut String, len: i64| {
            if len >= 0 {
                let chars: Vec<_> = s.chars().take(len as usize).collect();
                s.clear();
                chars.iter().for_each(|&ch| s.push(ch));
            } else {
                s.clear();
            }
        });
        self.register_fn("pad", |s: &mut String, len: i64, ch: char| {
            for _ in 0..s.chars().count() - len as usize {
                s.push(ch);
            }
        });
        self.register_fn("replace", |s: &mut String, find: String, sub: String| {
            let new_str = s.replace(&find, &sub);
            s.clear();
            s.push_str(&new_str);
        });
        self.register_fn("trim", |s: &mut String| {
            let trimmed = s.trim();

            if trimmed.len() < s.len() {
                let chars: Vec<_> = trimmed.chars().collect();
                s.clear();
                chars.iter().for_each(|&ch| s.push(ch));
            }
        });

        // Register array iterator
        self.register_iterator::<Array, _>(|a| {
            Box::new(a.downcast_ref::<Array>().unwrap().clone().into_iter())
        });

        // Register range function
        self.register_iterator::<Range<i64>, _>(|a| {
            Box::new(
                a.downcast_ref::<Range<i64>>()
                    .unwrap()
                    .clone()
                    .map(|n| Box::new(n) as Dynamic),
            )
        });

        self.register_fn("range", |i1: i64, i2: i64| (i1..i2));
    }
}
