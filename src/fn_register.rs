//! Module which defines the function registration mechanism.

#![allow(non_snake_case)]

use crate::any::{Dynamic, Variant};
use crate::engine::Engine;
use crate::fn_native::{CallableFunction, FnAny, FnCallArgs};
use crate::parser::FnAccess;
use crate::result::EvalAltResult;

use crate::stdlib::{any::TypeId, boxed::Box, mem};

/// A trait to register custom plugins with the `Engine`.
///
/// A plugin consists of a number of functions. All functions will be registered with the engine.
#[cfg(feature = "plugins")]
pub trait RegisterPlugin<PL: Plugin> {
    /// Register a custom function with the `Engine`.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Engine, INT, Plugin, RegisterDynamicFn, RegisterPlugin};
    ///
    /// // A simple custom plugin type. This should not usually be done with hand-written code.
    /// struct AddOffsetPlugin(INT);
    /// impl AddOffsetPlugin {
    ///     fn add_offset(&self, x: INT) -> Dynamic {
    ///         Dynamic::from(x + self.0)
    ///     }
    /// }
    /// impl Plugin for AddOffsetPlugin {
    ///     fn name(&self) -> &str {
    ///         "My Plugin"
    ///     }
    ///     fn register_contents(self, engine: &mut Engine) {
    ///         let add_offset_fn: Box<dyn Fn(INT) -> Dynamic> = {
    ///             Box::new(move |x| self.add_offset(x))
    ///         };
    ///         engine.register_dynamic_fn("add_offset", add_offset_fn);
    ///     }
    /// }
    ///
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    ///
    /// let mut engine = Engine::new();
    /// engine.register_plugin(AddOffsetPlugin(50));
    ///
    /// assert_eq!(engine.eval::<i64>("add_offset(42)")?, 92);
    /// # Ok(())
    /// # }
    /// ```
    fn register_plugin(&mut self, plugin: PL);
}

/// Trait to register custom functions with the `Engine`.
pub trait RegisterFn<FN, ARGS, RET> {
    /// Register a custom function with the `Engine`.
    ///
    /// # Example
    ///
    /// ```
    /// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
    /// use rhai::{Engine, RegisterFn};
    ///
    /// // Normal function
    /// fn add(x: i64, y: i64) -> i64 {
    ///     x + y
    /// }
    ///
    /// let mut engine = Engine::new();
    ///
    /// // You must use the trait rhai::RegisterFn to get this method.
    /// engine.register_fn("add", add);
    ///
    /// assert_eq!(engine.eval::<i64>("add(40, 2)")?, 42);
    ///
    /// // You can also register a closure.
    /// engine.register_fn("sub", |x: i64, y: i64| x - y );
    ///
    /// assert_eq!(engine.eval::<i64>("sub(44, 2)")?, 42);
    /// # Ok(())
    /// # }
    /// ```
    fn register_fn(&mut self, name: &str, f: FN);
}

/// Trait to register fallible custom functions returning `Result<Dynamic, Box<EvalAltResult>>` with the `Engine`.
pub trait RegisterResultFn<FN, ARGS> {
    /// Register a custom fallible function with the `Engine`.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Engine, Dynamic, RegisterResultFn, EvalAltResult};
    ///
    /// // Normal function
    /// fn div(x: i64, y: i64) -> Result<Dynamic, Box<EvalAltResult>> {
    ///     if y == 0 {
    ///         // '.into()' automatically converts to 'Box<EvalAltResult::ErrorRuntime>'
    ///         Err("division by zero!".into())
    ///     } else {
    ///         Ok((x / y).into())
    ///     }
    /// }
    ///
    /// let mut engine = Engine::new();
    ///
    /// // You must use the trait rhai::RegisterResultFn to get this method.
    /// engine.register_result_fn("div", div);
    ///
    /// engine.eval::<i64>("div(42, 0)")
    ///         .expect_err("expecting division by zero error!");
    /// ```
    fn register_result_fn(&mut self, name: &str, f: FN);
}

/// Represents an externally-written plugin for the Rhai interpreter.
///
/// This should not be used directly. Use the `plugin_module!` macro instead.
#[cfg(feature = "plugins")]
pub trait Plugin {
    fn register_contents(self, engine: &mut Engine);
    fn name(&self) -> &str;
}

// These types are used to build a unique _marker_ tuple type for each combination
// of function parameter types in order to make each trait implementation unique.
// That is because stable Rust currently does not allow distinguishing implementations
// based purely on parameter types of traits (Fn, FnOnce and FnMut).
//
// For example:
//
// `RegisterFn<FN, (Mut<A>, B, Ref<C>), R>`
//
// will have the function prototype constraint to:
//
// `FN: (&mut A, B, &C) -> R`
//
// These types are not actually used anywhere.
pub struct Mut<T>(T);
//pub struct Ref<T>(T);

/// Dereference into &mut.
#[inline(always)]
pub fn by_ref<T: Variant + Clone>(data: &mut Dynamic) -> &mut T {
    // Directly cast the &mut Dynamic into &mut T to access the underlying data.
    data.downcast_mut::<T>().unwrap()
}

/// Dereference into value.
#[inline(always)]
pub fn by_value<T: Variant + Clone>(data: &mut Dynamic) -> T {
    // We consume the argument and then replace it with () - the argument is not supposed to be used again.
    // This way, we avoid having to clone the argument again, because it is already a clone when passed here.
    mem::take(data).cast::<T>()
}

#[cfg(feature = "plugins")]
impl<PL: Plugin> RegisterPlugin<PL> for Engine {
    fn register_plugin(&mut self, plugin: PL) {
        plugin.register_contents(self)
    }
}

/// This macro creates a closure wrapping a registered function.
macro_rules! make_func {
	($fn:ident : $map:expr ; $($par:ident => $convert:expr),*) => {
//   ^ function pointer
//               ^ result mapping function
//                           ^ function parameter generic type name (A, B, C etc.)
//                                           ^ dereferencing function

		Box::new(move |args: &mut FnCallArgs| {
            // The arguments are assumed to be of the correct number and types!

			#[allow(unused_variables, unused_mut)]
			let mut drain = args.iter_mut();
			$(
			// Downcast every element, panic in case of a type mismatch (which shouldn't happen).
			// Call the user-supplied function using ($convert) to access it either by value or by reference.
			let $par = ($convert)(drain.next().unwrap());
			)*

            // Call the function with each parameter value
			let r = $fn($($par),*);

            // Map the result
            $map(r)
		}) as Box<FnAny>
	};
}

/// To Dynamic mapping function.
#[inline(always)]
pub fn map_dynamic<T: Variant + Clone>(data: T) -> Result<Dynamic, Box<EvalAltResult>> {
    Ok(data.into_dynamic())
}

/// To Dynamic mapping function.
#[inline(always)]
pub fn map_result(
    data: Result<Dynamic, Box<EvalAltResult>>,
) -> Result<Dynamic, Box<EvalAltResult>> {
    data
}

macro_rules! def_register {
    () => {
        def_register!(imp from_pure :);
    };
    (imp $abi:ident : $($par:ident => $mark:ty => $param:ty => $clone:expr),*) => {
    //   ^ function ABI type
    //                  ^ function parameter generic type name (A, B, C etc.)
    //                                ^ function parameter marker type (T, Ref<T> or Mut<T>)
    //                                            ^ function parameter actual type (T, &T or &mut T)
    //                                                         ^ dereferencing function
        impl<
            $($par: Variant + Clone,)*

            #[cfg(feature = "sync")]
            FN: Fn($($param),*) -> RET + Send + Sync + 'static,

            #[cfg(not(feature = "sync"))]
            FN: Fn($($param),*) -> RET + 'static,

            RET: Variant + Clone
        > RegisterFn<FN, ($($mark,)*), RET> for Engine
        {
            fn register_fn(&mut self, name: &str, f: FN) {
                self.global_module.set_fn(name, FnAccess::Public,
                    &[$(TypeId::of::<$par>()),*],
                    CallableFunction::$abi(make_func!(f : map_dynamic ; $($par => $clone),*))
                );
            }
        }

        impl<
            $($par: Variant + Clone,)*

            #[cfg(feature = "sync")]
            FN: Fn($($param),*) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync + 'static,
            #[cfg(not(feature = "sync"))]
            FN: Fn($($param),*) -> Result<Dynamic, Box<EvalAltResult>> + 'static,
        > RegisterResultFn<FN, ($($mark,)*)> for Engine
        {
            fn register_result_fn(&mut self, name: &str, f: FN) {
                self.global_module.set_fn(name, FnAccess::Public,
                    &[$(TypeId::of::<$par>()),*],
                    CallableFunction::$abi(make_func!(f : map_result ; $($par => $clone),*))
                );
            }
        }

        //def_register!(imp_pop $($par => $mark => $param),*);
    };
    ($p0:ident $(, $p:ident)*) => {
        def_register!(imp from_pure   : $p0 => $p0      => $p0      => by_value $(, $p => $p => $p => by_value)*);
        def_register!(imp from_method : $p0 => Mut<$p0> => &mut $p0 => by_ref   $(, $p => $p => $p => by_value)*);
        //                ^ CallableFunction
        // handle the first parameter                                  ^ first parameter passed through
        //                                                                                            ^ others passed by value (by_value)

        // Currently does not support first argument which is a reference, as there will be
        // conflicting implementations since &T: Any and T: Any cannot be distinguished
        //def_register!(imp $p0 => Ref<$p0> => &$p0     => by_ref   $(, $p => $p => $p => by_value)*);

        def_register!($($p),*);
    };
}

def_register!(A, B, C, D, E, F, G, H, J, K, L, M, N, P, Q, R, S, T, U, V);
