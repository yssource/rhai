use super::PackageStore;

use crate::any::{Dynamic, Variant};
use crate::calc_fn_hash;
use crate::engine::FnCallArgs;
use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{any::TypeId, boxed::Box};

/// This macro makes it easy to define a _package_ and register functions into it.
///
/// Functions can be added to the package using a number of helper functions under the `packages` module,
/// such as `reg_unary`, `reg_binary_mut`, `reg_trinary_mut` etc.
///
/// ```,ignore
/// use rhai::def_package;
/// use rhai::packages::reg_binary;
///
/// fn add(x: i64, y: i64) { x + y }
///
/// def_package!(MyPackage:"My super-duper package", lib,
/// {
///     reg_binary(lib, "my_add", add, |v| Ok(v.into_dynamic()));
///     //                                 ^^^^^^^^^^^^^^^^^^^^
///     //                                 map into Result<Dynamic, EvalAltResult>
/// });
/// ```
///
/// The above defines a package named 'MyPackage' with a single function named 'my_add'.
#[macro_export]
macro_rules! def_package {
    ($package:ident : $comment:expr , $lib:ident , $block:stmt) => {
        #[doc=$comment]
        pub struct $package(super::PackageLibrary);

        impl crate::packages::Package for $package {
            fn new() -> Self {
                let mut pkg = crate::packages::PackageStore::new();
                Self::init(&mut pkg);
                Self(pkg.into())
            }

            fn get(&self) -> crate::packages::PackageLibrary {
                self.0.clone()
            }

            fn init($lib: &mut crate::packages::PackageStore) {
                $block
            }
        }
    };
}

fn check_num_args(
    name: &str,
    num_args: usize,
    args: &mut FnCallArgs,
    pos: Position,
) -> Result<(), Box<EvalAltResult>> {
    if args.len() != num_args {
        Err(Box::new(EvalAltResult::ErrorFunctionArgsMismatch(
            name.to_string(),
            num_args,
            args.len(),
            pos,
        )))
    } else {
        Ok(())
    }
}

/// Add a function with no parameters to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_none<R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn() -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn() -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    let hash = calc_fn_hash(fn_name, ([] as [TypeId; 0]).iter().cloned());

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 0, args, pos)?;

        let r = func();
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

/// Add a function with one parameter to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_unary<T: Variant + Clone, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(T) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(T) -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}({})", fn_name, crate::std::any::type_name::<T>());

    let hash = calc_fn_hash(fn_name, [TypeId::of::<T>()].iter().cloned());

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 1, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut T = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x.clone());
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

/// Add a function with one mutable reference parameter to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_unary_mut<T: Variant + Clone, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(&mut T) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(&mut T) -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}(&mut {})", fn_name, crate::std::any::type_name::<T>());

    let hash = calc_fn_hash(fn_name, [TypeId::of::<T>()].iter().cloned());

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 1, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut T = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x);
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

pub(crate) fn reg_test<'a, A: Variant + Clone, B: Variant + Clone, X, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(X, B) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(X, B) -> R + Send + Sync + 'static,

    map: impl Fn(&mut A) -> X + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}({}, {})", fn_name, crate::std::any::type_name::<A>(), crate::std::any::type_name::<B>());

    let hash = calc_fn_hash(
        fn_name,
        [TypeId::of::<A>(), TypeId::of::<B>()].iter().cloned(),
    );

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 2, args, pos)?;

        let mut drain = args.iter_mut();
        let x: X = map(drain.next().unwrap().downcast_mut::<A>().unwrap());
        let y: B = drain.next().unwrap().downcast_mut::<B>().unwrap().clone();

        let r = func(x, y);
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

/// Add a function with two parameters to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_binary<A: Variant + Clone, B: Variant + Clone, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(A, B) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(A, B) -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}({}, {})", fn_name, crate::std::any::type_name::<A>(), crate::std::any::type_name::<B>());

    let hash = calc_fn_hash(
        fn_name,
        [TypeId::of::<A>(), TypeId::of::<B>()].iter().cloned(),
    );

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 2, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut A = drain.next().unwrap().downcast_mut().unwrap();
        let y: &mut B = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x.clone(), y.clone());
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

/// Add a function with two parameters (the first one being a mutable reference) to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_binary_mut<A: Variant + Clone, B: Variant + Clone, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(&mut A, B) -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}(&mut {}, {})", fn_name, crate::std::any::type_name::<A>(), crate::std::any::type_name::<B>());

    let hash = calc_fn_hash(
        fn_name,
        [TypeId::of::<A>(), TypeId::of::<B>()].iter().cloned(),
    );

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 2, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut A = drain.next().unwrap().downcast_mut().unwrap();
        let y: &mut B = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x, y.clone());
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

/// Add a function with three parameters to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_trinary<A: Variant + Clone, B: Variant + Clone, C: Variant + Clone, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(A, B, C) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(A, B, C) -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}({}, {}, {})", fn_name, crate::std::any::type_name::<A>(), crate::std::any::type_name::<B>(), crate::std::any::type_name::<C>());

    let hash = calc_fn_hash(
        fn_name,
        [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()]
            .iter()
            .cloned(),
    );

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 3, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut A = drain.next().unwrap().downcast_mut().unwrap();
        let y: &mut B = drain.next().unwrap().downcast_mut().unwrap();
        let z: &mut C = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x.clone(), y.clone(), z.clone());
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}

/// Add a function with three parameters (the first one is a mutable reference) to the package.
///
/// `map_result` is a function that maps the return type of the function to `Result<Dynamic, EvalAltResult>`.
pub fn reg_trinary_mut<A: Variant + Clone, B: Variant + Clone, C: Variant + Clone, R>(
    lib: &mut PackageStore,
    fn_name: &'static str,

    #[cfg(not(feature = "sync"))] func: impl Fn(&mut A, B, C) -> R + 'static,
    #[cfg(feature = "sync")] func: impl Fn(&mut A, B, C) -> R + Send + Sync + 'static,

    #[cfg(not(feature = "sync"))] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + 'static,
    #[cfg(feature = "sync")] map_result: impl Fn(R, Position) -> Result<Dynamic, Box<EvalAltResult>>
        + Send
        + Sync
        + 'static,
) {
    //println!("register {}(&mut {}, {}, {})", fn_name, crate::std::any::type_name::<A>(), crate::std::any::type_name::<B>(), crate::std::any::type_name::<C>());

    let hash = calc_fn_hash(
        fn_name,
        [TypeId::of::<A>(), TypeId::of::<B>(), TypeId::of::<C>()]
            .iter()
            .cloned(),
    );

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 3, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut A = drain.next().unwrap().downcast_mut().unwrap();
        let y: &mut B = drain.next().unwrap().downcast_mut().unwrap();
        let z: &mut C = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x, y.clone(), z.clone());
        map_result(r, pos)
    });

    lib.functions.insert(hash, f);
}
