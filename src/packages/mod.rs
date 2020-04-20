use crate::any::{Dynamic, Variant};
use crate::engine::{calc_fn_spec, FnAny, FnCallArgs, IteratorFn};
use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{
    any::{type_name, TypeId},
    boxed::Box,
    collections::HashMap,
    ops::Deref,
    rc::Rc,
    sync::Arc,
};

mod arithmetic;
mod array_basic;
mod iter_basic;
mod logic;
mod map_basic;
mod math_basic;
mod pkg_core;
mod pkg_std;
mod string_basic;
mod string_more;
mod time_basic;

pub use arithmetic::ArithmeticPackage;
pub use array_basic::BasicArrayPackage;
pub use iter_basic::BasicIteratorPackage;
pub use logic::LogicPackage;
pub use map_basic::BasicMapPackage;
pub use math_basic::BasicMathPackage;
pub use pkg_core::CorePackage;
pub use pkg_std::StandardPackage;
pub use string_basic::BasicStringPackage;
pub use string_more::MoreStringPackage;
pub use time_basic::BasicTimePackage;

pub trait Package: Deref {
    fn new() -> Self;
    fn init(lib: &mut PackageLibraryStore);
    fn get(&self) -> PackageLibrary;
}

pub type PackageLibraryStore = (HashMap<u64, Box<FnAny>>, HashMap<TypeId, Box<IteratorFn>>);

#[cfg(not(feature = "sync"))]
pub type PackageLibrary = Rc<PackageLibraryStore>;

#[cfg(feature = "sync")]
pub type PackageLibrary = Arc<PackageLibraryStore>;

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

fn create_new_package() -> PackageLibraryStore {
    (HashMap::new(), HashMap::new())
}

fn reg_none<R>(
    lib: &mut PackageLibraryStore,
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
    let hash = calc_fn_spec(fn_name, ([] as [TypeId; 0]).iter().cloned());

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 0, args, pos)?;

        let r = func();
        map_result(r, pos)
    });

    lib.0.insert(hash, f);
}

fn reg_unary<T: Variant + Clone, R>(
    lib: &mut PackageLibraryStore,
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
    //println!("register {}({})", fn_name, type_name::<T>());

    let hash = calc_fn_spec(fn_name, [TypeId::of::<T>()].iter().cloned());

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 1, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut T = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x.clone());
        map_result(r, pos)
    });

    lib.0.insert(hash, f);
}

fn reg_unary_mut<T: Variant + Clone, R>(
    lib: &mut PackageLibraryStore,
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
    //println!("register {}(&mut {})", fn_name, type_name::<T>());

    let hash = calc_fn_spec(fn_name, [TypeId::of::<T>()].iter().cloned());

    let f = Box::new(move |args: &mut FnCallArgs, pos: Position| {
        check_num_args(fn_name, 1, args, pos)?;

        let mut drain = args.iter_mut();
        let x: &mut T = drain.next().unwrap().downcast_mut().unwrap();

        let r = func(x);
        map_result(r, pos)
    });

    lib.0.insert(hash, f);
}

fn reg_binary<A: Variant + Clone, B: Variant + Clone, R>(
    lib: &mut PackageLibraryStore,
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
    //println!("register {}({}, {})", fn_name, type_name::<A>(), type_name::<B>());

    let hash = calc_fn_spec(
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

    lib.0.insert(hash, f);
}

fn reg_binary_mut<A: Variant + Clone, B: Variant + Clone, R>(
    lib: &mut PackageLibraryStore,
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
    //println!("register {}(&mut {}, {})", fn_name, type_name::<A>(), type_name::<B>());

    let hash = calc_fn_spec(
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

    lib.0.insert(hash, f);
}

fn reg_trinary<A: Variant + Clone, B: Variant + Clone, C: Variant + Clone, R>(
    lib: &mut PackageLibraryStore,
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
    //println!("register {}({}, {}, {})", fn_name, type_name::<A>(), type_name::<B>(), type_name::<C>());

    let hash = calc_fn_spec(
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

    lib.0.insert(hash, f);
}

fn reg_trinary_mut<A: Variant + Clone, B: Variant + Clone, C: Variant + Clone, R>(
    lib: &mut PackageLibraryStore,
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
    //println!("register {}(&mut {}, {}, {})", fn_name, type_name::<A>(), type_name::<B>(), type_name::<C>());

    let hash = calc_fn_spec(
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

    lib.0.insert(hash, f);
}
