use super::arithmetic::ArithmeticPackage;
use super::fn_basic::BasicFnPackage;
use super::iter_basic::BasicIteratorPackage;
use super::logic::LogicPackage;
use super::string_basic::BasicStringPackage;

use crate::fn_native::{CallableFunction, FnCallArgs};
use crate::stdlib::{any::TypeId, iter::empty};
use crate::{
    calc_script_fn_hash, def_package, FnAccess, FnNamespace, ImmutableString, NativeCallContext,
    INT,
};

def_package!(crate:CorePackage:"_Core_ package containing basic facilities.", lib, {
    #[cfg(not(feature = "no_function"))]
    {
        let f = |ctx: NativeCallContext, args: &mut FnCallArgs| {
            let num_params = args[1].clone().cast::<INT>();
            let fn_name = args[0].as_str().unwrap();

            Ok(if num_params < 0 {
                false.into()
            } else {
                let hash_script = calc_script_fn_hash(empty(), fn_name, num_params as usize);
                ctx.engine().has_override(ctx.mods, ctx.lib, 0, hash_script, true).into()
            })
        };

        lib.set_fn("is_def_fn", FnNamespace::Global, FnAccess::Public,
            Some(&["fn_name: &str", "num_params: INT"]),
            &[TypeId::of::<ImmutableString>(), TypeId::of::<INT>()],
            CallableFunction::from_method(Box::new(f)));
    }

    ArithmeticPackage::init(lib);
    LogicPackage::init(lib);
    BasicStringPackage::init(lib);
    BasicIteratorPackage::init(lib);
    BasicFnPackage::init(lib);
});
