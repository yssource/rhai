use crate::def_package;
use crate::fn_native::FnPtr;

def_package!(crate:BasicFnPackage:"Basic Fn functions.", lib, {
    lib.set_fn_1_mut("name", |f: &mut FnPtr| Ok(f.get_fn_name().clone()));

    #[cfg(not(feature = "no_object"))]
    lib.set_getter_fn("name", |f: &mut FnPtr| Ok(f.get_fn_name().clone()));
});
