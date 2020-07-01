use crate::def_package;
use crate::fn_native::FnPtr;

def_package!(crate:BasicFnPackage:"Basic Fn functions.", lib, {
    lib.set_fn_1_mut("name", |f: &mut FnPtr| Ok(f.get_fn_name().clone()));
    lib.set_getter_fn("name", |f: &mut FnPtr| Ok(f.get_fn_name().clone()));

});
