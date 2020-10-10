use crate::def_package;
use crate::fn_native::FnPtr;
use crate::plugin::*;

def_package!(crate:BasicFnPackage:"Basic Fn functions.", lib, {
    combine_with_exported_module!(lib, "FnPtr", fn_ptr_functions);
});

#[export_module]
mod fn_ptr_functions {
    #[rhai_fn(name = "name", get = "name")]
    pub fn name(f: &mut FnPtr) -> ImmutableString {
        f.get_fn_name().clone()
    }
}
