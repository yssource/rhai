use crate::plugin::*;
use crate::{def_package, FnPtr};

def_package!(crate:BasicFnPackage:"Basic Fn functions.", lib, {
    combine_with_exported_module!(lib, "FnPtr", fn_ptr_functions);
});

#[export_module]
mod fn_ptr_functions {
    #[rhai_fn(name = "name", get = "name")]
    pub fn name(f: &mut FnPtr) -> ImmutableString {
        f.get_fn_name().clone()
    }

    #[cfg(not(feature = "no_function"))]
    pub mod anonymous {
        #[rhai_fn(name = "is_anonymous", get = "is_anonymous")]
        pub fn is_anonymous(f: &mut FnPtr) -> bool {
            f.is_anonymous()
        }
    }
}
