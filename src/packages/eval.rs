use crate::any::Dynamic;
use crate::def_package;
use crate::parser::ImmutableString;
use crate::plugin::*;
use crate::result::EvalAltResult;

def_package!(crate:EvalPackage:"Disable 'eval'.", lib, {
    combine_with_exported_module!(lib, "eval", eval_override);
});

#[export_module]
mod eval_override {
    #[rhai_fn(return_raw)]
    pub fn eval(_script: ImmutableString) -> Result<Dynamic, Box<EvalAltResult>> {
        Err("eval is evil!".into())
    }
}
