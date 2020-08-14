use crate::any::Dynamic;
use crate::def_package;
use crate::parser::ImmutableString;
use crate::plugin::*;
use crate::result::EvalAltResult;

#[export_fn(return_raw)]
fn eval_override(_script: ImmutableString) -> Result<Dynamic, Box<EvalAltResult>> {
    Err("eval is evil!".into())
}

def_package!(crate:EvalPackage:"Disable 'eval'.", lib, {
    set_exported_fn!(lib, "eval", eval_override);
});
