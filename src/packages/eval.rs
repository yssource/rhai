use crate::def_package;
use crate::module::FuncReturn;
use crate::stdlib::string::String;

def_package!(crate:EvalPackage:"Disable 'eval'.", lib, {
    lib.set_fn_1_mut(
        "eval",
        |_: &mut String| -> FuncReturn<()> {
            Err("eval is evil!".into())
        },
    );
});
