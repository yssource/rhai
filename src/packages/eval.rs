use crate::def_package;
use crate::module::FuncReturn;
use crate::parser::ImmutableString;

def_package!(crate:EvalPackage:"Disable 'eval'.", lib, {
    lib.set_fn_1(
        "eval",
        |_: ImmutableString| -> FuncReturn<()> {
            Err("eval is evil!".into())
        },
    );
});
