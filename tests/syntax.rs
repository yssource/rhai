#![cfg(feature = "internals")]
use rhai::{
    Dynamic, Engine, EvalAltResult, EvalState, Expr, Imports, LexError, Module, Scope, INT,
};

#[test]
fn test_custom_syntax() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    // Disable 'while' and make sure it still works with custom syntax
    engine.disable_symbol("while");
    engine.consume("while false {}").expect_err("should error");
    engine.consume("let while = 0")?;

    engine
        .register_custom_syntax(
            &[
                "do", "|", "$ident$", "|", "->", "$block$", "while", "$expr$",
            ],
            1,
            |engine: &Engine,
             scope: &mut Scope,
             mods: &mut Imports,
             state: &mut EvalState,
             lib: &Module,
             this_ptr: &mut Option<&mut Dynamic>,
             inputs: &[Expr],
             level: usize| {
                let var_name = inputs[0].get_variable_name().unwrap().to_string();
                let stmt = inputs.get(1).unwrap();
                let expr = inputs.get(2).unwrap();

                scope.push(var_name, 0 as INT);

                loop {
                    engine.eval_expression_tree(scope, mods, state, lib, this_ptr, stmt, level)?;

                    if !engine
                        .eval_expression_tree(scope, mods, state, lib, this_ptr, expr, level)?
                        .as_bool()
                        .map_err(|_| {
                            EvalAltResult::ErrorBooleanArgMismatch(
                                "do-while".into(),
                                expr.position(),
                            )
                        })?
                    {
                        break;
                    }
                }

                Ok(().into())
            },
        )
        .unwrap();

    // 'while' is now a custom keyword so this it can no longer be a variable
    engine.consume("let while = 0").expect_err("should error");

    assert_eq!(
        engine.eval::<INT>(
            r"
                do |x| -> { x += 1 } while x < 42;
                x
            "
        )?,
        42
    );

    // The first symbol must be an identifier
    assert!(matches!(
        *engine.register_custom_syntax(&["!"], 0, |_, _, _, _, _, _, _, _| Ok(().into())).expect_err("should error"),
        LexError::ImproperSymbol(s) if s == "!"
    ));

    Ok(())
}
