#![cfg(feature = "internals")]
use rhai::{
    Dynamic, Engine, EvalAltResult, EvalState, Expr, Imports, LexError, Module, Scope, INT,
};

#[test]
fn test_custom_syntax() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine
        .add_custom_syntax(
            &["do", "$ident$", "$block$", "while", "$expr$"],
            1,
            |engine: &Engine,
             scope: &mut Scope,
             mods: &mut Imports,
             state: &mut EvalState,
             lib: &Module,
             this_ptr: &mut Option<&mut Dynamic>,
             exprs: &[Expr],
             level: usize| {
                let var_name = match exprs.get(0).unwrap() {
                    Expr::Variable(s) => (s.0).0.clone(),
                    _ => unreachable!(),
                };
                let stmt = exprs.get(1).unwrap();
                let expr = exprs.get(2).unwrap();

                scope.push(var_name, 0 as INT);

                loop {
                    engine.eval_expr_from_ast(scope, mods, state, lib, this_ptr, stmt, level)?;

                    if !engine
                        .eval_expr_from_ast(scope, mods, state, lib, this_ptr, expr, level)?
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

    assert!(matches!(
        *engine.add_custom_syntax(&["!"], 0, |_, _, _, _, _, _, _, _| Ok(().into())).expect_err("should error"),
        LexError::ImproperSymbol(s) if s == "!"
    ));

    assert_eq!(
        engine.eval::<INT>(
            r"
                do x { x += 1 } while x < 42;
                x
            "
        )?,
        42
    );

    Ok(())
}
