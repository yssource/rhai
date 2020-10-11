use rhai::{Engine, EvalAltResult, EvalContext, Expression, ParseErrorType, Scope, INT};

#[test]
fn test_custom_syntax() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    engine.consume("while false {}")?;

    // Disable 'while' and make sure it still works with custom syntax
    engine.disable_symbol("while");
    assert!(matches!(
        *engine.compile("while false {}").expect_err("should error").0,
        ParseErrorType::Reserved(err) if err == "while"
    ));
    assert!(matches!(
        *engine.compile("let while = 0").expect_err("should error").0,
        ParseErrorType::Reserved(err) if err == "while"
    ));

    engine.register_custom_syntax(
        &[
            "do", "|", "$ident$", "|", "->", "$block$", "while", "$expr$",
        ],
        1,
        |scope: &mut Scope, context: &mut EvalContext, inputs: &[Expression]| {
            let var_name = inputs[0].get_variable_name().unwrap().to_string();
            let stmt = inputs.get(1).unwrap();
            let condition = inputs.get(2).unwrap();

            scope.push(var_name, 0 as INT);

            loop {
                context.eval_expression_tree(scope, stmt)?;

                let stop = !context
                    .eval_expression_tree(scope, condition)?
                    .as_bool()
                    .map_err(|err| {
                        Box::new(EvalAltResult::ErrorMismatchDataType(
                            "bool".to_string(),
                            err.to_string(),
                            condition.position(),
                        ))
                    })?;

                if stop {
                    break;
                }
            }

            Ok(().into())
        },
    )?;

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
    assert_eq!(
        *engine
            .register_custom_syntax(&["!"], 0, |_, _, _| Ok(().into()))
            .expect_err("should error")
            .0,
        ParseErrorType::BadInput("Improper symbol for custom syntax: '!'".to_string())
    );

    Ok(())
}
