use rhai::{Engine, EvalAltResult, Scope, AST};

#[cfg(not(feature = "no_optimize"))]
use rhai::OptimizationLevel;

use std::{
    io::{stdin, stdout, Write},
    iter,
};

fn print_error(input: &str, err: EvalAltResult) {
    fn padding(pad: &str, len: usize) -> String {
        iter::repeat(pad).take(len).collect::<String>()
    }

    let lines: Vec<_> = input.split("\n").collect();

    // Print error
    match err.position() {
        p if p.is_eof() => {
            // EOF
            let last = lines[lines.len() - 2];
            println!("{}", last);
            println!("{}^ {}", padding(" ", last.len() - 1), err);
        }
        p if p.is_none() => {
            // No position
            println!("{}", err);
        }
        p => {
            // Specific position
            let pos_text = format!(
                " (line {}, position {})",
                p.line().unwrap(),
                p.position().unwrap()
            );

            println!("{}", lines[p.line().unwrap() - 1]);

            let err_text = match err {
                EvalAltResult::ErrorRuntime(err, _) if !err.is_empty() => {
                    format!("Runtime error: {}", err)
                }
                _ => err.to_string(),
            };

            println!(
                "{}^ {}",
                padding(" ", p.position().unwrap() - 1),
                err_text.replace(&pos_text, "")
            );
        }
    }
}

fn main() {
    let mut engine = Engine::new();

    #[cfg(not(feature = "no_optimize"))]
    engine.set_optimization_level(OptimizationLevel::Full);

    let mut scope = Scope::new();

    let mut input = String::new();
    let mut ast: Option<AST> = None;

    loop {
        print!("rhai> ");
        stdout().flush().expect("couldn't flush stdout");

        input.clear();

        if let Err(err) = stdin().read_line(&mut input) {
            println!("input error: {}", err);
        }

        // Implement standard commands
        match input.as_str().trim() {
            "exit" | "quit" => break, // quit
            "ast" => {
                if matches!(&ast, Some(_)) {
                    // print the last AST
                    println!("{:#?}", ast.as_ref().unwrap());
                } else {
                    println!("()");
                }
                continue;
            }
            _ => (),
        }

        if let Err(err) = engine
            .compile_with_scope(&scope, &input)
            .map_err(EvalAltResult::ErrorParsing)
            .and_then(|r| {
                ast = Some(r);
                engine
                    .consume_ast_with_scope(&mut scope, true, ast.as_ref().unwrap())
                    .or_else(|err| match err {
                        EvalAltResult::Return(_, _) => Ok(()),
                        err => Err(err),
                    })
            })
        {
            println!("");
            print_error(&input, err);
            println!("");
        }
    }
}
