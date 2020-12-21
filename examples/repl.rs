use rhai::{Dynamic, Engine, EvalAltResult, Scope, AST};

#[cfg(not(feature = "no_optimize"))]
use rhai::OptimizationLevel;

use std::io::{stdin, stdout, Write};

fn print_error(input: &str, err: EvalAltResult) {
    let lines: Vec<_> = input.trim().split('\n').collect();
    let pos = err.position();

    let line_no = if lines.len() > 1 {
        if pos.is_none() {
            "".to_string()
        } else {
            format!("{}: ", pos.line().unwrap())
        }
    } else {
        "".to_string()
    };

    // Print error
    let pos_text = format!(" ({})", pos);

    if pos.is_none() {
        // No position
        println!("{}", err);
    } else {
        // Specific position
        println!("{}{}", line_no, lines[pos.line().unwrap() - 1]);

        println!(
            "{0:>1$} {2}",
            "^",
            line_no.len() + pos.position().unwrap(),
            err.to_string().replace(&pos_text, "")
        );
    }
}

fn print_help() {
    println!("help       => print this help");
    println!("quit, exit => quit");
    println!("scope      => print all variables in the scope");
    println!("functions  => print all functions defined");
    println!("ast        => print the last AST");
    println!("astu       => print the last raw, un-optimized AST");
    println!(r"end a line with '\' to continue to the next line.");
    println!();
}

fn main() {
    let mut engine = Engine::new();

    #[cfg(not(feature = "no_optimize"))]
    engine.set_optimization_level(OptimizationLevel::None);

    let mut scope = Scope::new();

    let mut input = String::new();
    let mut main_ast: AST = Default::default();
    let mut ast_u: AST = Default::default();
    let mut ast: AST = Default::default();

    println!("Rhai REPL tool");
    println!("==============");
    print_help();

    'main_loop: loop {
        print!("rhai> ");
        stdout().flush().expect("couldn't flush stdout");

        input.clear();

        loop {
            match stdin().read_line(&mut input) {
                Ok(0) => break 'main_loop,
                Ok(_) => (),
                Err(err) => panic!("input error: {}", err),
            }

            let line = input.as_str().trim_end();

            // Allow line continuation
            if line.ends_with('\\') {
                let len = line.len();
                input.truncate(len - 1);
                input.push('\n');
            } else {
                break;
            }

            print!("> ");
            stdout().flush().expect("couldn't flush stdout");
        }

        let script = input.trim();

        if script.is_empty() {
            continue;
        }

        // Implement standard commands
        match script {
            "help" => {
                print_help();
                continue;
            }
            "exit" | "quit" => break, // quit
            "scope" => {
                scope
                    .iter_raw()
                    .enumerate()
                    .for_each(|(i, (name, constant, value))| {
                        println!(
                            "[{}] {}{}{} = {:?}",
                            i + 1,
                            if constant { "const " } else { "" },
                            name,
                            if value.is_shared() { " (shared)" } else { "" },
                            *value.read_lock::<Dynamic>().unwrap(),
                        )
                    });
                println!();
                continue;
            }
            "astu" => {
                // print the last un-optimized AST
                println!("{:#?}\n", &ast_u);
                continue;
            }
            "ast" => {
                // print the last AST
                println!("{:#?}\n", &ast);
                continue;
            }
            "functions" => {
                // print a list of all registered functions
                engine
                    .gen_fn_signatures(false)
                    .into_iter()
                    .for_each(|f| println!("{}", f));

                #[cfg(not(feature = "no_function"))]
                main_ast.iter_functions().for_each(|f| println!("{}", f));

                println!();
                continue;
            }
            // "json" => {
            //     println!(
            //         "{}",
            //         engine
            //             .gen_fn_metadata_to_json(Some(&main_ast), false)
            //             .unwrap()
            //     );
            //     continue;
            // }
            _ => (),
        }

        match engine
            .compile_with_scope(&scope, &script)
            .map_err(Into::into)
            .and_then(|r| {
                ast_u = r.clone();

                #[cfg(not(feature = "no_optimize"))]
                {
                    ast = engine.optimize_ast(&scope, r, OptimizationLevel::Simple);
                }

                #[cfg(feature = "no_optimize")]
                {
                    ast = r;
                }

                // Merge the AST into the main
                main_ast += ast.clone();

                // Evaluate
                engine.eval_ast_with_scope::<Dynamic>(&mut scope, &main_ast)
            }) {
            Ok(result) if !result.is::<()>() => {
                println!("=> {:?}", result);
                println!();
            }
            Ok(_) => (),
            Err(err) => {
                println!();
                print_error(&input, *err);
                println!();
            }
        }

        // Throw away all the statements, leaving only the functions
        main_ast.clear_statements();
    }
}
