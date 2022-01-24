use rhai::{Dynamic, Engine, EvalAltResult, Position, Scope};

#[cfg(feature = "debugging")]
use rhai::debugger::{BreakPoint, DebuggerCommand};

use std::{
    env,
    fs::File,
    io::{stdin, stdout, Read, Write},
    path::Path,
    process::exit,
};

/// Pretty-print source line.
fn print_source(lines: &[String], pos: Position) {
    let line_no = if lines.len() > 1 {
        if pos.is_none() {
            "".to_string()
        } else {
            format!("{}: ", pos.line().unwrap())
        }
    } else {
        "".to_string()
    };

    // Print error position
    if pos.is_none() {
        // No position
        println!();
    } else {
        // Specific position - print line text
        println!("{}{}", line_no, lines[pos.line().unwrap() - 1]);

        // Display position marker
        println!("{0:>1$}", "^", line_no.len() + pos.position().unwrap(),);
    }
}

/// Pretty-print error.
fn print_error(input: &str, mut err: EvalAltResult) {
    let lines: Vec<_> = input.trim().split('\n').collect();
    let pos = err.take_position();

    let line_no = if lines.len() > 1 {
        if pos.is_none() {
            "".to_string()
        } else {
            format!("{}: ", pos.line().unwrap())
        }
    } else {
        "".to_string()
    };

    // Print error position
    if pos.is_none() {
        // No position
        println!("{}", err);
    } else {
        // Specific position - print line text
        println!("{}{}", line_no, lines[pos.line().unwrap() - 1]);

        // Display position marker
        println!(
            "{0:>1$} {2}",
            "^",
            line_no.len() + pos.position().unwrap(),
            err
        );
    }
}

/// Print debug help.
fn print_debug_help() {
    println!("help        => print this help");
    println!("quit, exit  => quit");
    println!("scope       => print all variables in the scope");
    println!("node        => print the current AST node");
    println!("breakpoints => print all break-points");
    println!("clear       => delete all break-points");
    println!("break       => set a new break-point at the current position");
    println!("step        => go to the next expression, diving into functions");
    println!("next        => go to the next statement but don't dive into functions");
    println!("continue    => continue normal execution");
    println!();
}

/// Display the scope.
fn print_scope(scope: &Scope) {
    scope
        .iter_raw()
        .enumerate()
        .for_each(|(i, (name, constant, value))| {
            #[cfg(not(feature = "no_closure"))]
            let value_is_shared = if value.is_shared() { " (shared)" } else { "" };
            #[cfg(feature = "no_closure")]
            let value_is_shared = "";

            println!(
                "[{}] {}{}{} = {:?}",
                i + 1,
                if constant { "const " } else { "" },
                name,
                value_is_shared,
                *value.read_lock::<Dynamic>().unwrap(),
            )
        });

    println!();
}

#[cfg(feature = "debugging")]
fn main() {
    let title = format!("Rhai Debugger (version {})", env!("CARGO_PKG_VERSION"));
    println!("{}", title);
    println!("{0:=<1$}", "", title.len());

    // Initialize scripting engine
    let mut engine = Engine::new();

    let mut script = String::new();
    let main_ast;

    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_std"))]
    {
        // Load init scripts
        if let Some(filename) = env::args().skip(1).next() {
            let filename = match Path::new(&filename).canonicalize() {
                Err(err) => {
                    eprintln!("Error script file path: {}\n{}", filename, err);
                    exit(1);
                }
                Ok(f) => {
                    match f.strip_prefix(std::env::current_dir().unwrap().canonicalize().unwrap()) {
                        Ok(f) => f.into(),
                        _ => f,
                    }
                }
            };

            let mut f = match File::open(&filename) {
                Err(err) => {
                    eprintln!(
                        "Error reading script file: {}\n{}",
                        filename.to_string_lossy(),
                        err
                    );
                    exit(1);
                }
                Ok(f) => f,
            };

            if let Err(err) = f.read_to_string(&mut script) {
                println!(
                    "Error reading script file: {}\n{}",
                    filename.to_string_lossy(),
                    err
                );
                exit(1);
            }

            let script = if script.starts_with("#!") {
                // Skip shebang
                &script[script.find('\n').unwrap_or(0)..]
            } else {
                &script[..]
            };

            main_ast = match engine
                .compile(&script)
                .map_err(Into::<Box<EvalAltResult>>::into)
            {
                Err(err) => {
                    print_error(&script, *err);
                    exit(1);
                }
                Ok(ast) => ast,
            };

            println!("Script '{}' loaded.", filename.to_string_lossy());
            println!();
        } else {
            eprintln!("No script file specified.");
            exit(1);
        }
    }

    // Hook up debugger
    let lines: Vec<_> = script.trim().split('\n').map(|s| s.to_string()).collect();

    engine.on_debugger(move |context, node, source, pos| {
        print_source(&lines, pos);

        let mut input = String::new();

        loop {
            print!("rhai-dbg> ");
            stdout().flush().expect("couldn't flush stdout");

            input.clear();

            match stdin().read_line(&mut input) {
                Ok(0) => break DebuggerCommand::Continue,
                Ok(_) => match input.as_str().trim_end() {
                    "help" => print_debug_help(),
                    "exit" | "quit" => {
                        println!("Script terminated. Bye!");
                        exit(0);
                    }
                    "node" => {
                        println!("{:?} {}@{:?}", node, source.unwrap_or_default(), pos);
                        println!();
                    }
                    "continue" => break DebuggerCommand::Continue,
                    "" | "step" => break DebuggerCommand::StepInto,
                    "next" => break DebuggerCommand::StepOver,
                    "scope" => print_scope(context.scope()),
                    "clear" => {
                        context
                            .global_runtime_state_mut()
                            .debugger
                            .break_points_mut()
                            .clear();
                        println!("All break-points cleared.");
                    }
                    "breakpoints" => context
                        .global_runtime_state_mut()
                        .debugger
                        .iter_break_points()
                        .enumerate()
                        .for_each(|(i, bp)| match bp {
                            BreakPoint::AtPosition { pos, .. } => {
                                println!("[{}]", i);
                                print_source(&lines, *pos);
                            }
                            _ => println!("[{}]\n{}", i, bp),
                        }),
                    "break" => {
                        context
                            .global_runtime_state_mut()
                            .debugger
                            .break_points_mut()
                            .push(rhai::debugger::BreakPoint::AtPosition {
                                source: source.unwrap_or("").into(),
                                pos,
                            });
                        println!("Break-point added at the current position.");
                    }
                    cmd => eprintln!("Invalid debugger command: '{}'", cmd),
                },
                Err(err) => panic!("input error: {}", err),
            }
        }
    });

    // Set a file module resolver without caching
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_std"))]
    {
        let mut resolver = rhai::module_resolvers::FileModuleResolver::new();
        resolver.enable_cache(false);
        engine.set_module_resolver(resolver);
    }

    // Create scope
    let mut scope = Scope::new();

    print_debug_help();

    // Evaluate
    if let Err(err) = engine.run_ast_with_scope(&mut scope, &main_ast) {
        print_error(&script, *err);
    }
}

#[cfg(not(feature = "debugging"))]
fn main() {
    panic!("rhai-dbg requires the 'debugging' feature.")
}
