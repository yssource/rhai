use rhai::{Dynamic, Engine, EvalAltResult, Module, Scope, AST, INT};
use rustyline::config::Builder;
use rustyline::error::ReadlineError;
use rustyline::{Cmd, Editor, Event, EventHandler, KeyCode, KeyEvent, Modifiers, Movement};
use smallvec::smallvec;

use std::{env, fs::File, io::Read, path::Path, process::exit};

const HISTORY_FILE: &str = ".rhai-repl-history";

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

/// Print help text.
fn print_help() {
    println!("help       => print this help");
    println!("keys       => print list of key bindings");
    println!("quit, exit => quit");
    println!("history    => print lines history");
    println!("scope      => print all variables in the scope");
    println!("strict     => toggle on/off Strict Variables Mode");
    #[cfg(not(feature = "no_optimize"))]
    println!("optimize   => toggle on/off script optimization");
    #[cfg(feature = "metadata")]
    println!("functions  => print all functions defined");
    #[cfg(feature = "metadata")]
    println!("json       => output all functions in JSON format");
    println!("ast        => print the last AST (optimized)");
    println!("astu       => print the last raw, un-optimized AST");
    println!();
    println!("press Shift-Enter to continue to the next line,");
    println!(r"or end a line with '\' (e.g. when pasting code).");
    println!();
}

/// Print key bindings.
fn print_keys() {
    println!("Home              => move to beginning of line");
    println!("Ctrl-Home         => move to beginning of input");
    println!("End               => move to end of line");
    println!("Ctrl-End          => move to end of input");
    println!("Left              => move left");
    println!("Ctrl-Left         => move left by one word");
    println!("Right             => move right by one word");
    println!("Ctrl-Right        => move right");
    println!("Up                => previous line or history");
    println!("Ctrl-Up           => previous history");
    println!("Down              => next line or history");
    println!("Ctrl-Down         => next history");
    println!("Ctrl-R            => reverse search history");
    println!("                     (Ctrl-S forward, Ctrl-G cancel)");
    println!("Ctrl-L            => clear screen");
    println!("Escape            => clear all input");
    println!("Ctrl-C            => exit");
    println!("Ctrl-D            => EOF (when line empty)");
    println!("Ctrl-H, Backspace => backspace");
    println!("Ctrl-D, Del       => delete character");
    println!("Ctrl-U            => delete from start");
    println!("Ctrl-W            => delete previous word");
    println!("Ctrl-T            => transpose characters");
    println!("Ctrl-V            => insert special character");
    println!("Ctrl-Y            => paste yank");
    println!("Ctrl-Z            => suspend (Unix), undo (Windows)");
    println!("Ctrl-_            => undo");
    println!("Enter             => run code");
    println!("Shift-Ctrl-Enter  => continue to next line");
    println!();
    println!("Plus all standard Emacs key bindings");
    println!();
}

/// Display the scope.
fn print_scope(scope: &Scope) {
    for (i, (name, constant, value)) in scope.iter_raw().enumerate() {
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
    }

    println!();
}

// Load script files specified in the command line.
#[cfg(not(feature = "no_module"))]
#[cfg(not(feature = "no_std"))]
fn load_script_files(engine: &mut Engine) {
    // Load init scripts
    let mut contents = String::new();
    let mut has_init_scripts = false;

    for filename in env::args().skip(1) {
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

        contents.clear();

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

        if let Err(err) = f.read_to_string(&mut contents) {
            println!(
                "Error reading script file: {}\n{}",
                filename.to_string_lossy(),
                err
            );
            exit(1);
        }

        let module = match engine
            .compile(&contents)
            .map_err(|err| err.into())
            .and_then(|mut ast| {
                ast.set_source(filename.to_string_lossy().to_string());
                Module::eval_ast_as_new(Scope::new(), &ast, &engine)
            }) {
            Err(err) => {
                let filename = filename.to_string_lossy();

                eprintln!("{:=<1$}", "", filename.len());
                eprintln!("{}", filename);
                eprintln!("{:=<1$}", "", filename.len());
                eprintln!("");

                print_error(&contents, *err);
                exit(1);
            }
            Ok(m) => m,
        };

        engine.register_global_module(module.into());

        has_init_scripts = true;

        println!("Script '{}' loaded.", filename.to_string_lossy());
    }

    if has_init_scripts {
        println!();
    }
}

// Setup the Rustyline editor.
fn setup_editor() -> Editor<()> {
    let config = Builder::new()
        .tab_stop(4)
        .indent_size(4)
        .bracketed_paste(true)
        .build();
    let mut rl = Editor::<()>::with_config(config);

    // Bind more keys

    // On Windows, Esc clears the input buffer
    #[cfg(target_family = "windows")]
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(KeyCode::Esc, Modifiers::empty())]),
        EventHandler::Simple(Cmd::Kill(Movement::WholeBuffer)),
    );
    // On Windows, Ctrl-Z is undo
    #[cfg(target_family = "windows")]
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent::ctrl('z')]),
        EventHandler::Simple(Cmd::Undo(1)),
    );
    // Map Shift-Return to insert a new line - bypass need for `\` continuation
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(
            KeyCode::Char('m'),
            Modifiers::CTRL_SHIFT
        )]),
        EventHandler::Simple(Cmd::Newline),
    );
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(
            KeyCode::Char('j'),
            Modifiers::CTRL_SHIFT
        )]),
        EventHandler::Simple(Cmd::Newline),
    );
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(KeyCode::Enter, Modifiers::SHIFT)]),
        EventHandler::Simple(Cmd::Newline),
    );
    // Map Ctrl-Home and Ctrl-End for beginning/end of input
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(KeyCode::Home, Modifiers::CTRL)]),
        EventHandler::Simple(Cmd::Move(Movement::BeginningOfBuffer)),
    );
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(KeyCode::End, Modifiers::CTRL)]),
        EventHandler::Simple(Cmd::Move(Movement::EndOfBuffer)),
    );
    // Map Ctrl-Up and Ctrl-Down to skip up/down the history, even through multi-line histories
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(KeyCode::Down, Modifiers::CTRL)]),
        EventHandler::Simple(Cmd::NextHistory),
    );
    rl.bind_sequence(
        Event::KeySeq(smallvec![KeyEvent(KeyCode::Up, Modifiers::CTRL)]),
        EventHandler::Simple(Cmd::PreviousHistory),
    );

    // Load the history file
    if rl.load_history(HISTORY_FILE).is_err() {
        eprintln!("! No previous lines history!");
    }

    rl
}

fn main() {
    let title = format!("Rhai REPL tool (version {})", env!("CARGO_PKG_VERSION"));
    println!("{}", title);
    println!("{0:=<1$}", "", title.len());

    #[cfg(not(feature = "no_optimize"))]
    let mut optimize_level = rhai::OptimizationLevel::Simple;

    // Initialize scripting engine
    let mut engine = Engine::new();

    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_std"))]
    load_script_files(&mut engine);

    // Setup Engine
    #[cfg(not(feature = "no_optimize"))]
    engine.set_optimization_level(rhai::OptimizationLevel::None);

    // Set a file module resolver without caching
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_std"))]
    {
        let mut resolver = rhai::module_resolvers::FileModuleResolver::new();
        resolver.enable_cache(false);
        engine.set_module_resolver(resolver);
    }

    engine
        .register_fn("test", |x: INT, y: INT| format!("{} {}", x, y))
        .register_fn("test", |x: &mut INT, y: INT, z: &str| {
            *x += y;
            println!("{} {} {}", x, y, z);
        });

    // Create scope
    let mut scope = Scope::new();

    // REPL line editor setup
    let mut rl = setup_editor();

    // REPL loop
    let mut input = String::new();
    let mut main_ast = AST::empty();
    let mut ast_u = AST::empty();
    let mut ast = AST::empty();

    print_help();

    'main_loop: loop {
        input.clear();

        loop {
            let prompt = if input.is_empty() {
                "rhai-repl> "
            } else {
                "         > "
            };

            match rl.readline(prompt) {
                // Line continuation
                Ok(mut line) if line.ends_with("\\") => {
                    line.pop();
                    input += line.trim_end();
                    input.push('\n');
                }
                Ok(line) => {
                    input += line.trim_end();
                    if !input.is_empty() {
                        rl.add_history_entry(input.clone());
                    }
                    break;
                }

                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break 'main_loop,

                Err(err) => {
                    eprintln!("Error: {:?}", err);
                    break 'main_loop;
                }
            }
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
            "keys" => {
                print_keys();
                continue;
            }
            "exit" | "quit" => break, // quit
            "history" => {
                for (i, h) in rl.history().iter().enumerate() {
                    match &h.split('\n').collect::<Vec<_>>()[..] {
                        [line] => println!("[{}] {}", i + 1, line),
                        lines => {
                            for (x, line) in lines.iter().enumerate() {
                                let number = format!("[{}]", i + 1);
                                if x == 0 {
                                    println!("{} {}", number, line.trim_end());
                                } else {
                                    println!("{0:>1$} {2}", "", number.len(), line.trim_end());
                                }
                            }
                        }
                    }
                }
                continue;
            }
            "strict" if engine.strict_variables() => {
                engine.set_strict_variables(false);
                println!("Strict Variables Mode turned OFF.");
                continue;
            }
            "strict" => {
                engine.set_strict_variables(true);
                println!("Strict Variables Mode turned ON.");
                continue;
            }
            #[cfg(not(feature = "no_optimize"))]
            "optimize" if optimize_level == rhai::OptimizationLevel::Simple => {
                optimize_level = rhai::OptimizationLevel::None;
                println!("Script optimization turned OFF.");
                continue;
            }
            #[cfg(not(feature = "no_optimize"))]
            "optimize" => {
                optimize_level = rhai::OptimizationLevel::Simple;
                println!("Script optimization turned ON.");
                continue;
            }
            "scope" => {
                print_scope(&scope);
                continue;
            }
            "astu" => {
                // print the last un-optimized AST
                println!("{:#?}\n", ast_u);
                continue;
            }
            "ast" => {
                // print the last AST
                println!("{:#?}\n", ast);
                continue;
            }
            #[cfg(feature = "metadata")]
            "functions" => {
                // print a list of all registered functions
                for f in engine.gen_fn_signatures(false) {
                    println!("{}", f)
                }

                #[cfg(not(feature = "no_function"))]
                for f in main_ast.iter_functions() {
                    println!("{}", f)
                }

                println!();
                continue;
            }
            #[cfg(feature = "metadata")]
            "json" => {
                println!(
                    "{}",
                    engine
                        .gen_fn_metadata_with_ast_to_json(&main_ast, true)
                        .unwrap()
                );
                continue;
            }
            _ => (),
        }

        match engine
            .compile_with_scope(&scope, &script)
            .map_err(Into::into)
            .and_then(|r| {
                ast_u = r.clone();

                #[cfg(not(feature = "no_optimize"))]
                {
                    ast = engine.optimize_ast(&scope, r, optimize_level);
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

    rl.save_history(HISTORY_FILE).unwrap();

    println!("Bye!");
}
