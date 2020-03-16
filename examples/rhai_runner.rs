use rhai::{Engine, EvalAltResult};

#[cfg(not(feature = "no_optimize"))]
use rhai::OptimizationLevel;

use std::{env, fs::File, io::Read, iter, process::exit};

fn padding(pad: &str, len: usize) -> String {
    iter::repeat(pad).take(len).collect::<String>()
}

fn eprint_error(input: &str, err: EvalAltResult) {
    fn eprint_line(lines: &Vec<&str>, line: usize, pos: usize, err: &str) {
        let line_no = format!("{}: ", line);
        let pos_text = format!(" (line {}, position {})", line, pos);

        eprintln!("{}{}", line_no, lines[line - 1]);
        eprintln!(
            "{}^ {}",
            padding(" ", line_no.len() + pos - 1),
            err.replace(&pos_text, "")
        );
        eprintln!("");
    }

    let lines: Vec<_> = input.split("\n").collect();

    // Print error
    match err.position() {
        p if p.is_eof() => {
            // EOF
            let line = lines.len() - 1;
            let pos = lines[line - 1].len();
            eprint_line(&lines, line, pos, &err.to_string());
        }
        p if p.is_none() => {
            // No position
            eprintln!("{}", err);
        }
        p => {
            // Specific position
            eprint_line(
                &lines,
                p.line().unwrap(),
                p.position().unwrap(),
                &err.to_string(),
            )
        }
    }
}

fn main() {
    for filename in env::args().skip(1) {
        let mut engine = Engine::new();

        #[cfg(not(feature = "no_optimize"))]
        engine.set_optimization_level(OptimizationLevel::Full);

        let mut f = match File::open(&filename) {
            Err(err) => {
                eprintln!("Error reading script file: {}\n{}", filename, err);
                exit(1);
            }
            Ok(f) => f,
        };

        let mut contents = String::new();

        match f.read_to_string(&mut contents) {
            Err(err) => {
                eprintln!("Error reading script file: {}\n{}", filename, err);
                exit(1);
            }
            _ => (),
        }

        if let Err(err) = engine.consume(false, &contents) {
            eprintln!("{}", padding("=", filename.len()));
            eprintln!("{}", filename);
            eprintln!("{}", padding("=", filename.len()));
            eprintln!("");

            eprint_error(&contents, err);
        }
    }
}
