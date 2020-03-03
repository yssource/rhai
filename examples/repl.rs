use rhai::{Engine, RegisterFn, Scope};
use std::io::{stdin, stdout, Write};
use std::process::exit;

pub fn main() {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    engine.register_fn("exit", || exit(0));

    loop {
        print!("> ");

        let mut input = String::new();
        stdout().flush().expect("couldn't flush stdout");

        if let Err(e) = stdin().read_line(&mut input) {
            println!("input error: {}", e);
        }

        if let Err(e) = engine.consume_with_scope(&mut scope, &input) {
            println!("error: {}", e);
        }
    }
}
