use std::{
    env, fs,
    io::{self, BufRead, Write},
};

use lox_rs::{lexer::Lexer, LoxError};

fn main() -> Result<(), LoxError> {
    match script_path_from_args() {
        None => run_prompt()?,
        Some(path) => run_file(path.as_str())?,
    };

    println!();

    Ok(())
}

// Returns the path to the script, if specified
fn script_path_from_args() -> Option<String> {
    env::args().nth(1)
}

// Runs the source file specified at `path`
fn run_file(path: &str) -> Result<(), LoxError> {
    let source = fs::read_to_string(path)?;

    run_source(source);

    Ok(())
}

// Starts the REPL
fn run_prompt() -> Result<(), LoxError> {
    let mut stdin = io::stdin().lock();

    loop {
        let mut input = String::new();

        print!("> ");
        io::stdout().flush()?;

        stdin.read_line(&mut input)?;

        if input.is_empty() {
            break;
        }

        run_source(input);
    }

    Ok(())
}

// Attempts to run an arbitrary `String` as Lox source code
fn run_source(source: String) {
    let lexer = Lexer::new(source.as_bytes());
    let _ = lexer.lex();
}
