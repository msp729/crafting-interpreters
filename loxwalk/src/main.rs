#![warn(clippy::pedantic)]
pub mod lox;
pub mod reporting;
pub mod scanner;
pub mod tokens;

use std::env;
use std::fs;
use std::io;
use std::process;

fn main() -> process::ExitCode {
    let argc = env::args().len();
    if argc > 2 {
        println!("Usage: jlox [script]");
        return 64.into();
    } else if let Some(f) = env::args().nth(1) {
        let file = fs::read_to_string(f.clone()).expect("Failed to open the input file");
        let mut engine = lox::Lox::new();
        return engine.exec(file).into();
    } else {
        let stdin = io::stdin();
        let buffered = io::BufReader::new(stdin);
        let mut engine = lox::Lox::new();
        engine.repl(buffered, b"> ");
    }
    process::ExitCode::SUCCESS
}
