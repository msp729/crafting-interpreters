#![warn(clippy::pedantic)]
#![allow(clippy::needless_continue)]
pub mod expr;
pub mod lox;
pub mod parser;
pub mod passes;
pub mod reporting;
pub mod scanner;
pub mod stmt;
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
        let file = file.chars().collect::<Vec<char>>();
        let engine = lox::Lox::new();
        return engine.exec(&file).into();
    }
    let stdin = io::stdin();
    let buffered = io::BufReader::new(stdin);
    let engine = lox::Lox::new();
    engine.repl(buffered, b"> ");
    process::ExitCode::SUCCESS
}
