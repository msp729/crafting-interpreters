#![feature(try_trait_v2, never_type)]

pub mod control;
pub mod lox;
pub mod parser;
pub mod scanner;
pub mod tokens;

use std::{env, fs, io, process};

fn main() {
    let argc = env::args().len();
    if argc > 2 {
        println!("Usage: jlox [script]");
        process::exit(64);
    } else if let Some(f) = env::args().nth(1) {
        let file = fs::File::open(f.clone()).expect("Failed to open the input file");
        let buffered = io::BufReader::new(file);
        let mut runner = lox::Lox::file();
        runner.exec(buffered);
    } else {
        let stdin = io::stdin();
        let buffered = io::BufReader::new(stdin);
        let mut runner = lox::Lox::prompt();
        runner.repl(buffered, b"> ");
    }
}
