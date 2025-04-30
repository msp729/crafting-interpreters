use crate::interpreter::Interpreter;
use crate::scanner::Scanner;
use crate::{parser::Parser, reporting::ErrorManager};
use std::io::{BufRead, Write};

#[derive(Debug, Clone)]
pub struct Lox {
    err: ErrorManager,
}

impl Default for Lox {
    fn default() -> Self {
        Self::new()
    }
}

impl Lox {
    #[must_use]
    pub fn new() -> Self {
        Self {
            err: ErrorManager::new(),
        }
    }

    pub fn exec(&mut self, buffered: &[char]) -> u8 {
        self.run(buffered);
        if self.err.errored.get() { 65 } else { 0 }
    }

    /// # Panics
    /// on I/O failures
    /// probably shouldn't do that, but i'll get back to it
    pub fn repl<R: BufRead>(&mut self, mut buffered: R, prompt: &[u8]) {
        let mut out = std::io::stdout();
        loop {
            let mut line = String::new();
            let _ = out.write(prompt).expect("Failed to prompt user");
            out.flush().expect("Failed to prompt user");
            match buffered.read_line(&mut line) {
                Ok(0) => return println!(),
                Ok(_) => (),
                Err(_) => return eprintln!("Problem loading input into memory"),
            }
            let line: Vec<char> = line.chars().collect();
            self.run(&line);
        }
    }

    pub fn run(&mut self, source: &[char]) -> Option<()> {
        let scanner = Scanner::new(&self.err, source);
        let v = scanner.tokens();
        for x in &v {
            println!("{x}");
        }

        let mut parser = Parser::new(&self.err, &v);
        let e = parser.parse()?;
        println!("{e}");

        let mut interpreter = Interpreter::new(&self.err);
        let v = interpreter.evaluate(e)?;
        println!("{v}");

        Some(())
    }
}
