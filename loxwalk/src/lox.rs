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

    pub fn exec(&self, buffered: &[char]) -> u8 {
        let mut interp = Interpreter::new(&self.err);
        self.run(buffered, &mut interp);
        if self.err.errored.get() { 65 } else { 0 }
    }

    /// # Panics
    /// on I/O failures
    /// probably shouldn't do that, but i'll get back to it
    pub fn repl<R: BufRead>(&self, mut buffered: R, prompt: &[u8]) {
        let mut out = std::io::stdout();
        let mut interp = Interpreter::new(&self.err);
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
            self.run(&line, &mut interp);
        }
    }

    pub fn run(&self, source: &[char], interp: &mut Interpreter) -> Option<()> {
        let scanner = Scanner::new(&self.err, source);
        let v = scanner.tokens();

        let mut parser = Parser::new(&self.err, &v);
        let stmts = parser.parse();

        for stmt in stmts {
            _ = interp.interpret(stmt);
        }

        Some(())
    }
}
