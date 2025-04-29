use crate::reporting::ErrorManager;
use crate::scanner::Scanner;
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
    pub fn new() -> Self {
        Self {
            err: ErrorManager::new(),
        }
    }

    pub fn exec(&mut self, buffered: String) -> u8 {
        self.run(buffered);
        if self.err.errored.get() {
            65
        } else {
            0
        }
    }

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
            self.run(line)
        }
    }

    pub fn run(&mut self, source: String) {
        let scanner = Scanner::new(&self.err, source);
        for x in scanner.tokens() {
            println!("{x}");
        }
    }
}
