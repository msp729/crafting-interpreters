use std::io::{BufRead, Cursor, Seek, Write};

#[derive(Debug, Clone)]
pub struct Lox {}

impl Lox {
    pub fn file() -> Self {
        Self {}
    }

    pub fn prompt() -> Self {
        Self {}
    }

    pub fn exec<R: BufRead + Seek>(&mut self, buffered: R) {
        self.run(buffered);
    }

    pub fn repl<R: BufRead>(&mut self, mut buffered: R, prompt: &[u8]) {
        let mut out = std::io::stdout();
        loop {
            let mut line = String::new();
            out.write(prompt).expect("Failed to prompt user");
            out.flush().expect("Failed to prompt user");
            match buffered.read_line(&mut line) {
                Ok(0) => return println!(),
                Ok(_) => (),
                Err(_) => return eprintln!("Problem loading input into memory"),
            }
            let cursor = Cursor::new(line);
            self.run(cursor)
        }
    }

    pub fn run<B: BufRead + Seek>(&mut self, mut source: B) {
        let mut input = String::new();
        source
            .read_to_string(&mut input)
            .expect("Problem loading source into memory");
        println!("Run: {input:?}");
    }
}
