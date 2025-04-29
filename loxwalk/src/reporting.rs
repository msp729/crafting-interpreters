use std::cell::Cell;

#[macro_export]
macro_rules! failure {
    ($msg:expr) => {
        format!("Failed {}; from {}:{}", $msg, file!(), line!())
    }; //format!("Failed to read buffer; error code {} {}", file!(), line!());
}

#[macro_export]
macro_rules! fill {
    ($self:ident) => {{
        let msg = failure!("to fill buffer");
        $self.source.fill_buf().expect(&msg)
    }};
}

#[derive(Debug, Clone)]
pub struct ErrorManager {
    pub errored: Cell<bool>,
}

impl Default for ErrorManager {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ErrorClient<'a>(&'a Cell<bool>);

impl ErrorManager {
    #[must_use]
    pub fn new() -> ErrorManager {
        Self {
            errored: Cell::new(false),
        }
    }

    pub fn client(&'_ self) -> ErrorClient<'_> {
        ErrorClient(&self.errored)
    }
}

impl ErrorClient<'_> {
    pub fn report(&self, pos: Position, loc: &str, msg: &str) {
        eprintln!("[{pos}] Error{loc}: {msg}");
        self.0.set(true);
    }

    pub fn error(&self, pos: Position, msg: &str) {
        self.report(pos, "", msg);
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub lin: u64,
    pub col: u64,
    pub len: u64,
}

impl Position {
    pub fn step(&mut self, c: char) {
        self.len += 1;
        if c == '\n' {
            self.lin += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            lin: 1,
            col: 0,
            len: 0,
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sc = self.col - self.len + 1;
        if sc < self.col {
            write!(f, "{}:{}-{}", self.lin, sc, self.col)
        } else {
            write!(f, "{}:{}", self.lin, self.col)
        }
    }
}
