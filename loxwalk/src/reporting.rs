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
pub struct Loc {
    pub line: u64,
    pub col: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Position {
    pub start: Loc,
    pub end: Loc,
}

impl Position {
    pub fn step(&mut self, c: char) {
        if c == '\n' {
            self.end.line += 1;
            self.end.col = 0;
        } else {
            self.end.col += 1;
        }
    }

    pub fn sync(&mut self) {
        self.start = self.end;
    }
}

impl From<std::ops::Range<Position>> for Position {
    fn from(value: std::ops::Range<Position>) -> Self {
        Self {
            start: value.start.start,
            end: value.end.end,
        }
    }
}

impl Default for Loc {
    fn default() -> Self {
        Loc { line: 1, col: 0 }
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end {
            write!(f, "{}", self.start)
        } else if self.start.line == self.end.line {
            write!(f, "{}-{}", self.start, self.end.col)
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}
