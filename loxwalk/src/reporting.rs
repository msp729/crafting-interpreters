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
    pub errored: bool,
}

impl Default for ErrorManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ErrorManager {
    pub fn new() -> ErrorManager {
        Self { errored: false }
    }

    #[inline]
    pub fn report(&mut self, line: u64, col: u64, loc: String, msg: String) {
        eprintln!("[{line}:{col}] Error{loc}: {msg}");
        self.errored = true;
    }

    #[inline]
    pub fn error(&mut self, line: u64, col: u64, msg: String) {
        self.report(line, col, String::new(), msg);
    }
}
