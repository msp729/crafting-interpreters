use crate::tokens::{Delim, Keyword, Op, Side, Token};
use crate::{failure, fill};
use crate::{reporting::ErrorManager, tokens::Grammar};
use std::io::{BufRead, Seek};

#[derive(Debug)]
pub struct Scanner<'a, B> {
    line: u64,
    col: u64,
    source: B,
    mgr: &'a mut ErrorManager,
    eof_emitted: bool,
}

impl<B: BufRead + Seek> Iterator for Scanner<'_, B> {
    type Item = Token;

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.eof_emitted {
            (0, Some(0))
        } else {
            (1, None)
        }
    }

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof_emitted {
            return None;
        }
        let c = self.advance();
        match c {
            '(' => Some(Token::Grammar(Grammar::Delim(Side::Left, Delim::Paren))),
            ')' => Some(Token::Grammar(Grammar::Delim(Side::Right, Delim::Paren))),
            '{' => Some(Token::Grammar(Grammar::Delim(Side::Left, Delim::Brace))),
            '}' => Some(Token::Grammar(Grammar::Delim(Side::Right, Delim::Brace))),
            ',' => Some(Token::Grammar(Grammar::Comma)),
            '.' => Some(Token::Grammar(Grammar::Dot)),
            '-' => Some(Token::Grammar(Grammar::Op(Op::Minus))),
            '+' => Some(Token::Grammar(Grammar::Op(Op::Plus))),
            ';' => Some(Token::Grammar(Grammar::Semicolon)),
            '*' => Some(Token::Grammar(Grammar::Op(Op::Star))),

            '!' => Some(Token::Grammar(Grammar::Op({
                self.checkte('=', Op::NotEqual, Op::Bang)
            }))),
            '=' => Some(Token::Grammar(Grammar::Op({
                self.checkte('=', Op::Equal, Op::Assign)
            }))),
            '<' => Some(Token::Grammar(Grammar::Op({
                self.checkte('=', Op::LE, Op::LT)
            }))),
            '>' => Some(Token::Grammar(Grammar::Op({
                self.checkte('=', Op::GE, Op::GT)
            }))),

            '/' => {
                if !self.check('/') {
                    Some(Token::Grammar(Grammar::Op(Op::Slash)))
                } else {
                    self.advance_to('\n');
                    self.next()
                }
            }
            ' ' | '\t' | '\r' | '\n' => self.next(),

            '0'..='9' => Some(self.number(c.to_digit(10).expect("Digit precheck").into())),

            '_' | 'a'..='z' | 'A'..='Z' => Some(self.identifier(c.into())),
            '"' => Some(self.string()),
            '\0' => {
                self.eof_emitted = true;
                Some(Token::Grammar(Grammar::EOF))
            }
            _ => {
                self.err("Unexpected character".into());
                None
            }
        }
    }
}

impl<'a, B: BufRead + Seek> Scanner<'a, B> {
    pub fn new(mgr: &'a mut ErrorManager, source: B) -> Self {
        Self {
            line: 0,
            col: 0,
            source,
            mgr,
            eof_emitted: false,
        }
    }

    pub fn string(&mut self) -> Token {
        let mut interior = String::new();
        loop {
            match self.advance() {
                '\\' => interior.push(self.advance()),
                '"' => break,
                '\0' => {
                    self.err(String::from("unterminated string literal"));
                    self.eof_emitted = true;
                    return Token::Grammar(Grammar::EOF);
                }
                c => interior.push(c),
            }
        }
        Token::String(interior)
    }

    #[inline]
    pub fn gcheck<T, F: FnOnce(char) -> Option<T>>(&mut self, cond: F) -> Option<T> {
        let buf = fill!(self);
        if buf.is_empty() {
            None
        } else if let Some(v) = cond(buf[0] as char) {
            self.col += 1;
            if buf[0] == b'\n' {
                self.line += 1;
                self.col = 1;
            }
            self.source.consume(1);
            Some(v)
        } else {
            None
        }
    }

    #[inline]
    pub fn rcheck<F: FnOnce(char) -> bool>(&mut self, cond: F) -> Option<char> {
        self.gcheck(|x| if cond(x) { Some(x) } else { None })
    }

    #[inline]
    pub fn check(&mut self, c: char) -> bool {
        self.gcheck(|x| if x == c { Some(()) } else { None })
            .is_some()
    }

    #[inline]
    pub fn checkte<T>(&mut self, cond: char, then: T, r#else: T) -> T {
        if self.check(cond) {
            then
        } else {
            r#else
        }
    }

    fn advance(&mut self) -> char {
        let buf = fill!(self);
        if buf.is_empty() {
            '\0'
        } else {
            let v = buf[0];
            self.col += 1;
            self.source.consume(1);
            if v == b'\n' {
                self.line += 1;
                self.col = 1;
            }
            v as char
        }
    }

    fn err(&mut self, msg: String) {
        self.mgr.error(self.line, self.col, msg)
    }

    fn advance_to(&mut self, trg: char) -> () {
        while self.advance() != trg {}
        ()
    }

    fn identifier(&mut self, mut ident: String) -> Token {
        while let Some(c) = self.rcheck(|x| x.is_alphanumeric() || x == '_') {
            ident.push(c)
        }

        if let Ok(x) = Keyword::try_from(&ident) {
            Token::Grammar(Grammar::Keyword(x))
        } else {
            Token::Ident(ident)
        }
    }

    fn number(&mut self, mut num: f64) -> Token {
        while let Some(c) = self.gcheck(|x| x.to_digit(10)) {
            num *= 10.0;
            num += f64::from(c);
        }
        let buf = fill!(self);
        if buf.len() >= 2 && buf[0] == b'.' && (buf[1] as char).is_digit(10) {
            let mut d = 0.1;
            self.source.consume(1);
            while let Some(c) = self.gcheck(|x| x.to_digit(10)) {
                num += d * f64::from(c);
                d /= 10.0;
            }
        }
        Token::Number(num)
    }
}

#[cfg(test)]
mod tests {
    use super::Scanner;
    use crate::reporting::ErrorManager;
    use crate::tokens::{Delim, Grammar, Side, Token};
    use std::io::Cursor;

    #[test]
    fn braces() {
        let s = String::from("(){}");
        let c = Cursor::new(s);
        let mut e = ErrorManager::new();
        let mut sc = Scanner::new(&mut e, c);

        assert_eq!(
            sc.next(),
            Some(Token::Grammar(Grammar::Delim(Side::Left, Delim::Paren)))
        );
        assert_eq!(
            sc.next(),
            Some(Token::Grammar(Grammar::Delim(Side::Right, Delim::Paren)))
        );

        assert_eq!(
            sc.next(),
            Some(Token::Grammar(Grammar::Delim(Side::Left, Delim::Brace)))
        );
        assert_eq!(
            sc.next(),
            Some(Token::Grammar(Grammar::Delim(Side::Right, Delim::Brace)))
        );
    }
}
