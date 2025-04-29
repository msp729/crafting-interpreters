use crate::reporting::ErrorClient;
use crate::reporting::{ErrorManager, Position};
use crate::tokens::{Delim, Grammar, Keyword, Op, Payload, Side, Token};

#[derive(Debug)]
pub struct Scanner<'a> {
    pos: Position,
    idx: usize,
    source: Vec<char>,
    err: ErrorClient<'a>,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(mgr: &'a ErrorManager, source: String) -> Self {
        Self {
            pos: Position::default(),
            source: source.chars().collect(),
            idx: 0,
            err: mgr.client(),
            tokens: Vec::new(),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.idx];
        self.pos.step(c);
        self.idx += 1;
        c
    }

    pub fn tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.add_token();
        }
        self.placeg(Grammar::EOF);
        self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.source.len()
    }

    fn report(&self, msg: String) {
        self.err.error(self.pos, msg)
    }

    fn add_token(&mut self) {
        self.pos.len = 0;
        let c = self.advance();
        match c {
            '(' => self.placeg(Grammar::Delim(Side::Left, Delim::Paren)),
            ')' => self.placeg(Grammar::Delim(Side::Right, Delim::Paren)),
            '{' => self.placeg(Grammar::Delim(Side::Left, Delim::Brace)),
            '}' => self.placeg(Grammar::Delim(Side::Right, Delim::Brace)),
            ',' => self.placeg(Grammar::Comma),
            '.' => self.placeg(Grammar::Dot),
            ';' => self.placeg(Grammar::Semicolon),
            '-' => self.placeg(Grammar::Op(Op::Minus)),
            '+' => self.placeg(Grammar::Op(Op::Plus)),
            '*' => self.placeg(Grammar::Op(Op::Star)),
            '!' => self.checkte('=', Grammar::Op(Op::NotEqual), Grammar::Op(Op::Bang)),
            '=' => self.checkte('=', Grammar::Op(Op::Equal), Grammar::Op(Op::Assign)),
            '>' => self.checkte('=', Grammar::Op(Op::GE), Grammar::Op(Op::GT)),
            '<' => self.checkte('=', Grammar::Op(Op::LE), Grammar::Op(Op::LT)),
            '/' => {
                if self.check('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.placeg(Grammar::Op(Op::Slash))
                }
            }
            ' ' | '\r' | '\t' | '\n' => (),
            '"' => self.string(),
            '0'..='9' => self.number(),
            _ if c.is_alphabetic() => self.identifier(),
            '_' => self.identifier(),
            _ => self.report("Unexpected character".into()),
        }
    }

    fn string(&mut self) {
        let mut l = 0;
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
            l += 1;
        }
        if self.is_at_end() {
            self.report("Unterminated string".into())
        } else {
            let val: String = self.source[self.idx - l..self.idx].iter().collect();
            self.advance();
            self.place(Payload::String(val))
        }
    }

    fn number(&mut self) {
        let mut l = 1;
        while self.peek().is_ascii_digit() {
            l += 1;
            self.advance();
        }
        if self.peek() == '.' && self.peek2().is_ascii_digit() {
            self.advance();
            l += 1;
            while self.peek().is_ascii_digit() {
                l += 1;
                self.advance();
            }
        }
        let txt: String = self.source[self.idx - l..self.idx].iter().collect();
        self.place(Payload::Number(txt.parse().expect("Pre-vetted")));
    }

    fn identifier(&mut self) {
        let mut l = 1;
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
            l += 1;
        }
        let txt: String = self.source[self.idx - l..self.idx].iter().collect();
        if let Ok(k) = Keyword::try_from(&txt) {
            self.placeg(Grammar::Keyword(k));
        } else {
            self.place(Payload::Ident(txt));
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.idx]
        }
    }

    fn peek2(&self) -> char {
        if self.idx + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.idx + 1]
        }
    }

    fn check(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source[self.idx] != c {
            return false;
        }
        self.idx += 1;
        true
    }

    fn checkte(&mut self, c: char, t: Grammar, e: Grammar) {
        let b = self.check(c);
        if b {
            self.placeg(t)
        } else {
            self.placeg(e)
        }
    }

    fn place(&mut self, load: Payload) {
        self.tokens.push(Token {
            pos: self.pos,
            load,
        })
    }

    fn placeg(&mut self, g: Grammar) {
        self.place(Payload::Grammar(g))
    }
}
