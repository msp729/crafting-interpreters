use crate::reporting::ErrorClient;
use crate::reporting::{ErrorManager, Position};
use crate::tokens::{Delim, Grammar, Keyword, Op, Payload, Side, Token};

#[derive(Debug)]
pub struct Scanner<'a> {
    pos: Position,
    idx: usize,
    source: &'a [char],
    err: ErrorClient<'a>,
    tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(mgr: &'a ErrorManager, source: &'a [char]) -> Self {
        Self {
            pos: Position::default(),
            source,
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

    #[must_use]
    pub fn tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.add_token();
        }
        self.pos.step(' ');
        self.pos.sync();
        self.placeg(Grammar::EOF);
        self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.source.len()
    }

    fn report(&self, msg: &str) {
        self.err.error(self.pos, msg);
    }

    fn add_token(&mut self) {
        let c = self.advance();
        self.pos.sync();
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
                    self.placeg(Grammar::Op(Op::Slash));
                }
            }
            ' ' | '\r' | '\t' | '\n' => (),
            '"' => self.string(),
            '0'..='9' => self.number(),
            _ if c.is_alphabetic() => self.identifier(),
            '_' => self.identifier(),
            _ => self.report("Unexpected character"),
        }
    }

    fn string(&mut self) {
        let mut l = 0;
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
            l += 1;
        }
        if self.is_at_end() {
            self.report("Unterminated string");
        } else {
            let val: String = self.source[self.idx - l..self.idx].iter().collect();
            self.advance();
            self.place(Payload::String(val));
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
            self.placeg(t);
        } else {
            self.placeg(e);
        }
    }

    fn place(&mut self, load: Payload) {
        self.tokens.push(Token {
            pos: self.pos,
            load,
        });
    }

    fn placeg(&mut self, g: Grammar) {
        self.place(Payload::Grammar(g));
    }
}

#[cfg(test)]
mod tests {
    use crate::reporting::Loc;

    use super::*;

    fn pos(line: u64, col: u64, len: u64) -> Position {
        Position {
            start: Loc { line, col },
            end: Loc {
                line,
                col: col + len - 1,
            },
        }
    }

    fn tok(pos: Position, load: Payload) -> Token {
        Token { pos, load }
    }

    macro_rules! pos {
        // position parsing, pretty-style
        ($l:literal:$c:literal) => {
            pos($l, $c, 1)
        };
        ($l:literal:$b:literal-$e:literal) => {
            pos($l, $e, 1 + $e - $b)
        };
    }

    // i might be great at macros
    // enables elegant verification
    // from source to tokens
    macro_rules! verify {
        (test $name:ident payload $src:literal, $($wanted:expr),+ $(,)?) => {
            #[test]
            fn $name() {
                verify![payload from $src, $($wanted),+];
            }
        };

        (test $name:ident tokens $src:literal, $($loc:tt $load:expr),+ $(,)?) => {
            #[test]
            fn $name() {
                verify![tokens from $src, $($loc $load),+];
            }
        };

        (payload from $src:literal, $($wanted:expr),+ $(,)?) => {
            let src: Vec<char> = $src.chars().collect();
            let mgr = ErrorManager::new();
            let sc = Scanner::new(&mgr, &src);
            let tokens = sc.tokens();
            let expected = vec![$($wanted),+];
            verify!(payload tokens against expected);
            assert!(!mgr.errored.get());
        };

        (tokens from $src:literal, $($loc:tt $load:expr),+ $(,)?) => {
            let src: Vec<char> = $src.chars().collect();
            let mgr = ErrorManager::new();
            let sc = Scanner::new(&mgr, &src);
            let tokens = sc.tokens();
            let expected = vec![
                $(tok(pos!$loc, $load)),+ // the piece de resistance, pos!$loc
            ];
            verify!(full tokens against expected);
            assert!(!mgr.errored.get());
        };

        (payload $got:ident against $wanted:ident) => {
            println!("{:#?} against {:#?}", $got, $wanted);
            assert_eq!($got.len(), $wanted.len());
            for (g, w) in $got.into_iter().zip($wanted) {
                assert_eq!(g.load, w);
            }
        };
        (full $got:ident against $wanted:ident) => {
            println!("{:#?} against {:#?}", $got, $wanted);
            assert_eq!($got.len(), $wanted.len());
            for (g, w) in $got.into_iter().zip($wanted) {
                assert_eq!(g, w);
            }
        };
    }

    fn g(x: Grammar) -> Payload {
        Payload::Grammar(x)
    }

    verify![test paren
        payload "()",

        g(Grammar::LP),
        g(Grammar::RP),
        g(Grammar::EOF),
    ];

    verify![test braces
        payload "{}",

        g(Grammar::LB),
        g(Grammar::RB),
        g(Grammar::EOF),
    ];

    verify![
        test newline
        tokens "()\n()",

        [1:1] g(Grammar::LP),
        [1:2] g(Grammar::RP),
        [2:1] g(Grammar::LP),
        [2:2] g(Grammar::RP),
        [2:3] g(Grammar::EOF),
    ];

    verify![test ident
        payload "name",

        Payload::Ident("name".into()),
        g(Grammar::EOF),
    ];

    verify![test kword
        payload "or",

        g(Grammar::Keyword(Keyword::Or)),
        g(Grammar::EOF),
    ];

    verify![test number
        payload "1.3.4",

        Payload::Number(1.3),
        g(Grammar::Dot),
        Payload::Number(4.0),
        g(Grammar::EOF),
    ];
}
