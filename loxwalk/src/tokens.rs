use std::fmt::{Display, Formatter, Write};

use crate::reporting::Position;

#[derive(Debug, Clone)]
pub struct Token {
    pub pos: Position,
    pub load: Payload,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Payload {
    Grammar(Grammar),
    Ident(String),
    String(String),
    Number(f64),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Grammar {
    Delim(Side, Delim),
    Comma,
    Dot,
    Semicolon,
    Op(Op),
    Keyword(Keyword),
    EOF,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Side {
    Left,
    Right,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Delim {
    Paren,
    Brace,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    Slash,
    Bang,
    Assign,
    Equal,
    NotEqual,
    GE,
    GT,
    LE,
    LT,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "[{}] {}", self.pos, self.load)
    }
}

impl Display for Payload {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Payload::Grammar(g) => g.fmt(f),
            Payload::Ident(id) => write!(f, "IDENT {id}"),
            Payload::String(s) => write!(f, "STRING {s}"),
            Payload::Number(x) => write!(f, "NUMBER {x}"),
        }
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Grammar::Delim(side, delim) => {
                let c = match (side, delim) {
                    (Side::Left, Delim::Paren) => '(',
                    (Side::Left, Delim::Brace) => '{',
                    (Side::Right, Delim::Paren) => ')',
                    (Side::Right, Delim::Brace) => '}',
                };
                write!(f, "DELIM {c}")
            }
            Grammar::Comma => write!(f, "COMMA"),
            Grammar::Dot => write!(f, "DOT"),
            Grammar::Semicolon => write!(f, "SEMICOLON"),
            Grammar::Op(op) => write!(f, "OP {op}"),
            Grammar::Keyword(keyword) => write!(f, "KEYWORD {keyword}"),
            Grammar::EOF => f.write_str("EOF"),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Op::Minus => f.write_char('-'),
            Op::Plus => f.write_char('+'),
            Op::Star => f.write_char('*'),
            Op::Slash => f.write_char('/'),
            Op::Bang => f.write_char('!'),
            Op::Assign => f.write_char('='),
            Op::Equal => f.write_str("=="),
            Op::NotEqual => f.write_str("!="),
            Op::GE => f.write_str(">="),
            Op::GT => f.write_char('>'),
            Op::LE => f.write_str("<="),
            Op::LT => f.write_char('<'),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Keyword::And => f.write_str("and"),
            Keyword::Class => f.write_str("class"),
            Keyword::Else => f.write_str("else"),
            Keyword::False => f.write_str("false"),
            Keyword::Fun => f.write_str("fun"),
            Keyword::For => f.write_str("for"),
            Keyword::If => f.write_str("if"),
            Keyword::Nil => f.write_str("nil"),
            Keyword::Or => f.write_str("or"),
            Keyword::Print => f.write_str("print"),
            Keyword::Return => f.write_str("return"),
            Keyword::Super => f.write_str("super"),
            Keyword::This => f.write_str("this"),
            Keyword::True => f.write_str("true"),
            Keyword::Var => f.write_str("var"),
            Keyword::While => f.write_str("while"),
        }
    }
}

macro_rules! checks {
    ($discriminant:expr ; $($ck:expr => $val:expr),+ ; $anyway:expr) => {
        $(if $discriminant == $ck {$val})else+
            else {return $anyway;}
    };
    ($discriminant:expr ; $($ck:expr => $val:expr,)+ ; $anyway:expr) => {
        $(if $discriminant == $ck {$val})else+
            else {return $anyway;}
    };
}

impl TryFrom<&String> for Keyword {
    type Error = ();

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        let value = value.to_lowercase();
        Ok(checks!(value ;
            "and" => Keyword::And,
            "class" => Keyword::Class,
            "else" => Keyword::Else,
            "false" => Keyword::False,
            "fun" => Keyword::Fun,
            "for" => Keyword::For,
            "if" => Keyword::If,
            "nil" => Keyword::Nil,
            "or" => Keyword::Or,
            "print" => Keyword::Print,
            "return" => Keyword::Return,
            "super" => Keyword::Super,
            "this" => Keyword::This,
            "true" => Keyword::True,
            "var" => Keyword::Var,
            "while" => Keyword::While,
                ; Err(())))
    }
}
