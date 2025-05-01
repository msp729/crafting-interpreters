use crate::reporting::Position;

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Box<Expr>, (Position, Bin), Box<Expr>),
    Unary((Position, Un), Box<Expr>),
    Grouping(Position, Box<Expr>),
    Literal(Position, Value),
    Ident(Position, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    Nil,
}

#[derive(Clone, Debug)]
pub enum Bin {
    Eql,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Debug)]
pub enum Un {
    Not,
    Neg,
}

impl Value {
    #[must_use]
    pub fn truth(self) -> bool {
        match self {
            Value::Num(x) => x == 0.0,
            Value::Str(s) => s.is_empty(),
            Value::Bool(b) => b,
            Value::Nil => false,
        }
    }
}

impl Expr {
    #[must_use]
    pub fn pos(&self) -> Position {
        match self {
            Expr::Binary(left, _, right) => (left.pos()..right.pos()).into(),
            Expr::Unary((pos, _), expr) => (*pos..expr.pos()).into(),
            Expr::Grouping(pos, _) | Expr::Literal(pos, _) | Expr::Ident(pos, _) => *pos,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(expr, (_, op), expr1) => write!(f, "({op} {expr} {expr1})"),
            Expr::Unary((_, op), expr) => write!(f, "({op} {expr})"),
            Expr::Grouping(_, expr) => write!(f, "[group {expr}]"),
            Expr::Literal(_, value) => write!(f, "[literal {value}]"),
            Expr::Ident(_, name) => write!(f, "[ident {name}]"),
        }
    }
}

impl std::fmt::Display for Bin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bin::Eql => write!(f, "=="),
            Bin::Neq => write!(f, "!="),
            Bin::Gt => write!(f, ">"),
            Bin::Ge => write!(f, ">="),
            Bin::Lt => write!(f, "<"),
            Bin::Le => write!(f, "<="),
            Bin::Add => write!(f, "+"),
            Bin::Sub => write!(f, "-"),
            Bin::Mul => write!(f, "*"),
            Bin::Div => write!(f, "/"),
        }
    }
}

impl std::fmt::Display for Un {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Un::Not => write!(f, "not"),
            Un::Neg => write!(f, "negate"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(x) => write!(f, "{x}"),
            Value::Str(s) => write!(f, "{s:#?}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

/*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
 */
