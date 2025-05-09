use crate::{expr::Expr, reporting::Position};

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl((Position, String), Option<Expr>),
    Fun((Position, String), Vec<String>, Box<Stmt>),
    Expr(Expr),
    Print(Expr),
    Block(Vec<Stmt>),
    While(Expr, Box<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Return(Option<Expr>),
    NOP,
    Break,
    Continue,
}
