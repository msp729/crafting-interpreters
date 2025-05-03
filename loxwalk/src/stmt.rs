use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(String, Option<Expr>),
    Fun(String, Vec<String>, Box<Stmt>),
    Expr(Expr),
    Print(Expr),
    Block(Vec<Stmt>),
    While(Expr, Box<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Return(Expr),
    NOP,
    Break,
    Continue,
}
