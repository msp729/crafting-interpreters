use crate::{expr::Expr, reporting::Position};

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(String, Option<Expr>),
    Asn(Position, String, Expr),
    Expr(Expr),
    Print(Expr),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(n, None) => write!(f, "VAR {n}"),
            Stmt::Decl(n, Some(e)) => write!(f, "VAR {n} := {e}"),
            Stmt::Asn(_, n, e) => write!(f, "ASSIGN {n} := {e}"),
            Stmt::Expr(expr) => write!(f, "{expr}"),
            Stmt::Print(expr) => write!(f, "PRINT {expr}"),
        }
    }
}
