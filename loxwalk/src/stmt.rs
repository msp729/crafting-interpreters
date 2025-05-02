use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(String, Option<Expr>),
    Expr(Expr),
    Print(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Decl(n, None) => write!(f, "VAR {n}"),
            Stmt::Decl(n, Some(e)) => write!(f, "VAR {n} := {e}"),
            Stmt::Expr(expr) => write!(f, "{expr}"),
            Stmt::Print(expr) => write!(f, "PRINT {expr}"),
            Stmt::Block(v) => {
                f.write_str("{\n")?;
                for x in v {
                    write!(f, "{x}")?;
                }
                f.write_str("}")
            }
            Stmt::If(i, t, Some(e)) => write!(f, "IF {i} THEN {t} ELSE {e}"),
            Stmt::If(i, t, None) => write!(f, "IF {i} THEN {t}"),
        }
    }
}
