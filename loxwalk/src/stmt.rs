use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(String, Option<Expr>),
    Expr(Expr),
    Print(Expr),
    Block(Vec<Stmt>),
    While(Expr, Box<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    NOP,
    Break,
    Continue,
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
                    writeln!(f, "{x}")?;
                }
                f.write_str("}")
            }
            Stmt::While(e, s) => write!(f, "WHILE {e} {s}"),
            Stmt::If(i, t, Some(e)) => write!(f, "IF {i} THEN {t} ELSE {e}"),
            Stmt::If(i, t, None) => write!(f, "IF {i} THEN {t}"),
            Stmt::NOP => f.write_str("NOP"),
            Stmt::Break => f.write_str("BREAK"),
            Stmt::Continue => f.write_str("CONTINUE"),
        }
    }
}
