use std::{cell::RefCell, rc::Rc};

use crate::{interpreter::Interpreter, reporting::Position};

#[derive(Clone, Debug)]
pub enum Expr {
    Binary(Box<Expr>, (Position, Bin), Box<Expr>),
    Unary((Position, Un), Box<Expr>),
    Call(Box<Expr>, Position, Vec<Expr>),
    Grouping(Position, Box<Expr>),
    Literal(Position, Value),
    Ident(Position, String),
    Assign((Position, String), Box<Expr>),
    Push((Position, String), Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    Function(LoxFunc),
    Nil,
}

#[derive(Clone)]
pub struct LoxFunc(pub Rc<RefCell<dyn LFT>>);

impl PartialEq for LoxFunc {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl LFT for LoxFunc {
    fn arity(&self) -> usize {
        (*self.0).borrow().arity()
    }

    fn evaluate(&mut self, rt: &mut Interpreter, args: Vec<Value>) -> Value {
        (*self.0).borrow_mut().evaluate(rt, args)
    }
}

impl std::fmt::Debug for LoxFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function>")
    }
}

pub trait LFT {
    fn arity(&self) -> usize;
    fn evaluate(&mut self, rt: &mut Interpreter, args: Vec<Value>) -> Value;
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
    And,
    Or,
}

#[derive(Clone, Debug)]
pub enum Un {
    Not,
    Neg,
}

impl Value {
    #[must_use]
    pub fn truth(&self) -> bool {
        match self {
            Value::Num(x) => *x == 0.0,
            Value::Str(s) => s.is_empty(),
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::Function(_) => true,
        }
    }
}

impl Expr {
    #[must_use]
    pub fn pos(&self) -> Position {
        match self {
            Expr::Binary(l, _, r) => (l.pos()..r.pos()).into(),
            Expr::Push((p, _), e) | Expr::Unary((p, _), e) | Expr::Assign((p, _), e) => {
                (*p..e.pos()).into()
            }
            Expr::Grouping(p, _) | Expr::Literal(p, _) | Expr::Ident(p, _) => *p,
            Expr::Call(expr, pos, _) => (expr.pos()..*pos).into(),
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
            Expr::Assign((_, name), expr) => write!(f, "[assign {name} = {expr}]"),
            Expr::Push((_, name), expr) => write!(f, "[push {name} : {expr}]"),
            Expr::Call(fun, _, args) => {
                if args.is_empty() {
                    write!(f, "{fun}()")
                } else {
                    write!(f, "{fun}({}", args[0])?;
                    for arg in &args[1..] {
                        write!(f, ", {arg}")?;
                    }
                    Ok(())
                }
            }
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
            Bin::And => write!(f, "and"),
            Bin::Or => write!(f, "or"),
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
            Value::Function(_) => f.write_str("<function>"),
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
