use crate::{
    expr::{Expr, Value},
    reporting::{ErrorClient, ErrorManager},
    stmt::Stmt,
};
use std::collections::HashMap;

pub struct Interpreter<'a> {
    err: ErrorClient<'a>,
    env: HashMap<String, Value>,
}

impl<'a> Interpreter<'a> {
    pub fn interpret(&mut self, st: Stmt) {
        match st {
            Stmt::Expr(e) => _ = self.evaluate(e),
            Stmt::Print(e) => {
                () = match self.evaluate(e) {
                    None => (),
                    Some(Value::Str(s)) => println!("{s}"),
                    Some(x) => println!("{x}"),
                }
            }
            Stmt::Decl(name, None) => _ = self.env.insert(name, Value::Nil),
            Stmt::Decl(name, Some(e)) => {
                if let Some(v) = self.evaluate(e) {
                    self.env.insert(name, v);
                } else {
                    self.env.insert(name, Value::Nil);
                }
            }
        }
    }

    pub fn evaluate(&mut self, ex: Expr) -> Option<Value> {
        match ex {
            Expr::Binary(e1, (pos, op), e2) => {
                let l = self.evaluate(*e1)?;
                let r = self.evaluate(*e2)?;
                Some(match (op, l, r) {
                    (crate::expr::Bin::Eql, l, r) => Value::Bool(l == r),
                    (crate::expr::Bin::Neq, l, r) => Value::Bool(l != r),
                    (crate::expr::Bin::Gt, Value::Num(x), Value::Num(y)) => Value::Bool(x > y),
                    (crate::expr::Bin::Ge, Value::Num(x), Value::Num(y)) => Value::Bool(x >= y),
                    (crate::expr::Bin::Lt, Value::Num(x), Value::Num(y)) => Value::Bool(x < y),
                    (crate::expr::Bin::Le, Value::Num(x), Value::Num(y)) => Value::Bool(x <= y),
                    (crate::expr::Bin::Add, Value::Num(x), Value::Num(y)) => Value::Num(x + y),
                    (crate::expr::Bin::Sub, Value::Num(x), Value::Num(y)) => Value::Num(x - y),
                    (crate::expr::Bin::Mul, Value::Num(x), Value::Num(y)) => Value::Num(x * y),
                    (crate::expr::Bin::Div, Value::Num(x), Value::Num(y)) => Value::Num(x / y),
                    (crate::expr::Bin::Add, Value::Str(x), Value::Str(y)) => Value::Str(x + &y),
                    (crate::expr::Bin::Add, Value::Str(x), y) => Value::Str(format!("{x}{y}")),
                    (crate::expr::Bin::Add, x, Value::Str(y)) => Value::Str(format!("{x}{y}")),
                    (op, l, r) => {
                        self.err.error(pos, &format!("Operator {op} cannot be applied to values {l} and {r} due to incompatible types."));
                        return None;
                    }
                })
            }

            Expr::Unary((pos, op), ex) => match op {
                crate::expr::Un::Neg => match self.evaluate(*ex)? {
                    Value::Num(x) => Some(Value::Num(-x)),
                    v => {
                        self.err.error(pos, &format!("Cannot negate {v}"));
                        None
                    }
                },
                crate::expr::Un::Not => Some(Value::Bool(!self.evaluate(*ex)?.truth())),
            },

            Expr::Grouping(_, ex) => self.evaluate(*ex),

            Expr::Literal(_, value) => Some(value),

            Expr::Ident(pos, ident) => {
                if let Some(v) = self.env.get(&ident) {
                    Some(v.clone())
                } else {
                    self.err
                        .error(pos, &format!("Variable `{ident}` is undefined"));
                    None
                }
            }

            Expr::Push((pos, name), expr) => {
                if self.env.contains_key(&name) {
                    let v = self.evaluate(*expr).unwrap_or(Value::Nil);
                    self.env.insert(name, v)
                } else {
                    self.err
                        .error(pos, "Variable assigned to without declaration");
                    None
                }
            }

            Expr::Assign((pos, name), expr) => {
                if self.env.contains_key(&name) {
                    let v = self.evaluate(*expr).unwrap_or(Value::Nil);
                    self.env.insert(name, v.clone());
                    Some(v)
                } else {
                    self.err
                        .error(pos, "Variable assigned to without declaration");
                    None
                }
            }
        }
    }

    pub fn new(mgr: &'a ErrorManager) -> Self {
        Self {
            err: mgr.client(),
            env: HashMap::new(),
        }
    }
}
