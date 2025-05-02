use crate::{
    expr::{Bin, Expr, Value},
    reporting::{ErrorClient, ErrorManager, Position},
    stmt::Stmt,
};
use std::collections::HashMap;

pub struct Interpreter<'a> {
    err: ErrorClient<'a>,
    env: Environment,
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
            Stmt::Decl(name, None) => self.env.declare(name, Value::Nil),
            Stmt::Decl(name, Some(e)) => {
                let v = self.evaluate(e).unwrap_or(Value::Nil);
                self.env.declare(name, v);
            }
            Stmt::Block(sts) => {
                self.env.push();
                for st in sts {
                    self.interpret(st);
                }
                self.env.pop();
            }
            Stmt::If(cond, then, r#else) => {
                if self.evaluate(cond).unwrap_or(Value::Nil).truth() {
                    self.interpret(*then);
                } else if let Some(e) = r#else {
                    self.interpret(*e);
                }
            }
        }
    }

    pub fn evaluate(&mut self, ex: Expr) -> Option<Value> {
        match ex {
            Expr::Binary(e1, (_, Bin::And), e2) => {
                let v = self.evaluate(*e1)?;
                if v.truth() {
                    Some(self.evaluate(*e2)?)
                } else {
                    Some(v)
                }
            }
            Expr::Binary(e1, (_, Bin::Or), e2) => {
                let v = self.evaluate(*e1)?;
                if v.truth() {
                    Some(v)
                } else {
                    Some(self.evaluate(*e2)?)
                }
            }
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

            Expr::Ident(pos, ident) => Some(self.value(pos, &ident)?.clone()),

            Expr::Push((pos, name), expr) => {
                let v = self.evaluate(*expr).unwrap_or(Value::Nil);
                self.assign(pos, name, v.clone())
            }

            Expr::Assign((pos, name), expr) => {
                let v = self.evaluate(*expr).unwrap_or(Value::Nil);
                self.assign(pos, name, v.clone())?;
                Some(v)
            }
        }
    }

    pub fn new(mgr: &'a ErrorManager) -> Self {
        Self {
            err: mgr.client(),
            env: Environment::top(),
        }
    }

    fn assign(&mut self, pos: Position, name: String, val: Value) -> Option<Value> {
        self.env.assign(self.err, pos, name, val)
    }

    fn value(&mut self, pos: Position, name: &str) -> Option<Value> {
        self.env.value(self.err, pos, name)
    }
}

struct Environment(Vec<Scope>);
impl Environment {
    fn top() -> Self {
        Self(vec![Scope(HashMap::new())])
    }

    fn push(&mut self) {
        self.0.push(Scope(HashMap::new()));
    }

    fn pop(&mut self) {
        self.0.pop();
    }

    fn assign(
        &mut self,
        err: ErrorClient,
        pos: Position,
        mut name: String,
        val: Value,
    ) -> Option<Value> {
        for i in self.idxs() {
            let scope = &mut self.0[i];
            name = match scope.updater(name) {
                // a very convoluted way to do all of this without ever cloning name
                Ok(mut updater) => return Some(updater(val)),
                // move name back out of the function if necessary
                Err(name) => name,
            }
        }
        err.error(pos, &format!("Assigning to '{name}' before declaration"));
        None
    }

    fn declare(&mut self, name: String, v: Value) {
        self.local().0.insert(name, v);
    }

    fn value(&self, err: ErrorClient, pos: Position, name: &str) -> Option<Value> {
        for i in self.idxs() {
            let scope = &self.0[i];
            if let Some(v) = scope.0.get(name) {
                return Some(v.clone());
            }
        }
        err.error(pos, &format!("'{name}' is undefined"));
        None
    }

    fn local(&mut self) -> &mut Scope {
        let i = self.0.len() - 1;
        &mut self.0[i]
    }

    fn idxs(&self) -> impl Iterator<Item = usize> + use<> {
        let x = self.0.len();
        (1..=x).map(move |i| x - i)
    }
}

struct Scope(HashMap<String, Value>);

impl Scope {
    fn updater(&mut self, name: String) -> Result<impl FnMut(Value) -> Value, String> {
        match self.0.entry(name) {
            std::collections::hash_map::Entry::Occupied(mut entry) => Ok(move |v| entry.insert(v)),
            std::collections::hash_map::Entry::Vacant(entry) => Err(entry.into_key()),
        }
    }
}
