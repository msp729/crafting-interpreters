use crate::{
    expr::{Bin, Expr, LFT, Value},
    reporting::{ErrorClient, ErrorManager, Position},
    stmt::Stmt,
};
use std::collections::HashMap;

pub struct Interpreter<'a> {
    err: ErrorClient<'a>,
    env: Environment,
}

impl<'a> Interpreter<'a> {
    /// # Errors
    /// doesn't
    pub fn interpret(&mut self, st: Stmt) -> Result<(), FlowControl> {
        match st {
            Stmt::Expr(e) => {
                self.evaluate(e);
                Ok(())
            }
            Stmt::Print(e) => {
                match self.evaluate(e) {
                    None => (),
                    Some(Value::Str(s)) => println!("{s}"),
                    Some(x) => println!("{x}"),
                }
                Ok(())
            }

            Stmt::Decl(name, None) => {
                self.env.declare(name, Value::Nil);
                Ok(())
            }
            Stmt::Decl(name, Some(e)) => {
                let v = self.evaluate(e).unwrap_or(Value::Nil);
                self.env.declare(name, v);
                Ok(())
            }

            Stmt::Block(sts) => {
                self.env.push();
                for st in sts {
                    self.interpret(st)?;
                }
                self.env.pop();
                Ok(())
            }

            Stmt::If(cond, then, r#else) => {
                if self.evaluate(cond).unwrap_or(Value::Nil).truth() {
                    self.interpret(*then)
                } else if let Some(e) = r#else {
                    self.interpret(*e)
                } else {
                    Ok(())
                }
            }

            Stmt::While(cond, interior) => {
                while self.evaluate(cond.clone()).unwrap_or(Value::Nil).truth() {
                    match self.interpret(*interior.clone()) {
                        Ok(()) => (),
                        Err(FlowControl::Break) => break,
                        Err(FlowControl::Continue) => continue,
                    }
                }
                Ok(())
            }

            Stmt::NOP => Ok(()),
            Stmt::Break => Err(FlowControl::Break),
            Stmt::Continue => Err(FlowControl::Continue),
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

            Expr::Call(function, pos, args) => {
                let Value::Function(mut f) = self.evaluate(*function)? else {
                    self.err
                        .error(pos, "Cannot use function call syntax with non-function");
                    return None;
                };
                let values = args
                    .into_iter()
                    .map(|x| self.evaluate(x))
                    .collect::<Option<Vec<Value>>>()?;
                if values.len() == f.arity() {
                    Some(f.evaluate(self, values))
                } else {
                    self.err
                        .error(pos, &format!("Expected {} arguments", f.arity()));
                    None
                }
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
        Self(vec![Scope::globals()])
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

    fn globals() -> Self {
        let mut map = HashMap::new();
        map.insert("clock".into(), Value::Function(global::clock()));
        Self(map)
    }
}

mod global {
    use std::{cell::RefCell, rc::Rc};

    use crate::expr::{LFT, LoxFunc, Value};

    pub(crate) fn clock() -> crate::expr::LoxFunc {
        struct Ret;
        impl LFT for Ret {
            fn arity(&self) -> usize {
                0
            }

            fn evaluate(
                &mut self,
                _rt: &mut super::Interpreter,
                _args: Vec<crate::expr::Value>,
            ) -> crate::expr::Value {
                Value::Num(
                    std::time::UNIX_EPOCH
                        .elapsed()
                        .expect("Getting system time")
                        .as_secs_f64(),
                )
            }
        }
        LoxFunc(Rc::new(RefCell::new(Ret)))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FlowControl {
    Continue,
    Break,
}
