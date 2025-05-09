use crate::{
    expr::{Bin, Expr, LFT, LoxFunc, Value},
    reporting::{ErrorClient, ErrorManager, Position},
    stmt::Stmt,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Interpreter<'a> {
    pub(crate) err: ErrorClient<'a>,
    env: Environment,
    pub(crate) locals: HashMap<Position, usize>,
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

            Stmt::Decl((_, name), None) => {
                self.env.declare(name, Value::Nil);
                Ok(())
            }
            Stmt::Decl((_, name), Some(e)) => {
                let v = self.evaluate(e).unwrap_or(Value::Nil);
                self.env.declare(name, v);
                Ok(())
            }

            Stmt::Block(sts) => {
                self.env.push();
                for st in sts {
                    match self.interpret(st) {
                        Ok(()) => (),
                        Err(err) => {
                            self.env.pop();
                            return Err(err);
                        }
                    }
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
                        Err(FlowControl::Return(x)) => return Err(FlowControl::Return(x)),
                    }
                }
                Ok(())
            }

            Stmt::Fun((_, fname), args, body) => {
                self.env.declare(
                    fname.clone(),
                    Value::Function(LoxFunc(Rc::new(RefCell::new(global::LF(
                        fname, args, *body,
                    ))))),
                );
                Ok(())
            }

            Stmt::NOP => Ok(()),
            Stmt::Break => Err(FlowControl::Break),
            Stmt::Continue => Err(FlowControl::Continue),
            Stmt::Return(Some(e)) => {
                Err(FlowControl::Return(self.evaluate(e).unwrap_or(Value::Nil)))
            }
            Stmt::Return(None) => Err(FlowControl::Return(Value::Nil)),
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
            locals: HashMap::new(),
        }
    }

    fn assign(&mut self, pos: Position, name: String, val: Value) -> Option<Value> {
        if let Some(dist) = self.locals.get(&pos) {
            self.env.assign_at(*dist, name, val)
        } else {
            self.globals().assign(name, val)
        }
    }

    fn value(&mut self, pos: Position, name: &str) -> Option<Value> {
        if let Some(dist) = self.locals.get(&pos) {
            self.env.get_at(*dist, name)
        } else {
            self.globals().get(name)
        }
    }

    fn globals(&mut self) -> &mut Scope {
        self.env.global()
    }
}

#[derive(Debug)]
struct Environment(Vec<Scope>);
impl Environment {
    fn top() -> Self {
        Self(vec![Scope::globals()])
    }

    fn get_at(&self, dist: usize, name: &str) -> Option<Value> {
        self.0[self.0.len() - 1 - dist].get(name)
    }

    fn push(&mut self) {
        self.0.push(Scope(HashMap::new()));
    }

    fn pop(&mut self) {
        self.0.pop();
    }

    fn declare(&mut self, name: String, v: Value) {
        self.local().0.insert(name, v);
    }

    fn local(&mut self) -> &mut Scope {
        let i = self.0.len() - 1;
        &mut self.0[i]
    }

    fn global(&mut self) -> &mut Scope {
        &mut self.0[0]
    }

    fn assign_at(&mut self, dist: usize, name: String, val: Value) -> Option<Value> {
        let x = self.0.len() - dist - 1;
        self.0[x].assign(name, val)
    }
}

#[derive(Debug)]
struct Scope(HashMap<String, Value>);

impl Scope {
    fn get(&self, name: &str) -> Option<Value> {
        self.0.get(name).cloned()
    }

    fn globals() -> Self {
        let mut map = HashMap::new();
        map.insert("clock".into(), Value::Function(global::clock()));
        Self(map)
    }

    fn assign(&mut self, name: String, val: Value) -> Option<Value> {
        self.0.insert(name, val)
    }
}

mod global {
    use std::{cell::RefCell, rc::Rc};

    use super::{FlowControl, Interpreter};
    use crate::expr::{LFT, LoxFunc, Value};
    use crate::stmt::Stmt;

    macro_rules! newfunc {
        ($name:ident, $arity:literal, $sname:ident, $fn:expr) => {
            pub(crate) fn $name() -> LoxFunc {
                struct $sname;
                impl LFT for $sname {
                    fn arity(&self) -> usize {
                        $arity
                    }
                    fn evaluate(&mut self, _: &mut Interpreter, args: Vec<Value>) -> Value {
                        $fn(args)
                    }
                    fn name(&self) -> String {
                        stringify!($name).into()
                    }
                    fn is_native(&self) -> bool {
                        true
                    }
                }
                LoxFunc(Rc::new(RefCell::new($sname)))
            }
        };
    }

    newfunc!(
        clock,
        0,
        Clock,
        (|_| std::time::UNIX_EPOCH
            .elapsed()
            .expect("Failed to get clock time")
            .as_secs_f64()
            .into())
    );

    pub struct LF(pub String, pub Vec<String>, pub Stmt);

    impl LFT for LF {
        fn arity(&self) -> usize {
            self.1.len()
        }

        fn evaluate(&mut self, rt: &mut Interpreter, args: Vec<Value>) -> Value {
            rt.env.push();
            let x = 0..args.len();
            for (arg, i) in args.into_iter().zip(x) {
                rt.env.declare(self.1[i].clone(), arg);
            }
            let rv = rt.interpret(self.2.clone());
            rt.env.pop();

            match rv {
                Ok(()) => Value::Nil,
                Err(FlowControl::Return(x)) => x,
                Err(FlowControl::Break) => {
                    eprintln!("top-level break in function");
                    Value::Nil
                }
                Err(FlowControl::Continue) => {
                    eprintln!("top-level continue in function");
                    Value::Nil
                }
            }
        }

        fn name(&self) -> String {
            self.0.clone()
        }

        fn is_native(&self) -> bool {
            false
        }
    }
}

#[derive(Debug, Clone)]
pub enum FlowControl {
    Return(Value),
    Continue,
    Break,
}
