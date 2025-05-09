use std::collections::HashMap;

use crate::{
    expr::Expr,
    reporting::{ErrorClient, ErrorManager, Position},
    stmt::Stmt,
};

use super::interpreter::Interpreter;

type Scope = HashMap<String, bool>;

#[derive(Debug)]
pub struct Resolver<'a, 'b> {
    scopes: Vec<Scope>,
    err: ErrorClient<'a>,
    interp: &'a mut Interpreter<'b>,
}

impl<'a, 'b> Resolver<'a, 'b> {
    pub fn new(mgr: &'a ErrorManager, interp: &'a mut Interpreter<'b>) -> Self {
        Self {
            scopes: Vec::new(),
            err: mgr.client(),
            interp,
        }
    }

    pub fn resolve(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Decl((pos, name), None) => {
                self.declare(*pos, name);
                self.define(name);
            }
            Stmt::Decl((pos, name), Some(e)) => {
                self.declare(*pos, name);
                self.resolve_expr(e);
                self.define(name);
            }

            Stmt::Fun((pos, name), items, stmt) => {
                self.declare(*pos, name);
                self.define(name);
                self.resolve_fun(items, stmt);
            }

            Stmt::Expr(expr) | Stmt::Print(expr) | Stmt::Return(Some(expr)) => {
                self.resolve_expr(expr);
            }

            Stmt::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts {
                    self.resolve(stmt);
                }
                self.end_scope();
            }

            Stmt::While(cond, body) | Stmt::If(cond, body, None) => {
                self.resolve_expr(cond);
                self.resolve(body);
            }

            Stmt::If(cond, th, Some(el)) => {
                self.resolve_expr(cond);
                self.resolve(th);
                self.resolve(el);
            }

            _ => (),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(el, _, er) => {
                self.resolve_expr(el);
                self.resolve_expr(er);
            }
            Expr::Unary(_, ex) | Expr::Grouping(_, ex) => self.resolve_expr(ex),
            Expr::Call(fun, _, args) => {
                self.resolve_expr(fun);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Literal(_, _) => (),

            Expr::Ident(pos, name) => {
                if !self.scopes.is_empty() && self.scopes[0].get(name) == Some(&false) {
                    self.err
                        .error(*pos, "Cannot access variable in its initialization");
                }
                self.resolve_local(*pos, name);
            }

            Expr::Assign((pos, name), expr) | Expr::Push((pos, name), expr) => {
                self.resolve_expr(expr);
                self.resolve_local(*pos, name);
            }
        }
    }

    fn declare(&mut self, pos: Position, name: &str) {
        if !self.scopes.is_empty() {
            let x = self.scopes.len() - 1;
            if self.scopes[x].insert(name.into(), false).is_some() {
                self.err.error(pos, "Cannot redeclare variable");
            }
        }
    }

    fn define(&mut self, name: &str) {
        if !self.scopes.is_empty() {
            self.scopes[0].insert(name.into(), true);
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_local(&mut self, pos: Position, name: &str) {
        let mut i = self.scopes.len();
        while i > 0 {
            i -= 1;
            if self.scopes[i].contains_key(name) {
                self.interp.locals.insert(pos, self.scopes.len() - i - 1);
            }
        }
    }

    fn resolve_fun(&mut self, args: &[String], body: &Stmt) {
        self.begin_scope();
        for arg in args {
            self.define(arg);
        }
        self.resolve(body);
        self.end_scope();
    }
}
