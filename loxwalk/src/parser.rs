use crate::expr::{Bin, Expr, Un, Value};
use crate::reporting::{ErrorClient, ErrorManager, Position};
use crate::stmt::Stmt;
use crate::tokens::{Grammar, Keyword, Op, Payload, Token};

#[derive(Debug, Clone)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    err: ErrorClient<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(mgr: &'a ErrorManager, tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current: 0,
            err: mgr.client(),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut ret = Vec::new();
        while !self.is_at_end() {
            let Some(s) = self.declaration() else {
                break;
            };
            ret.push(s);
        }
        ret
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
            || self.tokens[self.current].load == Payload::Grammar(Grammar::EOF)
    }

    pub fn declaration(&mut self) -> Option<Stmt> {
        let Token { pos: _, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Keyword(Keyword::Var)) => self.decl_stmt(),

            _ => {
                self.current -= 1;
                self.statement()
            }
        }
    }

    fn statement(&mut self) -> Option<Stmt> {
        let Token { pos: _, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Keyword(Keyword::Print)) => self.print_stmt(),
            Payload::Grammar(Grammar::LB) => Some(Stmt::Block(self.block()?)),

            _ => {
                self.current -= 1;
                self.expr_stmt()
            }
        }
    }

    fn expression(&mut self) -> Option<Expr> {
        self.assignment()
    }

    fn block(&mut self) -> Option<Vec<Stmt>> {
        let mut v = Vec::new();
        while !self.is_at_end() && self.check(Grammar::RB).is_err() {
            v.push(self.declaration()?);
        }
        Some(v)
    }
}

// expression guts
impl Parser<'_> {
    fn assignment(&mut self) -> Option<Expr> {
        let lhs = self.equality()?;
        if self.check(Grammar::Op(Op::Assign)).is_ok() {
            let rhs = self.assignment()?;
            match lhs {
                Expr::Ident(pos, name) => Some(Expr::Assign((pos, name), Box::new(rhs))),
                lhs => {
                    self.err.error(lhs.pos(), "Cannot assign to expression");
                    None
                }
            }
        } else if self.check(Grammar::Op(Op::Push)).is_ok() {
            let rhs = self.assignment()?;
            match lhs {
                Expr::Ident(pos, name) => Some(Expr::Push((pos, name), Box::new(rhs))),
                lhs => {
                    self.err.error(lhs.pos(), "Cannot assign to expression");
                    None
                }
            }
        } else {
            Some(lhs)
        }
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut running = self.comparison()?;
        while let Some(op) = self.equality_op() {
            if let Some(rhs) = self.comparison() {
                running = Expr::Binary(Box::new(running), op, Box::new(rhs));
            } else {
                break;
            }
        }
        Some(running)
    }

    fn equality_op(&mut self) -> Option<(Position, Bin)> {
        let Token { pos, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Op(Op::Equal)) => Some((*pos, Bin::Eql)),
            Payload::Grammar(Grammar::Op(Op::NotEqual)) => Some((*pos, Bin::Neq)),
            _ => {
                self.current -= 1;
                None
            }
        }
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut running = self.term()?;
        while let Some(op) = self.comparison_op() {
            if let Some(rhs) = self.term() {
                running = Expr::Binary(Box::new(running), op, Box::new(rhs));
            } else {
                break;
            }
        }
        Some(running)
    }

    fn comparison_op(&mut self) -> Option<(Position, Bin)> {
        let Token { pos, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Op(Op::LE)) => Some((*pos, Bin::Le)),
            Payload::Grammar(Grammar::Op(Op::LT)) => Some((*pos, Bin::Lt)),
            Payload::Grammar(Grammar::Op(Op::GE)) => Some((*pos, Bin::Ge)),
            Payload::Grammar(Grammar::Op(Op::GT)) => Some((*pos, Bin::Gt)),
            _ => {
                self.current -= 1;
                None
            }
        }
    }

    fn term(&mut self) -> Option<Expr> {
        let mut running = self.factor()?;
        while let Some(op) = self.term_op() {
            if let Some(rhs) = self.factor() {
                running = Expr::Binary(Box::new(running), op, Box::new(rhs));
            } else {
                break;
            }
        }
        Some(running)
    }

    fn term_op(&mut self) -> Option<(Position, Bin)> {
        let Token { pos, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Op(Op::Plus)) => Some((*pos, Bin::Add)),
            Payload::Grammar(Grammar::Op(Op::Minus)) => Some((*pos, Bin::Sub)),
            _ => {
                self.current -= 1;
                None
            }
        }
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut running = self.unary()?;
        while let Some(op) = self.factor_op() {
            if let Some(rhs) = self.unary() {
                running = Expr::Binary(Box::new(running), op, Box::new(rhs));
            } else {
                break;
            }
        }
        Some(running)
    }

    fn factor_op(&mut self) -> Option<(Position, Bin)> {
        let Token { pos, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Op(Op::Star)) => Some((*pos, Bin::Mul)),
            Payload::Grammar(Grammar::Op(Op::Slash)) => Some((*pos, Bin::Div)),
            _ => {
                self.current -= 1;
                None
            }
        }
    }

    fn unary(&mut self) -> Option<Expr> {
        let Token { pos, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Op(Op::Bang)) => {
                Some(Expr::Unary((*pos, Un::Not), Box::new(self.unary()?)))
            }
            Payload::Grammar(Grammar::Op(Op::Minus)) => {
                Some(Expr::Unary((*pos, Un::Neg), Box::new(self.unary()?)))
            }
            _ => {
                self.current -= 1;
                self.primary()
            }
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        let Token { pos, load } = &self.tokens[self.current];
        self.current += 1;
        match load {
            Payload::Grammar(Grammar::Keyword(Keyword::True)) => {
                Some(Expr::Literal(*pos, Value::Bool(true)))
            }
            Payload::Grammar(Grammar::Keyword(Keyword::False)) => {
                Some(Expr::Literal(*pos, Value::Bool(false)))
            }
            Payload::Grammar(Grammar::Keyword(Keyword::Nil)) => {
                Some(Expr::Literal(*pos, Value::Nil))
            }
            Payload::String(s) => Some(Expr::Literal(*pos, Value::Str(s.clone()))),
            Payload::Number(x) => Some(Expr::Literal(*pos, Value::Num(*x))),
            Payload::Grammar(Grammar::LP) => {
                let interior = self.expression()?;
                match self.closep() {
                    Ok(end) => Some(Expr::Grouping((*pos..end).into(), Box::new(interior))),
                    Err(pos) => {
                        self.err.error(pos, "Unclosed parenthetical");
                        None
                    }
                }
            }
            Payload::Grammar(g) => {
                self.err.error(*pos, &format!("Unexpected {g}"));
                None
            }
            Payload::Ident(n) => Some(Expr::Ident(*pos, n.clone())),
        }
    }

    fn closep(&mut self) -> Result<Position, Position> {
        let Token { pos, load } = &self.tokens[self.current];
        if *load == Payload::Grammar(Grammar::RP) {
            self.current += 1;
            Ok(*pos)
        } else {
            Err(*pos)
        }
    }
}

// statement guts
impl Parser<'_> {
    fn check(&mut self, g: Grammar) -> Result<Position, Position> {
        let Token { pos, load } = &self.tokens[self.current];
        if load == &Payload::Grammar(g) {
            self.current += 1;
            Ok(*pos)
        } else {
            Err(*pos)
        }
    }

    fn semicolon(&mut self) -> Option<()> {
        if let Err(pos) = self.check(Grammar::Semicolon) {
            self.err.error(pos, "Expected semicolon");
            None
        } else {
            Some(())
        }
    }

    fn expr_stmt(&mut self) -> Option<Stmt> {
        let e = self.expression()?;
        self.semicolon()?;
        Some(Stmt::Expr(e))
    }

    fn print_stmt(&mut self) -> Option<Stmt> {
        let e = self.expression()?;
        self.semicolon()?;
        Some(Stmt::Print(e))
    }

    fn decl_name(&mut self) -> Option<String> {
        let Token { pos, load } = &self.tokens[self.current];
        let Payload::Ident(name) = load else {
            self.err.error(*pos, "Expected variable name");
            return None;
        };
        self.current += 1;
        Some(name.clone())
    }

    fn decl_stmt(&mut self) -> Option<Stmt> {
        let name = self.decl_name()?;
        let value = match self.check(Grammar::Op(Op::Assign)) {
            Ok(_) => Some(self.expression()?),
            Err(_) => None,
        };
        self.semicolon();
        Some(Stmt::Decl(name, value))
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
