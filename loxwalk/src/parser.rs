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

    fn trace(&self) -> String {
        let mut ret = String::new();
        for i in self.current.saturating_sub(1)..=self.current + 1 {
            if i < self.tokens.len() {
                ret += &self.tokens[i].to_string();
            }
        }
        ret
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
            Payload::Grammar(Grammar::Keyword(Keyword::Fun)) => self.fun_decl(),

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
            Payload::Grammar(Grammar::Keyword(Keyword::If)) => self.if_stmt(),
            Payload::Grammar(Grammar::Keyword(Keyword::While)) => self.while_stmt(),
            Payload::Grammar(Grammar::Keyword(Keyword::For)) => self.for_stmt(),

            Payload::Grammar(Grammar::Keyword(Keyword::Break)) => {
                self.semicolon()?;
                Some(Stmt::Break)
            }
            Payload::Grammar(Grammar::Keyword(Keyword::Continue)) => {
                self.semicolon()?;
                Some(Stmt::Continue)
            }
            Payload::Grammar(Grammar::Keyword(Keyword::Return)) => {
                if self.check(Grammar::Semicolon).is_err() {
                    let e = self.expression()?;
                    self.semicolon()?;
                    Some(Stmt::Return(Some(e)))
                } else {
                    Some(Stmt::Return(None))
                }
            }

            Payload::Grammar(Grammar::Semicolon) => Some(Stmt::NOP),

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

const EXPRTRACE: bool = false;
// expression guts
impl Parser<'_> {
    fn assignment(&mut self) -> Option<Expr> {
        if EXPRTRACE {
            println!("assignment, {}", self.trace());
        }
        let lhs = self.or_expr()?;
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

    fn or_expr(&mut self) -> Option<Expr> {
        if EXPRTRACE {
            println!("or_expr, {}", self.trace());
        }
        let mut running = self.and_expr()?;
        while let Ok(pos) = self.check(Grammar::Keyword(Keyword::Or)) {
            if let Some(rhs) = self.and_expr() {
                running = Expr::Binary(Box::new(running), (pos, Bin::Or), Box::new(rhs));
            } else {
                break;
            }
        }
        Some(running)
    }

    fn and_expr(&mut self) -> Option<Expr> {
        if EXPRTRACE {
            println!("and_expr, {}", self.trace());
        }
        let mut running = self.equality()?;
        while let Ok(pos) = self.check(Grammar::Keyword(Keyword::And)) {
            if let Some(rhs) = self.equality() {
                running = Expr::Binary(Box::new(running), (pos, Bin::And), Box::new(rhs));
            } else {
                break;
            }
        }
        Some(running)
    }

    fn equality(&mut self) -> Option<Expr> {
        if EXPRTRACE {
            println!("equality, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("equality_op, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("comparison, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("comparison_op, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("term, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("term_op, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("factor, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("factor_op, {}", self.trace());
        }
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
        if EXPRTRACE {
            println!("unary, {}", self.trace());
        }
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
                self.fcall()
            }
        }
    }

    fn fcall(&mut self) -> Option<Expr> {
        if EXPRTRACE {
            println!("fcall, {}", self.trace());
        }
        let mut function = self.primary()?;
        while let Ok(beg) = self.check(Grammar::LP) {
            let mut args = Vec::new();
            let mut end = beg;
            if let Err(ending) = self.check(Grammar::RP) {
                end = ending;
                args.push(self.expression()?);
                while self.check(Grammar::RP).is_err() {
                    self.consume(
                        Grammar::Comma,
                        "Function argument must be followed by paren or comma",
                    );
                    args.push(self.expression()?);
                }
            }
            if args.len() > 255 {
                self.err
                    .error((beg..end).into(), "Cannot have more than 255 arguments");
            }
            function = Expr::Call(Box::new(function), (beg..end).into(), args);
        }
        Some(function)
    }

    fn primary(&mut self) -> Option<Expr> {
        if EXPRTRACE {
            println!("primary, {}", self.trace());
        }
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

    fn consume(&mut self, g: Grammar, msg: &str) -> Option<()> {
        if let Err(pos) = self.check(g) {
            self.err.error(pos, msg);
            None
        } else {
            Some(())
        }
    }

    fn semicolon(&mut self) -> Option<()> {
        self.consume(Grammar::Semicolon, "Expected semicolon")
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

    fn get_name(&mut self, msg: &str) -> Option<(Position, String)> {
        let Token { pos, load } = &self.tokens[self.current];
        let Payload::Ident(name) = load else {
            self.err.error(*pos, msg);
            return None;
        };
        self.current += 1;
        Some((*pos, name.clone()))
    }

    fn arg_name(&mut self) -> Option<String> {
        self.get_name("Expected function argument").map(|x| x.1)
    }

    fn fun_name(&mut self) -> Option<(Position, String)> {
        self.get_name("Expected function name")
    }

    fn decl_name(&mut self) -> Option<(Position, String)> {
        self.get_name("Expected variable name")
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

    fn fun_decl(&mut self) -> Option<Stmt> {
        let fname = self.fun_name()?;
        self.consume(Grammar::LP, "Expected argument list")?;
        let mut args = Vec::new();
        if self.check(Grammar::RP).is_err() {
            args.push(self.arg_name()?);
            while !self.is_at_end() && self.check(Grammar::Comma).is_ok() {
                args.push(self.arg_name()?);
            }
            self.consume(Grammar::RP, "Argument list must end")?;
        }
        let body = self.statement()?;
        Some(Stmt::Fun(fname, args, Box::new(body)))
    }

    fn if_stmt(&mut self) -> Option<Stmt> {
        let cond = self.expression()?;
        let result = self.statement()?;
        if self.check(Grammar::Keyword(Keyword::Else)).is_ok() {
            let fallback = self.statement()?;
            Some(Stmt::If(cond, Box::new(result), Some(Box::new(fallback))))
        } else {
            Some(Stmt::If(cond, Box::new(result), None))
        }
    }

    fn while_stmt(&mut self) -> Option<Stmt> {
        let cond = self.expression()?;
        let int = self.statement()?;
        Some(Stmt::While(cond, Box::new(int)))
    }

    fn for_stmt(&mut self) -> Option<Stmt> {
        self.consume(Grammar::LP, "For loops must have parentheses")?;
        let init = if self.check(Grammar::Semicolon).is_ok() {
            Stmt::NOP
        } else if self.check(Grammar::Keyword(Keyword::Var)).is_ok() {
            self.decl_stmt()?
        } else {
            self.expr_stmt()?
        };
        let cond = self.expression()?;
        self.semicolon()?;
        let update = if let Ok(pos) = self.check(Grammar::RP) {
            Expr::Literal(pos, Value::Nil)
        } else {
            let e = self.expression()?;
            self.check(Grammar::RP).ok()?;
            e
        };
        let body = self.statement()?;
        Some(Stmt::Block(vec![
            init,
            Stmt::While(cond, Box::new(Stmt::Block(vec![body, Stmt::Expr(update)]))),
        ]))
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
