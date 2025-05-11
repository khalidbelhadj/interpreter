use std::collections::HashMap;

use crate::tokeniser::*;

pub type Program = Vec<TopLevel>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopLevel {
    RecDecl(RecDecl),
    FunDecl(FunDecl),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RecDecl {
    pub name: String,
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ArrayLength {
    Fixed(usize),
    Dynamic,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Unit,
    Int,
    Str,
    Bool,
    Struct(String),
    Array(Box<Type>, ArrayLength),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub returns: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunDecl {
    pub name: String,
    pub params: HashMap<String, Type>,
    pub ret_ty: Type,
    pub body: Block,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    VarDecl(String, Type, Expr),
    Assign(AssignTarget, Expr),
    If(Expr, Block),
    IfElse(Expr, Block, Block),
    While(Expr, Block),
    For(String, Expr, Expr, Block),
    Call(String, Vec<Expr>),
    Ret(Expr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AssignTarget {
    Var(String),
    Proj(String, String),
    Index(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Unit,
    Lit(Lit),
    Var(String),
    Bin(Box<Expr>, BinOp, Box<Expr>),
    Call(String, Vec<Expr>),
    Proj(String, String),
    Index(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Lit {
    Int(i64),
    Str(String),
    Bool(bool),
    Struct(HashMap<String, Expr>),
    Array(Vec<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,

    And,
    Or,
}

impl BinOp {
    pub fn is_arithmetic(&self) -> bool {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            BinOp::And | BinOp::Or => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => true,
            _ => false,
        }
    }
}

impl BinOp {
    pub fn precedence(&self) -> Prec {
        match self {
            BinOp::Or => Prec::LogicalOr,
            BinOp::And => Prec::LogicalAnd,
            BinOp::Eq | BinOp::Neq => Prec::Equality,
            BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq => Prec::Relational,
            BinOp::Add | BinOp::Sub => Prec::Additive,
            BinOp::Mul | BinOp::Div => Prec::Multiplicative,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Prec {
    Base, // Lowest precedence for non-operators
    Dot,
    Index,
    Call,
    LogicalOr,      // ||
    LogicalAnd,     // &&
    Equality,       // ==, !=
    Relational,     // <, >, <=, >=
    Additive,       // +, -
    Multiplicative, // *, /
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    pub program: Program,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            program: vec![],
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Token {
        self.tokens.get(self.current).cloned().unwrap_or(Token::EOF)
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn consume(&mut self, expected: Token) {
        if self.peek() == expected {
            self.current += 1;
        } else {
            // print last 10 tokens
            let start = if self.current >= 10 {
                self.current - 10
            } else {
                0
            };
            let end = self.current + 10;
            let tokens = &self.tokens[start..end];
            println!("Tokens: {:?}", tokens);
            panic!("Expected token {:?}, got {:?}", expected, self.peek());
        }
    }

    fn consume_identifier(&mut self) -> String {
        if let Token::Ident(name) = self.peek() {
            self.current += 1;
            name.clone()
        } else {
            panic!("Expected identifier, got {:?}", self.peek());
        }
    }

    pub fn parse(&mut self) {
        while !self.is_at_end() {
            let name = match self.peek() {
                Token::Ident(name) => name,
                Token::EOF => break,
                _ => panic!("Expected identifier, got {:?}", self.peek()),
            };

            self.advance();

            // Consume compile time assign operator
            self.consume(Token::Colon);
            self.consume(Token::Colon);

            let tok = self.peek();
            match tok {
                Token::LeftParen => {
                    let fun = self.parse_func(name);
                    self.program.push(TopLevel::FunDecl(fun));
                }
                Token::Struct => {
                    let struct_def = self.parse_struct(name);
                    self.program.push(TopLevel::RecDecl(struct_def));
                }
                _ => panic!(
                    "Expected top-level function or struct decleration, got {:?}",
                    tok
                ),
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        let ty = self.peek();
        match ty {
            Token::Int => {
                self.advance();
                Type::Int
            }
            Token::String => {
                self.advance();
                Type::Str
            }
            Token::Bool => {
                self.advance();
                Type::Bool
            }
            Token::Unit => {
                self.advance();
                Type::Unit
            }
            Token::Ident(name) => {
                self.advance();
                Type::Struct(name)
            }
            Token::LeftBracket => {
                self.advance();
                let elem_ty = self.parse_type();
                self.consume(Token::Comma);
                // Parse int literal for array size
                match self.peek() {
                    Token::Integer(size) => {
                        self.advance();
                        if size <= 0 {
                            panic!("Array size must be greater than 0");
                        }

                        self.consume(Token::RightBracket);
                        return Type::Array(Box::new(elem_ty), ArrayLength::Fixed(size as usize));
                    }
                    Token::QuestionMark => {
                        self.advance();
                        self.consume(Token::RightBracket);
                        return Type::Array(Box::new(elem_ty), ArrayLength::Dynamic);
                    }
                    _ => {
                        panic!(
                            "Expected integer literal for array size, got {:?}",
                            self.peek()
                        );
                    }
                }
            }
            _ => panic!("Expected type, got {:?}", self.peek()),
        }
    }

    fn parse_struct(&mut self, name: String) -> RecDecl {
        self.consume(Token::Struct);
        self.consume(Token::LeftBrace);

        let mut fields = HashMap::new();
        loop {
            if self.peek() == Token::RightBrace {
                break;
            }

            let label = self.consume_identifier();
            let ty = self.parse_type();
            fields.insert(label, ty);

            if self.peek() == Token::RightBrace {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightBrace);
        RecDecl { name, fields }
    }

    fn parse_func(&mut self, name: String) -> FunDecl {
        self.consume(Token::LeftParen);

        let mut params = HashMap::new();
        loop {
            // Right paren or "<param> <type>"
            if self.peek() == Token::RightParen {
                break;
            }
            let param = self.consume_identifier();
            let ty = self.parse_type();
            params.insert(param, ty);

            // Right paren or comma
            if self.peek() == Token::RightParen {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightParen);

        FunDecl {
            name,
            params,
            ret_ty: self.parse_type(),
            body: self.parse_block(),
        }
    }

    fn parse_arguments(&mut self) -> Vec<Expr> {
        self.consume(Token::LeftParen);

        let mut args = vec![];
        loop {
            // Right paren or "<param> <type>"
            if self.peek() == Token::RightParen {
                break;
            }

            let expr = self.parse_expr();
            args.push(expr);

            // Right paren or comma
            if self.peek() == Token::RightParen {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightParen);
        args
    }

    fn parse_block(&mut self) -> Block {
        self.consume(Token::LeftBrace);

        let mut statements = vec![];
        let mut returns = false;

        while self.peek() != Token::RightBrace {
            let statement = self.peek();
            match statement {
                // Statements
                Token::Let => {
                    self.consume(Token::Let);
                    let name = self.consume_identifier();
                    let ty = self.parse_type();
                    self.consume(Token::Equal);
                    let expr = self.parse_expr();
                    self.consume(Token::Semicolon);
                    statements.push(Stmt::VarDecl(name, ty, expr));
                }
                Token::If => {
                    self.consume(Token::If);
                    let cond = self.parse_condition();
                    println!("cond: {:?}", cond);
                    let if_block = self.parse_block();

                    if self.peek() == Token::Else {
                        self.consume(Token::Else);
                        let else_block = self.parse_block();
                        statements.push(Stmt::IfElse(cond, if_block, else_block));
                    } else {
                        statements.push(Stmt::If(cond, if_block));
                    }
                }
                Token::While => {
                    self.consume(Token::While);
                    let cond = self.parse_condition();
                    let block = self.parse_block();
                    statements.push(Stmt::While(cond, block));
                }
                Token::For => {
                    self.consume(Token::For);
                    let name = self.consume_identifier();
                    self.consume(Token::In);

                    let from = self.parse_expr();
                    self.consume(Token::DotDot);
                    let to = self.parse_expr();

                    let block = self.parse_block();
                    statements.push(Stmt::For(name, from, to, block));
                }
                Token::Return => {
                    returns = true;
                    self.consume(Token::Return);
                    let expr = if self.peek() == Token::Semicolon {
                        Expr::Unit
                    } else {
                        self.parse_expr()
                    };

                    self.consume(Token::Semicolon);
                    statements.push(Stmt::Ret(expr));
                }
                // Try to parse expressions
                _ => {
                    let expr = self.parse_expr();
                    match self.peek() {
                        Token::Semicolon => {
                            // Check that it's a function call
                            self.consume(Token::Semicolon);
                            if let Expr::Call(name, args) = expr {
                                statements.push(Stmt::Call(name, args));
                            } else {
                                panic!("Expected function call, got {:?}", expr);
                            }
                        }
                        Token::Equal => {
                            // Check that the expression is an lvalue
                            self.consume(Token::Equal);
                            let target = match expr {
                                Expr::Var(name) => AssignTarget::Var(name),
                                Expr::Proj(name, label) => AssignTarget::Proj(name, label),
                                Expr::Index(name, index) => AssignTarget::Index(name, index),
                                _ => panic!("Expected lvalue, got {:?}", expr),
                            };
                            // Parse the right hand side and check for semicolon
                            let rhs = self.parse_expr();
                            self.consume(Token::Semicolon);
                            statements.push(Stmt::Assign(target, rhs));
                        }
                        _ => {
                            panic!(
                                "Expected function call or assignment, got {:?}",
                                self.peek()
                            );
                        }
                    }
                }
            }
        }

        self.consume(Token::RightBrace);
        Block {
            statements,
            returns,
        }
    }

    fn parse_struct_literal(&mut self) -> HashMap<String, Expr> {
        self.consume(Token::LeftBrace);
        let mut fields = HashMap::new();
        loop {
            // Right brace or "<field> = <expression>"
            if self.peek() == Token::RightBrace {
                break;
            }

            let field = self.consume_identifier();
            self.consume(Token::Equal);
            let expr = self.parse_expr();
            fields.insert(field, expr);

            // Right brace or comma
            if self.peek() == Token::RightBrace {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightBrace);
        fields
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_expr_prec(Prec::Base, false)
    }

    fn parse_condition(&mut self) -> Expr {
        self.parse_expr_prec(Prec::Base, true)
    }

    fn parse_expr_prec(&mut self, prec: Prec, is_condition: bool) -> Expr {
        let mut left = match self.peek() {
            Token::Integer(n) => {
                self.advance();
                Expr::Lit(Lit::Int(n))
            }
            Token::Hash | Token::Ident(_) => {
                let name = if let Token::Ident(name) = self.peek() {
                    self.advance();
                    name
                } else {
                    self.advance();
                    if let Token::Ident(name) = self.peek() {
                        self.advance();
                        format!("#{name}")
                    } else {
                        panic!("Expected identifier, got {:?}", self.peek());
                    }
                };

                match self.peek() {
                    Token::LeftParen => {
                        let args = self.parse_arguments();
                        Expr::Call(name, args)
                    }
                    Token::Dot => {
                        self.advance();
                        let label = self.consume_identifier();
                        Expr::Proj(name, label)
                    }
                    Token::LeftBracket => {
                        self.advance();
                        let index = self.parse_expr();
                        self.consume(Token::RightBracket);
                        Expr::Index(name, Box::new(index))
                    }
                    Token::LeftBrace => {
                        if is_condition {
                            return Expr::Var(name);
                        }

                        let fields = self.parse_struct_literal();
                        Expr::Lit(Lit::Struct(fields))
                    }
                    _ => Expr::Var(name),
                }
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expr::Lit(Lit::Str(s))
            }
            Token::True => {
                self.advance();
                Expr::Lit(Lit::Bool(true))
            }
            Token::False => {
                self.advance();
                Expr::Lit(Lit::Bool(false))
            }
            Token::LeftParen => {
                self.advance();
                let expr = self.parse_expr(); // Parse inner expression
                if let Token::RightParen = self.peek() {
                    self.advance();
                    expr
                } else {
                    panic!("Expected ')' after expression"); // Replace with proper error handling
                }
            }
            Token::LeftBracket => {
                self.advance();
                let mut elems = vec![];
                while self.peek() != Token::RightBracket {
                    let expr = self.parse_expr();
                    elems.push(expr);

                    if self.peek() == Token::RightBracket {
                        break;
                    }

                    self.consume(Token::Comma);
                }

                self.consume(Token::RightBracket);
                Expr::Lit(Lit::Array(elems))
            }
            _ => panic!("Unexpected token while parsing expression"), // Replace with proper error handling
        };

        while let Some(op) = self.peek_binary_operator() {
            let op_prec = op.precedence();
            if op_prec <= prec {
                break;
            }

            self.advance(); // Consume operator
            let right = self.parse_expr_prec(op_prec, false); // Parse right-hand side with higher precedence
            left = Expr::Bin(Box::new(left), op, Box::new(right));
        }

        left
    }

    fn peek_binary_operator(&mut self) -> Option<BinOp> {
        match self.peek() {
            Token::Plus => Some(BinOp::Add),
            Token::Minus => Some(BinOp::Sub),
            Token::Star => Some(BinOp::Mul),
            Token::Slash => Some(BinOp::Div),
            Token::And => Some(BinOp::And),
            Token::Or => Some(BinOp::Or),
            Token::EqualEqual => Some(BinOp::Eq),
            Token::NotEqual => Some(BinOp::Neq),
            Token::Less => Some(BinOp::Lt),
            Token::Greater => Some(BinOp::Gt),
            Token::LessEqual => Some(BinOp::Leq),
            Token::GreaterEqual => Some(BinOp::Geq),
            _ => None,
        }
    }
}
