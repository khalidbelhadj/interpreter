use std::{collections::HashMap, process::exit};

use crate::tokeniser::*;
use log::{debug, error};

pub type Program = Vec<TopLevel>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopLevel {
    StructDecl(StructDecl),
    ProcDecl(ProcDecl),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: HashMap<String, Type>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ProcDecl {
    pub name: String,
    pub params: HashMap<String, Type>,
    pub ret_ty: Type,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    VarDecl {
        name: String,
        ty: Type,
        expr: Expr,
        span: Span,
    },
    Assign {
        lhs: Expr,
        rhs: Expr,
        span: Span,
    },
    If {
        cond: Expr,
        then_block: Block,
        span: Span,
    },
    IfElse {
        cond: Expr,
        then_block: Block,
        else_block: Block,
        span: Span,
    },
    While {
        cond: Expr,
        block: Block,
        span: Span,
    },
    For {
        name: String,
        from: Expr,
        to: Expr,
        block: Block,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    Ret {
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Unit(Span),
    Lit(Lit),
    Var {
        name: String,
        span: Span,
    },
    Bin {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },

    Proj {
        expr: Box<Expr>,
        field: String,
        span: Span,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Ref(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Unit,
    Int,
    Str,
    Bool,
    Struct(String),
    Array(Box<Type>, ArrayLength),
    Ref(Box<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ArrayLength {
    Fixed(usize),
    Dynamic,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Lit {
    Int(i64, Span),
    Str(String, Span),
    Bool(bool, Span),
    Struct(HashMap<String, Expr>, Span),
    Array(Vec<Expr>, Span),
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

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            Expr::Var { name: _, span: _ }
                | Expr::Proj {
                    expr: _,
                    field: _,
                    span: _
                }
                | Expr::Index {
                    expr: _,
                    index: _,
                    span: _
                }
        )
    }

    pub fn is_rvalue(&self) -> bool {
        !self.is_lvalue()
    }
}

impl BinOp {
    pub fn is_arithmetic(&self) -> bool {
        matches!(self, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div)
    }

    pub fn is_logical(&self) -> bool {
        matches!(self, BinOp::And | BinOp::Or)
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq
        )
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

pub struct Parser {
    file_path: String,
    tokens: Vec<Token>,
    current: usize,
    pub program: Program,
}

impl Parser {
    pub fn new(file_path: String, tokens: Vec<Token>) -> Parser {
        Parser {
            file_path,
            tokens,
            current: 0,
            program: vec![],
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Token {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or(Token::new(TokenType::EOF, Span::new(0, 0, 0, 0)))
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn consume(&mut self, expected: TokenType) {
        if self.peek().token_type == expected {
            self.current += 1;
        } else {
            let tok = self.peek();
            error!(
                "{}:{}:{} Expected token {}, got {}",
                self.file_path,
                tok.span.start_line,
                tok.span.start_column,
                expected,
                tok.token_type
            );
            exit(1);
        }
    }

    fn consume_identifier(&mut self) -> String {
        let tok = self.peek();
        if let TokenType::Ident(name) = tok.token_type {
            self.current += 1;
            return name.clone();
        }

        error!(
            "{}:{}:{} Expected identifier, got {}",
            self.file_path, tok.span.start_line, tok.span.start_column, tok.token_type
        );
        exit(1);
    }

    pub fn parse(&mut self) {
        while !self.is_at_end() {
            // Get the top level identifier name, either a struct or proc
            let name = self.consume_identifier();

            // Consume compile time assign operator
            self.consume(TokenType::Colon);
            self.consume(TokenType::Colon);

            let tok = self.peek().token_type;
            match tok {
                // Proc decleration
                TokenType::LeftParen => {
                    let proc_decl = self.parse_proc(name);
                    self.program.push(TopLevel::ProcDecl(proc_decl));
                }
                // Struct decleration
                TokenType::Struct => {
                    let struct_decl = self.parse_struct(name);
                    self.program.push(TopLevel::StructDecl(struct_decl));
                }
                _ => {
                    let token = self.peek();
                    error!(
                        "{}:{}:{} Expected top-level procedure or struct declaration, got {}",
                        self.file_path, token.span.start_line, token.span.start_column, tok
                    );
                    exit(1);
                }
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        let ty = self.peek().token_type;
        match ty {
            TokenType::Ampersand => {
                self.advance();
                let inner_ty = self.parse_type();
                Type::Ref(Box::new(inner_ty))
            }
            TokenType::Int => {
                self.advance();
                Type::Int
            }
            TokenType::String => {
                self.advance();
                Type::Str
            }
            TokenType::Bool => {
                self.advance();
                Type::Bool
            }
            TokenType::Unit => {
                self.advance();
                Type::Unit
            }
            TokenType::Ident(name) => {
                self.advance();
                Type::Struct(name)
            }
            TokenType::LeftBracket => {
                self.advance();

                // Get the element type
                let elem_ty = self.parse_type();

                let tok = self.peek();

                // Fixed size array
                if let TokenType::Comma = tok.token_type {
                    self.advance();
                    if let TokenType::Integer(length) = self.peek().token_type {
                        self.advance();
                        if length <= 0 {
                            let tok = self.peek();
                            error!(
                                "{}:{}:{} Array length must be greater than 0",
                                self.file_path, tok.span.start_line, tok.span.start_column
                            );
                            exit(1);
                        }

                        self.advance();
                        self.consume(TokenType::RightBracket);
                        return Type::Array(Box::new(elem_ty), ArrayLength::Fixed(length as usize));
                    }

                    let tok = self.peek();
                    error!(
                        "{}:{}:{} Expected integer literal for array length, got {}",
                        self.file_path, tok.span.start_line, tok.span.start_column, tok.token_type
                    );
                    exit(1);
                }

                // Dynamic size array
                if let TokenType::RightBracket = tok.token_type {
                    self.advance();
                    return Type::Array(Box::new(elem_ty), ArrayLength::Dynamic);
                }

                error!(
                    "{}:{}:{} Expected integer literal for array length, got {}",
                    self.file_path, tok.span.start_line, tok.span.start_column, tok.token_type
                );
                exit(1);
            }
            _ => {
                let tok = self.peek();
                error!(
                    "{}:{}:{} Expected type, got {}",
                    self.file_path, tok.span.start_line, tok.span.start_column, tok.token_type
                );
                exit(1);
            }
        }
    }

    fn parse_struct(&mut self, name: String) -> StructDecl {
        let start_span = self.peek().span;
        self.consume(TokenType::Struct);
        self.consume(TokenType::LeftBrace);

        let mut fields = HashMap::new();
        loop {
            if self.peek().token_type == TokenType::RightBrace {
                break;
            }

            let field_name = self.consume_identifier();
            let field_ty = self.parse_type();
            fields.insert(field_name, field_ty);

            // This ensures that there is no trailing comma
            if self.peek().token_type == TokenType::RightBrace {
                break;
            }

            self.consume(TokenType::Comma);
        }

        let end_span = self.peek().span;
        self.consume(TokenType::RightBrace);
        StructDecl {
            name,
            fields,
            span: Span::join(start_span, end_span),
        }
    }

    fn parse_proc(&mut self, name: String) -> ProcDecl {
        let start_span = self.peek().span;
        self.consume(TokenType::LeftParen);

        let mut params = HashMap::new();
        loop {
            // Right paren or "<param> <type>"
            if self.peek().token_type == TokenType::RightParen {
                break;
            }
            let param_name = self.consume_identifier();
            let param_ty = self.parse_type();
            params.insert(param_name, param_ty);

            // Right paren or comma
            if self.peek().token_type == TokenType::RightParen {
                break;
            }

            self.consume(TokenType::Comma);
        }

        self.consume(TokenType::RightParen);

        let ret_ty = self.parse_type();
        let block = self.parse_block();
        let end_span = block.span.clone();

        ProcDecl {
            name,
            params,
            ret_ty,
            block,
            span: Span::join(start_span, end_span),
        }
    }

    fn parse_arguments(&mut self) -> Vec<Expr> {
        self.consume(TokenType::LeftParen);

        let mut args = vec![];
        loop {
            // Right paren or arg expression
            if self.peek().token_type == TokenType::RightParen {
                break;
            }

            let arg_expr = self.parse_expr();
            args.push(arg_expr);

            // Prevents trailing comma
            if self.peek().token_type == TokenType::RightParen {
                break;
            }

            self.consume(TokenType::Comma);
        }

        self.consume(TokenType::RightParen);
        args
    }

    fn parse_block(&mut self) -> Block {
        let start_span = self.peek().span.clone();
        self.consume(TokenType::LeftBrace);

        let mut statements = vec![];

        while self.peek().token_type != TokenType::RightBrace {
            let statement = self.peek();
            match statement.token_type {
                // Statements
                TokenType::Let => {
                    let span = statement.span.clone();
                    self.consume(TokenType::Let);
                    let name = self.consume_identifier();
                    let ty = self.parse_type();
                    self.consume(TokenType::Equal);
                    let expr = self.parse_expr();
                    self.consume(TokenType::Semicolon);
                    statements.push(Stmt::VarDecl {
                        name,
                        ty,
                        expr,
                        span,
                    });
                }
                TokenType::If => {
                    let span = statement.span.clone();
                    self.consume(TokenType::If);
                    let cond = self.parse_condition();
                    let then_block = self.parse_block();

                    if self.peek().token_type == TokenType::Else {
                        self.consume(TokenType::Else);
                        let else_block = self.parse_block();
                        statements.push(Stmt::IfElse {
                            cond,
                            then_block,
                            else_block,
                            span,
                        });
                    } else {
                        statements.push(Stmt::If {
                            cond,
                            then_block,
                            span,
                        });
                    }
                }
                TokenType::While => {
                    let span = statement.span.clone();
                    self.consume(TokenType::While);
                    let cond = self.parse_condition();
                    let block = self.parse_block();
                    statements.push(Stmt::While { cond, block, span });
                }
                TokenType::For => {
                    let span = statement.span.clone();
                    self.consume(TokenType::For);
                    let name = self.consume_identifier();
                    self.consume(TokenType::In);

                    let from = self.parse_expr();
                    self.consume(TokenType::DotDot);
                    let to = self.parse_condition();

                    let block = self.parse_block();
                    statements.push(Stmt::For {
                        name,
                        from,
                        to,
                        block,
                        span,
                    });
                }
                TokenType::Return => {
                    let span = statement.span.clone();
                    self.consume(TokenType::Return);
                    let expr = if self.peek().token_type == TokenType::Semicolon {
                        Expr::Unit(span.clone())
                    } else {
                        self.parse_expr()
                    };

                    self.consume(TokenType::Semicolon);
                    statements.push(Stmt::Ret { expr, span });
                }
                // Try to parse expression for call or assignment
                _ => {
                    let expr = self.parse_expr();
                    match self.peek().token_type {
                        TokenType::Semicolon => {
                            // Check that it's a procedure call
                            let stmt_span = self.peek().span.clone();
                            self.consume(TokenType::Semicolon);
                            if let Expr::Call { name, args, span } = expr {
                                statements.push(Stmt::Call { name, args, span });
                            } else {
                                let tok = self.peek();
                                error!(
                                    "{}:{}:{} Expected statement, got {}",
                                    self.file_path,
                                    tok.span.start_line,
                                    tok.span.start_column,
                                    tok.token_type
                                );
                                exit(1);
                            }
                        }
                        TokenType::Equal => {
                            // Check that the expression is an lvalue
                            let span = self.peek().span.clone();
                            self.consume(TokenType::Equal);
                            let lhs = expr;
                            if !lhs.is_lvalue() {
                                let tok = self.peek();
                                error!(
                                    "{}:{}:{} Expected lvalue, got {}",
                                    self.file_path,
                                    tok.span.start_line,
                                    tok.span.start_column,
                                    tok.token_type
                                );
                                exit(1);
                            }

                            // Parse the right hand side and check for semicolon
                            let rhs = self.parse_expr();
                            self.consume(TokenType::Semicolon);
                            statements.push(Stmt::Assign { lhs, rhs, span });
                        }
                        _ => {
                            let tok = self.peek();
                            error!(
                                "{}:{}:{} Expected statement, got {}",
                                self.file_path,
                                tok.span.start_line,
                                tok.span.start_column,
                                tok.token_type
                            );
                            exit(1);
                        }
                    }
                }
            }
        }

        let end_span = self.peek().span.clone();
        self.consume(TokenType::RightBrace);
        Block {
            statements,
            span: Span::new(
                start_span.start_line,
                start_span.start_column,
                end_span.end_line,
                end_span.end_column,
            ),
        }
    }

    fn parse_struct_literal(&mut self) -> HashMap<String, Expr> {
        self.consume(TokenType::LeftBrace);
        let mut fields = HashMap::new();
        loop {
            // Right brace or "<field> = <expression>"
            if self.peek().token_type == TokenType::RightBrace {
                break;
            }

            let field_name = self.consume_identifier();
            self.consume(TokenType::Equal);
            let field_expr = self.parse_expr();
            fields.insert(field_name, field_expr);

            // Right brace or comma
            if self.peek().token_type == TokenType::RightBrace {
                break;
            }

            self.consume(TokenType::Comma);
        }

        self.consume(TokenType::RightBrace);
        fields
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_expr_prec(Prec::Base, false)
    }

    fn parse_condition(&mut self) -> Expr {
        self.parse_expr_prec(Prec::Base, true)
    }

    fn parse_expr_prec(&mut self, prec: Prec, is_condition: bool) -> Expr {
        let mut lhs = match self.peek().token_type {
            TokenType::Integer(n) => {
                let span = self.peek().span.clone();
                self.advance();
                Expr::Lit(Lit::Int(n, span.clone()))
            }
            TokenType::Hash | TokenType::Ident(_) => {
                // Identifier
                let start_span = self.peek().span.clone();
                let name = if let TokenType::Ident(name) = self.peek().token_type {
                    self.advance();
                    name
                } else {
                    self.advance();
                    if let TokenType::Ident(name) = self.peek().token_type {
                        self.advance();
                        format!("#{name}")
                    } else {
                        let tok = self.peek();
                        error!(
                            "{}:{}:{} Expected identifier, got {}",
                            self.file_path,
                            tok.span.start_line,
                            tok.span.start_column,
                            tok.token_type
                        );
                        exit(1);
                    }
                };

                match self.peek().token_type {
                    TokenType::LeftParen => {
                        let args = self.parse_arguments();
                        let end_span = self.peek().span.clone();
                        Expr::Call {
                            name,
                            args,
                            span: Span::new(
                                start_span.start_line,
                                start_span.start_column,
                                end_span.end_line,
                                end_span.end_column,
                            ),
                        }
                    }
                    TokenType::LeftBrace => {
                        // To disambiguate between struct literal and blocks
                        if is_condition {
                            return Expr::Var {
                                name,
                                span: start_span,
                            };
                        }

                        let fields = self.parse_struct_literal();
                        let end_span = self.peek().span.clone();
                        Expr::Lit(Lit::Struct(fields, Span::join(start_span, end_span)))
                    }
                    _ => Expr::Var {
                        name,
                        span: start_span,
                    },
                }
            }
            TokenType::StringLiteral(s) => {
                let span = self.peek().span.clone();
                self.advance();
                Expr::Lit(Lit::Str(s, span.clone()))
            }
            TokenType::True => {
                let span = self.peek().span.clone();
                self.advance();
                Expr::Lit(Lit::Bool(true, span.clone()))
            }
            TokenType::False => {
                let span = self.peek().span.clone();
                self.advance();
                Expr::Lit(Lit::Bool(false, span.clone()))
            }
            TokenType::LeftParen => {
                let start_span = self.peek().span.clone();
                self.advance();
                let expr = self.parse_expr(); // Parse inner expression
                if let TokenType::RightParen = self.peek().token_type {
                    let end_span = self.peek().span.clone();
                    self.advance();
                    expr
                } else {
                    let tok = self.peek();
                    error!(
                        "{}:{}:{} Expected ')' after expression",
                        self.file_path, tok.span.start_line, tok.span.start_column
                    );
                    exit(1);
                }
            }
            TokenType::LeftBracket => {
                let start_span = self.peek().span.clone();
                self.advance();
                let mut elem_exprs = vec![];
                while self.peek().token_type != TokenType::RightBracket {
                    let elem_expr = self.parse_expr();
                    elem_exprs.push(elem_expr);

                    if self.peek().token_type == TokenType::RightBracket {
                        break;
                    }

                    self.consume(TokenType::Comma);
                }

                let end_span = self.peek().span.clone();
                self.consume(TokenType::RightBracket);
                Expr::Lit(Lit::Array(elem_exprs, Span::join(start_span, end_span)))
            }

            TokenType::Ampersand => {
                self.advance();
                let expr = self.parse_expr();
                return Expr::Ref(Box::new(expr));
            }
            _ => {
                let tok = self.peek();
                error!(
                    "{}:{}:{} Unexpected token while parsing expression: {}",
                    self.file_path, tok.span.start_line, tok.span.start_column, tok.token_type
                );
                exit(1);
            }
        };

        loop {
            if (self.peek().token_type == TokenType::LeftBracket) {
                self.advance();
                let index_expr = self.parse_expr();
                let end_span = self.peek().span.clone();
                self.consume(TokenType::RightBracket);
                lhs = Expr::Index {
                    expr: Box::new(lhs),
                    index: Box::new(index_expr),
                    span: Span::new(
                        0,
                        0, // TODO: Fix this
                        end_span.end_line,
                        end_span.end_column,
                    ),
                };
            }
            if (self.peek().token_type == TokenType::Dot) {
                self.advance();
                let field = self.consume_identifier();
                let end_span = self.peek().span.clone();
                lhs = Expr::Proj {
                    expr: Box::new(lhs),
                    field,
                    span: Span::new(
                        0,
                        0, // TODO: Fix this
                        end_span.end_line,
                        end_span.end_column,
                    ),
                }
            } else {
                break;
            }
        }

        while let Some(op) = self.peek_binary_operator() {
            let op_prec = op.precedence();
            if op_prec <= prec {
                break;
            }

            let span = self.peek().span.clone();
            self.advance(); // Consume operator
            let rhs = self.parse_expr_prec(op_prec, false); // Parse right-hand side with higher precedence
            lhs = Expr::Bin {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span,
            };
        }

        lhs
    }

    fn peek_binary_operator(&mut self) -> Option<BinOp> {
        match self.peek().token_type {
            TokenType::Plus => Some(BinOp::Add),
            TokenType::Minus => Some(BinOp::Sub),
            TokenType::Star => Some(BinOp::Mul),
            TokenType::Slash => Some(BinOp::Div),
            TokenType::And => Some(BinOp::And),
            TokenType::Or => Some(BinOp::Or),
            TokenType::EqualEqual => Some(BinOp::Eq),
            TokenType::NotEqual => Some(BinOp::Neq),
            TokenType::Less => Some(BinOp::Lt),
            TokenType::Greater => Some(BinOp::Gt),
            TokenType::LessEqual => Some(BinOp::Leq),
            TokenType::GreaterEqual => Some(BinOp::Geq),
            _ => None,
        }
    }
}
