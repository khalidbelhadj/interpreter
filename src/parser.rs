use core::{error, panic};
use std::{collections::HashMap, fmt::Display, process::exit};

use crate::ast::*;
use crate::token::*;
use crate::tokeniser::*;
use log::{debug, error};

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

    fn peek(&self) -> Token {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or(Token::new(TokenType::EOF, Span::empty()))
    }

    fn peek_type(&self) -> TokenType {
        self.peek().token_type
    }

    fn advance(&mut self) -> Token {
        self.current += 1;
        self.peek()
    }

    fn consume(&mut self, expected: TokenType) {
        if self.peek_type() == expected {
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
            panic!();
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
        while self.current < self.tokens.len() {
            // Get the top level identifier name, either a struct or proc
            let name = self.consume_identifier();

            // Consume compile time assign operator

            self.consume(TokenType::Colon);
            self.consume(TokenType::Colon);

            let tok = self.peek_type();
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
        let ty = self.peek_type();
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
            TokenType::Float => {
                self.advance();
                Type::Float
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
                    if let TokenType::IntegerLiteral(length) = self.peek_type() {
                        if length < 0 {
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
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            let field_name = self.consume_identifier();
            let field_ty = self.parse_type();
            fields.insert(field_name, field_ty);

            // This ensures that there is no trailing comma
            if self.peek_type() == TokenType::RightBrace {
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
            if self.peek_type() == TokenType::RightParen {
                break;
            }
            let param_name = self.consume_identifier();
            let param_ty = self.parse_type();
            params.insert(param_name, param_ty);

            // Right paren or comma
            if self.peek_type() == TokenType::RightParen {
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
            if self.peek_type() == TokenType::RightParen {
                break;
            }

            let arg_expr = self.parse_expr();
            args.push(arg_expr);

            // Prevents trailing comma
            if self.peek_type() == TokenType::RightParen {
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

        while self.peek_type() != TokenType::RightBrace {
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

                    if self.peek_type() == TokenType::Else {
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
                    let expr = if self.peek_type() == TokenType::Semicolon {
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
                    match self.peek_type() {
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
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            let field_name = self.consume_identifier();
            self.consume(TokenType::Equal);
            let field_expr = self.parse_expr();
            fields.insert(field_name, field_expr);

            // Right brace or comma
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            self.consume(TokenType::Comma);
        }

        self.consume(TokenType::RightBrace);
        fields
    }

    fn parse_expr(&mut self) -> Expr {
        self._parse_expr(false)
    }

    fn parse_condition(&mut self) -> Expr {
        self._parse_expr(true)
    }

    fn _parse_expr(&mut self, is_condition: bool) -> Expr {
        self.parse_disjuncts(is_condition)
    }

    fn parse_disjuncts(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_conjuncts(is_condition);

        while TokenType::Or == self.peek_type() {
            self.advance();
            let rhs = self.parse_conjuncts(is_condition);

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: BinaryOp::Or,
                rhs: Box::new(rhs),
                span: Span::empty(),
            };
        }

        lhs
    }

    fn parse_conjuncts(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_equality(is_condition);

        while TokenType::And == self.peek_type() {
            self.advance();
            let rhs = self.parse_equality(is_condition);

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: BinaryOp::And,
                rhs: Box::new(rhs),
                span: Span::empty(),
            };
        }

        lhs
    }

    fn parse_equality(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_comparison(is_condition);

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                EqualEqual => BinaryOp::Eq,
                NotEqual => BinaryOp::Neq,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_comparison(is_condition);

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::empty(),
            };
        }

        lhs
    }

    fn parse_comparison(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_term(is_condition);

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                Gt => BinaryOp::Gt,
                Geq => BinaryOp::Geq,
                Lt => BinaryOp::Lt,
                Leq => BinaryOp::Leq,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_term(is_condition);

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::empty(),
            };
        }

        lhs
    }

    fn parse_term(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_factor(is_condition);

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                Plus => BinaryOp::Add,
                Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_factor(is_condition);

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::empty(),
            };
        }

        lhs
    }

    fn parse_factor(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_prefix(is_condition);

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                Star => BinaryOp::Mul,
                Slash => BinaryOp::Div,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_prefix(is_condition);

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::empty(),
            };
        }

        lhs
    }

    fn parse_prefix(&mut self, is_condition: bool) -> Expr {
        use TokenType::*;

        match self.peek_type() {
            Not => {
                self.advance();
                let rhs = self.parse_prefix(is_condition);

                return Expr::Unary {
                    op: UnaryOp::Not,
                    rhs: Box::new(rhs),
                    span: Span::empty(),
                };
            }
            Ampersand => {
                self.advance();
                let rhs = self.parse_prefix(is_condition);
                if rhs.is_rvalue() {
                    let tok = self.peek();
                    error!(
                        "{}:{}:{} Can not take reference of rvalue",
                        self.file_path, tok.span.start_line, tok.span.start_column
                    );
                    exit(1);
                }
                return Expr::Ref(Box::new(rhs));
            }
            Star => {
                self.advance();
                let rhs = self.parse_prefix(is_condition);
                return Expr::Deref(Box::new(rhs));
            }
            _ => {}
        }

        if let Not = self.peek_type() {}

        self.parse_postfix(is_condition)
    }

    fn parse_postfix(&mut self, is_condition: bool) -> Expr {
        let mut lhs = self.parse_primary(is_condition);

        loop {
            lhs = match self.peek_type() {
                TokenType::LeftBracket => {
                    self.advance();
                    let index_expr = self.parse_expr();
                    let end_span = self.peek().span.clone();
                    self.consume(TokenType::RightBracket);
                    Expr::Index {
                        expr: Box::new(lhs),
                        index: Box::new(index_expr),
                        span: Span::new(
                            0,
                            0, // TODO: Fix this
                            end_span.end_line,
                            end_span.end_column,
                        ),
                    }
                }
                TokenType::Dot => {
                    self.advance();
                    let field = self.consume_identifier();
                    let end_span = self.peek().span.clone();
                    Expr::Proj {
                        expr: Box::new(lhs),
                        field,
                        span: Span::new(
                            0,
                            0, // TODO: Fix this
                            end_span.end_line,
                            end_span.end_column,
                        ),
                    }
                }
                _ => break,
            };
        }

        lhs
    }

    fn parse_primary(&mut self, is_condition: bool) -> Expr {
        match self.peek_type() {
            TokenType::IntegerLiteral(i) => Expr::Lit(Lit::Int(i, self.advance().span)),
            TokenType::FloatLiteral(f) => Expr::Lit(Lit::Float(f, self.advance().span)),
            TokenType::StringLiteral(s) => Expr::Lit(Lit::Str(s, self.advance().span)),
            TokenType::Ident(_) | TokenType::Hash => {
                // Identifier
                let start_span = self.peek().span.clone();
                let name = if let TokenType::Ident(name) = self.peek_type() {
                    self.advance();
                    name
                } else {
                    self.advance();
                    if let TokenType::Ident(name) = self.peek_type() {
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

                match self.peek_type() {
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
            TokenType::LeftParen => {
                self.advance();
                let inner_expr = self.parse_expr();
                self.consume(TokenType::RightParen);
                inner_expr
            }
            TokenType::LeftBracket => self.parse_array_literal(),
            _ => {
                let tok = self.peek();
                error!(
                    "{}:{}:{} {:?}",
                    self.file_path, tok.span.start_line, tok.span.start_column, tok
                );
                panic!()
            }
        }
    }

    fn parse_array_literal(&mut self) -> Expr {
        self.consume(TokenType::LeftBracket);
        let start_span = self.peek().span.clone();
        let mut elem_exprs = vec![];
        while self.peek_type() != TokenType::RightBracket {
            let elem_expr = self.parse_expr();
            elem_exprs.push(elem_expr);

            if self.peek_type() == TokenType::RightBracket {
                break;
            }

            self.consume(TokenType::Comma);
        }

        let end_span = self.peek().span.clone();
        self.consume(TokenType::RightBracket);
        Expr::Lit(Lit::Array(elem_exprs, Span::join(start_span, end_span)))
    }
}
