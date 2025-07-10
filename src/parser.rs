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
            program: Program { top_levels: vec![] },
        }
    }

    pub fn parse(&mut self) -> Result<(), ParseError> {
        while self.current < self.tokens.len() {
            // Get the top level identifier name, either a struct or proc
            let name = self.consume_identifier()?;

            // Consume compile time assign operator
            self.consume(TokenType::Colon)?;
            self.consume(TokenType::Colon)?;

            let tok = self.peek_type();
            match tok {
                // Proc decleration
                TokenType::LeftParen => {
                    let proc_decl = self.parse_proc_decl(name)?;
                    self.program.top_levels.push(TopLevel::ProcDecl(proc_decl));
                }
                // Struct decleration
                TokenType::Struct => {
                    let struct_decl = self.parse_struct_decl(name)?;
                    self.program
                        .top_levels
                        .push(TopLevel::StructDecl(struct_decl));
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken {
                            expected: TokenSet::Text("top-level".to_string()),
                            actual: self.peek_type(),
                        },
                        span: self.peek_span(),
                    });
                }
            }
        }

        Ok(())
    }

    fn parse_struct_decl(&mut self, name: String) -> Result<StructDecl, ParseError> {
        let start_span = self.peek_span();
        self.consume(TokenType::Struct)?;
        self.consume(TokenType::LeftBrace)?;

        let mut fields = HashMap::new();
        loop {
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            let field_name = self.consume_identifier()?;
            let field_ty = self.parse_type()?;

            let mut default_value: Option<Expr> = None;

            if self.peek_type() == TokenType::Equal {
                self.advance();
                default_value = Some(self.parse_expr()?);
            }

            fields.insert(field_name, (field_ty, default_value));

            // This ensures that there is no trailing comma
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            self.consume(TokenType::Comma)?;
        }

        let end_span = self.peek_span();
        self.consume(TokenType::RightBrace)?;
        Ok(StructDecl {
            name,
            fields,
            span: Span::join(start_span, end_span),
        })
    }

    fn parse_proc_decl(&mut self, name: String) -> Result<ProcDecl, ParseError> {
        let start_span = self.peek_span();
        self.consume(TokenType::LeftParen)?;

        let mut params = Vec::new();
        loop {
            // Right paren or "<param> <type>"
            if self.peek_type() == TokenType::RightParen {
                break;
            }
            let param_name = self.consume_identifier()?;
            let param_ty = self.parse_type()?;
            params.push((param_name, param_ty));

            // Right paren or comma
            if self.peek_type() == TokenType::RightParen {
                break;
            }

            self.consume(TokenType::Comma)?;
        }

        self.consume(TokenType::RightParen)?;

        let ret_ty = self.parse_type()?;
        let block = self.parse_block()?;
        let end_span = block.span;

        Ok(ProcDecl {
            name,
            params,
            ret_ty,
            block,
            span: Span::join(start_span, end_span),
        })
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.consume(TokenType::LeftParen)?;

        let mut args = vec![];
        loop {
            // Right paren or arg expression
            if self.peek_type() == TokenType::RightParen {
                break;
            }

            let arg_expr = self.parse_expr()?;
            args.push(arg_expr);

            // Prevents trailing comma
            if self.peek_type() == TokenType::RightParen {
                break;
            }

            self.consume(TokenType::Comma)?;
        }

        self.consume(TokenType::RightParen)?;
        Ok(args)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let ty = self.peek_type();
        match ty {
            TokenType::Ampersand => {
                self.advance();
                let inner_ty = self.parse_type()?;
                Ok(Type::Ref(Box::new(inner_ty)))
            }
            TokenType::Int => {
                self.advance();
                Ok(Type::Int)
            }
            TokenType::Float => {
                self.advance();
                Ok(Type::Float)
            }
            TokenType::String => {
                self.advance();
                Ok(Type::Str)
            }
            TokenType::Bool => {
                self.advance();
                Ok(Type::Bool)
            }
            TokenType::Unit => {
                self.advance();
                Ok(Type::Unit)
            }
            TokenType::Ident(name) => {
                self.advance();
                Ok(Type::Struct(name))
            }
            TokenType::LeftBracket => {
                self.advance();

                let elem_ty = self.parse_type()?;
                let mut array_length = ArrayLength::Dynamic;

                // Fixed size array
                if let TokenType::Comma = self.peek_type() {
                    self.advance();

                    if let TokenType::IntegerLiteral(length) = self.peek_type() {
                        if length < 0 {
                            return Err(ParseError {
                                kind: ParseErrorKind::NegativeArrayLength,
                                span: self.peek_span(),
                            });
                        }
                        array_length = ArrayLength::Fixed(length as usize);
                        self.advance();
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::NonIntLitArrayLength,
                            span: self.peek_span(),
                        });
                    }
                }

                self.consume(TokenType::RightBracket)?;
                Ok(Type::Array(Box::new(elem_ty), array_length))
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: TokenSet::Text("type".to_string()),
                    actual: self.peek_type(),
                },
                span: self.peek_span(),
            }),
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start_span = self.peek_span();
        self.consume(TokenType::LeftBrace)?;

        let mut statements = vec![];

        while self.peek_type() != TokenType::RightBrace {
            let span = self.peek_span();
            match self.peek_type() {
                // Statements
                TokenType::Let => {
                    self.consume(TokenType::Let)?;
                    let name = self.consume_identifier()?;
                    let ty = self.parse_type()?;
                    self.consume(TokenType::Equal)?;
                    let expr = self.parse_expr()?;
                    self.consume(TokenType::Semicolon)?;
                    statements.push(Stmt::VarDecl {
                        name,
                        ty,
                        expr,
                        span,
                    });
                }
                TokenType::If => {
                    self.consume(TokenType::If)?;
                    let cond = self.parse_condition()?;
                    let then_block = self.parse_block()?;

                    if self.peek_type() == TokenType::Else {
                        self.consume(TokenType::Else)?;
                        let else_block = self.parse_block()?;
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
                    self.consume(TokenType::While)?;
                    let cond = self.parse_condition()?;
                    let block = self.parse_block()?;
                    statements.push(Stmt::While { cond, block, span });
                }
                TokenType::For => {
                    self.consume(TokenType::For)?;
                    let name = self.consume_identifier()?;
                    self.consume(TokenType::In)?;
                    let range = self.parse_condition()?;
                    let block = self.parse_block()?;
                    statements.push(Stmt::For {
                        name,
                        range,
                        block,
                        span,
                    });
                }
                TokenType::Return => {
                    self.consume(TokenType::Return)?;
                    let expr = if self.peek_type() == TokenType::Semicolon {
                        Expr::Unit(span)
                    } else {
                        self.parse_expr()?
                    };

                    self.consume(TokenType::Semicolon)?;
                    statements.push(Stmt::Ret { expr, span });
                }
                // Try to parse expression for call or assignment
                _ => {
                    let expr = self.parse_expr()?;
                    match self.peek_type() {
                        TokenType::Semicolon => {
                            // Check that it's a procedure call
                            let stmt_span = self.peek_span();
                            self.consume(TokenType::Semicolon)?;
                            if let Expr::Call(call) = expr {
                                statements.push(Stmt::Call(call));
                            } else {
                                return Err(ParseError {
                                    kind: ParseErrorKind::UnexpectedToken {
                                        expected: TokenSet::Text("statement".to_string()),
                                        actual: self.peek_type(),
                                    },
                                    span: self.peek_span(),
                                });
                            }
                        }
                        TokenType::Equal => {
                            // Check that the expression is an lvalue
                            let span = self.peek_span();
                            self.consume(TokenType::Equal)?;
                            let lhs = expr;
                            if !lhs.is_lvalue() {
                                return Err(ParseError {
                                    kind: ParseErrorKind::InvalidLeftHandSide,
                                    span: lhs.span(),
                                });
                            }

                            // Parse the right hand side and check for semicolon
                            let rhs = self.parse_expr()?;
                            self.consume(TokenType::Semicolon)?;
                            statements.push(Stmt::Assign { lhs, rhs, span });
                        }
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken {
                                    expected: TokenSet::Text("statement".to_string()),
                                    actual: self.peek_type(),
                                },
                                span: self.peek_span(),
                            });
                        }
                    }
                }
            }
        }

        let end_span = self.peek_span();
        self.consume(TokenType::RightBrace)?;
        Ok(Block {
            statements,
            span: Span::new(
                start_span.start_line,
                start_span.start_column,
                end_span.end_line,
                end_span.end_column,
            ),
        })
    }

    fn parse_struct_literal(&mut self) -> Result<HashMap<String, Expr>, ParseError> {
        self.consume(TokenType::LeftBrace)?;
        let mut fields = HashMap::new();
        loop {
            // Right brace or "<field> = <expression>"
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            let field_name = self.consume_identifier()?;
            self.consume(TokenType::Equal)?;
            let field_expr = self.parse_expr()?;
            fields.insert(field_name, field_expr);

            // Right brace or comma
            if self.peek_type() == TokenType::RightBrace {
                break;
            }

            self.consume(TokenType::Comma)?;
        }

        self.consume(TokenType::RightBrace)?;
        Ok(fields)
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self._parse_expr(false)
    }

    fn parse_condition(&mut self) -> Result<Expr, ParseError> {
        self._parse_expr(true)
    }

    fn _parse_expr(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        self.parse_disjuncts(is_condition)
    }

    fn parse_disjuncts(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_conjuncts(is_condition)?;

        while TokenType::Or == self.peek_type() {
            self.advance();
            let rhs = self.parse_conjuncts(is_condition)?;

            let start_span = lhs.span();
            let end_span = rhs.span();

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: BinaryOp::Or,
                rhs: Box::new(rhs),
                span: Span::join(start_span, end_span),
            };
        }

        Ok(lhs)
    }

    fn parse_conjuncts(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_equality(is_condition)?;

        while TokenType::And == self.peek_type() {
            self.advance();
            let rhs = self.parse_equality(is_condition)?;

            let start_span = lhs.span();
            let end_span = rhs.span();

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op: BinaryOp::And,
                rhs: Box::new(rhs),
                span: Span::join(start_span, end_span),
            };
        }

        Ok(lhs)
    }

    fn parse_equality(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_comparison(is_condition)?;

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                EqualEqual => BinaryOp::Eq,
                NotEqual => BinaryOp::Neq,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_comparison(is_condition)?;

            let start_span = lhs.span();
            let end_span = rhs.span();

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::join(start_span, end_span),
            };
        }

        Ok(lhs)
    }

    fn parse_comparison(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_term(is_condition)?;

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

            let rhs = self.parse_term(is_condition)?;

            let start_span = lhs.span();
            let end_span = rhs.span();

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::join(start_span, end_span),
            };
        }

        Ok(lhs)
    }

    fn parse_term(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_factor(is_condition)?;

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                Plus => BinaryOp::Add,
                Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_factor(is_condition)?;

            let start_span = lhs.span();
            let end_span = rhs.span();

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::join(start_span, end_span),
            };
        }

        Ok(lhs)
    }

    fn parse_factor(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix(is_condition)?;

        use TokenType::*;
        loop {
            let op = match self.peek_type() {
                Star => BinaryOp::Mul,
                Slash => BinaryOp::Div,
                _ => break,
            };
            self.advance();

            let rhs = self.parse_prefix(is_condition)?;

            let start_span = lhs.span();
            let end_span = rhs.span();

            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span: Span::join(start_span, end_span),
            };
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        use TokenType::*;

        match self.peek_type() {
            Not => {
                self.advance();
                let rhs = self.parse_prefix(is_condition)?;

                return Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    rhs: Box::new(rhs),
                    span: Span::empty(),
                });
            }
            Ampersand => {
                self.advance();
                let rhs = self.parse_prefix(is_condition)?;
                if rhs.is_rvalue() {
                    return Err(ParseError {
                        kind: ParseErrorKind::InvalidReferenceTarget,
                        span: self.peek_span(),
                    });
                }
                return Ok(Expr::Ref(Box::new(rhs)));
            }
            Star => {
                self.advance();
                let rhs = self.parse_prefix(is_condition)?;
                return Ok(Expr::Deref(Box::new(rhs)));
            }
            _ => {}
        }

        self.parse_postfix(is_condition)
    }

    fn parse_postfix(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary(is_condition)?;

        loop {
            lhs = match self.peek_type() {
                TokenType::LeftBracket => {
                    self.advance();
                    let index_expr = self.parse_expr()?;
                    let end_span = self.peek_span();
                    self.consume(TokenType::RightBracket)?;
                    let start_span = lhs.span();
                    Expr::Index {
                        expr: Box::new(lhs),
                        index: Box::new(index_expr),
                        span: Span::join(start_span, end_span),
                    }
                }
                TokenType::Dot => {
                    self.advance();
                    let field = self.consume_identifier()?;
                    let end_span = self.peek_span();
                    let start_span = lhs.span();
                    Expr::Proj {
                        expr: Box::new(lhs),
                        field,
                        span: Span::join(start_span, end_span),
                    }
                }
                _ => break,
            };
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self, is_condition: bool) -> Result<Expr, ParseError> {
        match self.peek_type() {
            TokenType::IntegerLiteral(i) => {
                let span = self.peek_span();
                self.advance();
                Ok(Expr::Lit(Lit::Int(i, span)))
            }
            TokenType::FloatLiteral(f) => {
                let span = self.peek_span();
                self.advance();
                Ok(Expr::Lit(Lit::Float(f, span)))
            }
            TokenType::StringLiteral(s) => {
                let span = self.peek_span();
                self.advance();
                Ok(Expr::Lit(Lit::Str(s, span)))
            }
            TokenType::True => {
                let span = self.peek_span();
                self.advance();
                Ok(Expr::Lit(Lit::Bool(true, span)))
            }
            TokenType::False => {
                let span = self.peek_span();
                self.advance();
                Ok(Expr::Lit(Lit::Bool(false, span)))
            }
            TokenType::Ident(_) | TokenType::Hash => {
                // Identifier
                let start_span = self.peek_span();
                let mut name = String::new();

                if let TokenType::Hash = self.peek_type() {
                    self.advance();
                    name.push('#');
                }

                name.push_str(&self.consume_identifier()?);

                match self.peek_type() {
                    TokenType::LeftParen => {
                        if name == "#array" {
                            self.consume(TokenType::LeftParen)?;
                            let ty = self.parse_type()?;
                            self.consume(TokenType::Comma)?;
                            let expr = self.parse_expr()?;
                            self.consume(TokenType::RightParen)?;

                            return Ok(Expr::MakeArray {
                                ty,
                                expr: Box::new(expr),
                                span: Span::empty(),
                            });
                        }

                        let args = self.parse_arguments()?;
                        let end_span = self.peek_span();
                        Ok(Expr::Call(Call {
                            name,
                            args,
                            span: Span::new(
                                start_span.start_line,
                                start_span.start_column,
                                end_span.end_line,
                                end_span.end_column,
                            ),
                        }))
                    }
                    TokenType::LeftBrace => {
                        // To disambiguate between struct literal and blocks
                        if is_condition {
                            return Ok(Expr::Var {
                                name,
                                span: start_span,
                            });
                        }

                        let fields = self.parse_struct_literal()?;
                        let end_span = self.peek_span();
                        Ok(Expr::Lit(Lit::Struct(
                            fields,
                            Span::join(start_span, end_span),
                        )))
                    }
                    _ => Ok(Expr::Var {
                        name,
                        span: start_span,
                    }),
                }
            }
            TokenType::LeftParen => {
                self.advance();
                let inner_expr = self.parse_expr();
                self.consume(TokenType::RightParen)?;
                inner_expr
            }
            TokenType::LeftBracket => self.parse_array_literal(),
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: TokenSet::Text("primary expression".to_string()),
                    actual: self.peek_type(),
                },
                span: self.peek_span(),
            }),
        }
    }

    fn parse_array_literal(&mut self) -> Result<Expr, ParseError> {
        self.consume(TokenType::LeftBracket)?;
        let start_span = self.peek_span();
        let mut elem_exprs = vec![];
        while self.peek_type() != TokenType::RightBracket {
            let elem_expr = self.parse_expr()?;
            elem_exprs.push(elem_expr);

            if self.peek_type() == TokenType::RightBracket {
                break;
            }

            self.consume(TokenType::Comma)?;
        }

        let end_span = self.peek_span();
        self.consume(TokenType::RightBracket)?;
        Ok(Expr::Lit(Lit::Array(
            elem_exprs,
            Span::join(start_span, end_span),
        )))
    }

    fn peek(&self) -> Token {
        self.tokens
            .get(self.current)
            .cloned()
            .unwrap_or_else(|| unreachable!())
    }

    fn peek_type(&self) -> TokenType {
        self.peek().token_type
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.current)
            .unwrap_or_else(|| unreachable!())
            .span
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn consume(&mut self, expected: TokenType) -> Result<(), ParseError> {
        if self.peek_type() == expected {
            self.current += 1;
            Ok(())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: TokenSet::One(expected),
                    actual: self.peek_type(),
                },
                span: self.peek_span(),
            })
        }
    }

    fn consume_identifier(&mut self) -> Result<String, ParseError> {
        if let TokenType::Ident(name) = self.peek_type() {
            self.current += 1;
            Ok(name)
        } else {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken {
                    expected: TokenSet::Text("identifier".to_string()),
                    actual: self.peek_type(),
                },
                span: self.peek_span(),
            })
        }
    }
}
