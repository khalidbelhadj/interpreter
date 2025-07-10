use core::default::Default;
use log::error;
use std::{
    fmt::{write, Debug, Display},
    process::exit,
};

#[derive(Debug, Clone)]
pub enum TokenType {
    // Keywords
    Struct,
    Let,
    If,
    Else,
    Return,
    While,
    For,
    In,
    True,
    False,

    // Types
    Int,
    Float,
    String,
    Bool,
    Unit,

    And,
    Or,
    LeftBracket,
    RightBracket,
    Not,
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Colon,
    Dot,
    Comma,
    Gt,
    Lt,
    Geq,
    Leq,
    EqualEqual,
    NotEqual,
    Semicolon,
    Hash,
    QuestionMark,
    Ampersand,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Literals
    Ident(String),
    IntegerLiteral(i64),
    StringLiteral(String),
    FloatLiteral(f64),
    EOF,
}

impl PartialEq for TokenType {
    fn eq(&self, other: &TokenType) -> bool {
        match (self, other) {
            // Handle variants with data explicitly
            (TokenType::Ident(s1), TokenType::Ident(s2)) => s1 == s2,
            (TokenType::IntegerLiteral(i1), TokenType::IntegerLiteral(i2)) => i1 == i2,
            (TokenType::StringLiteral(s1), TokenType::StringLiteral(s2)) => s1 == s2,
            (TokenType::FloatLiteral(f1), TokenType::FloatLiteral(f2)) => f1 == f2,

            // Handle all unit variants (variants without data)
            (TokenType::Struct, TokenType::Struct) => true,
            (TokenType::Let, TokenType::Let) => true,
            (TokenType::If, TokenType::If) => true,
            (TokenType::Else, TokenType::Else) => true,
            (TokenType::Return, TokenType::Return) => true,
            (TokenType::While, TokenType::While) => true,
            (TokenType::For, TokenType::For) => true,
            (TokenType::In, TokenType::In) => true,
            (TokenType::True, TokenType::True) => true,
            (TokenType::False, TokenType::False) => true,
            (TokenType::Int, TokenType::Int) => true,
            (TokenType::Float, TokenType::Float) => true,
            (TokenType::String, TokenType::String) => true,
            (TokenType::Bool, TokenType::Bool) => true,
            (TokenType::Unit, TokenType::Unit) => true,
            (TokenType::And, TokenType::And) => true,
            (TokenType::Or, TokenType::Or) => true,
            (TokenType::LeftBracket, TokenType::LeftBracket) => true,
            (TokenType::RightBracket, TokenType::RightBracket) => true,
            (TokenType::Not, TokenType::Not) => true,
            (TokenType::Plus, TokenType::Plus) => true,
            (TokenType::Minus, TokenType::Minus) => true,
            (TokenType::Star, TokenType::Star) => true,
            (TokenType::Slash, TokenType::Slash) => true,
            (TokenType::Equal, TokenType::Equal) => true,
            (TokenType::Colon, TokenType::Colon) => true,
            (TokenType::Dot, TokenType::Dot) => true,
            (TokenType::Comma, TokenType::Comma) => true,
            (TokenType::Gt, TokenType::Gt) => true,
            (TokenType::Lt, TokenType::Lt) => true,
            (TokenType::Geq, TokenType::Geq) => true,
            (TokenType::Leq, TokenType::Leq) => true,
            (TokenType::EqualEqual, TokenType::EqualEqual) => true,
            (TokenType::NotEqual, TokenType::NotEqual) => true,
            (TokenType::Semicolon, TokenType::Semicolon) => true,
            (TokenType::Hash, TokenType::Hash) => true,
            (TokenType::QuestionMark, TokenType::QuestionMark) => true,
            (TokenType::Ampersand, TokenType::Ampersand) => true,
            (TokenType::LeftParen, TokenType::LeftParen) => true,
            (TokenType::RightParen, TokenType::RightParen) => true,
            (TokenType::LeftBrace, TokenType::LeftBrace) => true,
            (TokenType::RightBrace, TokenType::RightBrace) => true,
            (TokenType::EOF, TokenType::EOF) => true,

            // All other combinations are not equal
            _ => false,
        }
    }
}
impl Eq for TokenType {}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::Struct => "struct",
            TokenType::Let => "let",
            TokenType::If => "if",
            TokenType::Else => "else",
            TokenType::Return => "return",
            TokenType::While => "while",
            TokenType::For => "for",
            TokenType::In => "in",
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::Int => "int",
            TokenType::Float => "float",
            TokenType::String => "string",
            TokenType::Bool => "bool",
            TokenType::Unit => "unit",
            TokenType::And => "and",
            TokenType::Or => "or",
            TokenType::LeftBracket => "[",
            TokenType::RightBracket => "]",
            TokenType::Not => "!",
            TokenType::Ampersand => "&",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Star => "*",
            TokenType::Slash => "/",
            TokenType::Equal => "=",
            TokenType::Colon => ":",
            TokenType::Dot => ".",
            TokenType::Comma => ",",
            TokenType::Gt => ">",
            TokenType::Lt => "<",
            TokenType::Geq => ">=",
            TokenType::Leq => "<=",
            TokenType::EqualEqual => "==",
            TokenType::NotEqual => "!=",
            TokenType::Semicolon => ";",
            TokenType::Hash => "#",
            TokenType::QuestionMark => "?",
            TokenType::LeftParen => "",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Ident(_) => "identifier",
            TokenType::IntegerLiteral(_) => "int",
            TokenType::StringLiteral(_) => "string literal",
            TokenType::FloatLiteral(_) => "float",
            TokenType::EOF => "EOF",
        };

        write!(f, "{string}")
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Span {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl Copy for Span {}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "X")
    }
}

impl Span {
    pub fn new(start_line: usize, start_column: usize, end_line: usize, end_column: usize) -> Span {
        Span {
            start_line,
            start_column,
            end_line,
            end_column,
        }
    }

    pub fn empty() -> Span {
        Span {
            start_line: 0,
            start_column: 0,
            end_line: 0,
            end_column: 0,
        }
    }

    pub fn join(start_span: Span, end_span: Span) -> Span {
        Span::new(
            start_span.start_line,
            start_span.start_column,
            end_span.end_line,
            end_span.end_column,
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span) -> Token {
        Token { token_type, span }
    }
}

#[derive(Debug, Clone)]
pub enum LexicalErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    UnterminatedMultilineComment,
    InvalidFloat,
    InvalidInt,
}

#[derive(Debug, Clone)]
pub struct LexicalError {
    pub span: Span,
    pub kind: LexicalErrorKind,
}
