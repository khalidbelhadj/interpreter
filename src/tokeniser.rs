use core::default::Default;
use log::error;
use std::{
    fmt::{write, Display},
    process::exit,
};

#[derive(Debug, PartialEq, Eq, Clone)]
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
    String,
    Bool,
    Unit,

    And,
    Or,
    DotDot,
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
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
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
    Integer(i64),
    StringLiteral(String),
    EOF,
}

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
            TokenType::String => "string",
            TokenType::Bool => "bool",
            TokenType::Unit => "unit",
            TokenType::And => "and",
            TokenType::Or => "or",
            TokenType::DotDot => "..",
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
            TokenType::Greater => ">",
            TokenType::Less => "<",
            TokenType::GreaterEqual => ">=",
            TokenType::LessEqual => "<=",
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
            TokenType::Integer(_) => "int",
            TokenType::StringLiteral(_) => "string literal",
            TokenType::EOF => "EOF",
        };

        write!(f, "{string}")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
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

pub struct Tokeniser {
    // Raw file data
    pub file_path: String,
    source: String,

    // Starting at
    start: usize,
    start_line: usize,
    start_col: usize,

    // Current
    curr: usize,
    curr_line: usize,
    curr_col: usize,

    // Output tokens
    pub tokens: Vec<Token>,
}

impl Tokeniser {
    pub fn from_file(file_path: String) -> Tokeniser {
        let source = match std::fs::read_to_string(file_path.clone()) {
            Ok(s) => s,
            Err(e) => {
                error!("Failed to read file: {}", e);
                exit(1);
            }
        };

        Tokeniser {
            file_path,
            source,
            tokens: Vec::new(),
            curr: 0,
            start: 0,
            start_line: 1,
            start_col: 1,
            curr_line: 1,
            curr_col: 1,
        }
    }

    pub fn from_source(source: String) -> Tokeniser {
        Tokeniser {
            file_path: String::new(),
            source,
            tokens: Vec::new(),
            curr: 0,
            start: 0,
            start_line: 1,
            start_col: 1,
            curr_line: 1,
            curr_col: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.curr >= self.source.len()
    }

    fn advance(&mut self) -> char {
        self.curr_col += 1;
        self.curr += 1;
        self.source
            .chars()
            .nth(self.curr - 1)
            .expect("Failed to advance")
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source.chars().nth(self.curr).expect("Failed to peek")
    }

    fn span(&self) -> Span {
        Span::new(
            self.start_line,
            self.start_col,
            self.curr_line,
            self.curr_col,
        )
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(token_type, self.span()))
    }

    fn scan_token(&mut self) {
        // Save the starting position before processing the token
        self.start_line = self.curr_line;
        self.start_col = self.curr_col;

        let c = self.advance();

        match c {
            '\n' => {
                self.curr_line += 1;
                self.curr_col = 1;
            }
            ' ' | '\r' | '\t' => {}
            '+' => self.add_token(TokenType::Plus),
            '-' => self.add_token(TokenType::Minus),
            '&' => self.add_token(TokenType::Ampersand),
            '*' => self.add_token(TokenType::Star),
            ':' => self.add_token(TokenType::Colon),
            ';' => self.add_token(TokenType::Semicolon),
            ',' => self.add_token(TokenType::Comma),
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            '#' => self.add_token(TokenType::Hash),
            '?' => self.add_token(TokenType::QuestionMark),

            '/' => {
                match self.peek() {
                    '/' => {
                        // Single-line comment
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    }
                    '*' => {
                        // Multi-line comment
                        self.advance(); // consume the '*'

                        // Continue until we find a closing */
                        let mut nesting = 1;
                        while nesting > 0 && !self.is_at_end() {
                            if self.peek() == '*' {
                                self.advance();
                                if self.peek() == '/' {
                                    self.advance();
                                    nesting -= 1;
                                }
                            } else if self.peek() == '/' {
                                self.advance();
                                if self.peek() == '*' {
                                    self.advance();
                                    nesting += 1;
                                }
                            } else if self.peek() == '\n' {
                                self.advance();
                                self.curr_line += 1;
                                self.curr_col = 1;
                            } else {
                                self.advance();
                            }
                        }

                        if self.is_at_end() && nesting > 0 {
                            error!(
                                "{}:{}:{}: Unterminated multi-line comment",
                                self.file_path, self.curr_line, self.start
                            );
                            exit(1);
                        }
                    }
                    _ => self.add_token(TokenType::Slash),
                }
            }
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    self.add_token(TokenType::NotEqual);
                } else {
                    self.add_token(TokenType::Not);
                }
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    self.add_token(TokenType::EqualEqual);
                } else {
                    self.add_token(TokenType::Equal);
                }
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    self.add_token(TokenType::GreaterEqual);
                } else {
                    self.add_token(TokenType::Greater);
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    self.add_token(TokenType::LessEqual);
                } else {
                    self.add_token(TokenType::Less);
                }
            }
            '.' => {
                if self.peek() == '.' {
                    self.advance();
                    self.add_token(TokenType::DotDot);
                } else {
                    self.add_token(TokenType::Dot);
                }
            }
            '"' => {
                let mut string = String::new();
                while self.peek() != '"' && !self.is_at_end() {
                    string.push(self.advance());
                }

                if self.is_at_end() {
                    error!(
                        "{}:{}:{}: Unterminated string",
                        self.file_path, self.curr_line, self.start
                    );
                    exit(1);
                }

                self.advance();
                self.add_token(TokenType::StringLiteral(string));
            }
            _ => {
                if c.is_ascii_digit() {
                    // Parse number
                    // TODO: This needs to take into account different base literals and floats
                    let mut number = c.to_string();
                    while self.peek().is_ascii_digit() {
                        number.push(self.advance());
                    }

                    match number.parse() {
                        Ok(n) => self.add_token(TokenType::Integer(n)),
                        Err(_) => {
                            error!(
                                "{}:{}:{}: Unable to parse number",
                                self.file_path, self.curr_line, self.start
                            );
                            exit(1);
                        }
                    }
                } else if c.is_alphabetic() {
                    let mut identifier = c.to_string();
                    while self.peek().is_alphanumeric() || self.peek() == '_' {
                        identifier.push(self.advance());
                    }

                    match identifier.as_str() {
                        "struct" => self.add_token(TokenType::Struct),
                        "let" => self.add_token(TokenType::Let),
                        "if" => self.add_token(TokenType::If),
                        "else" => self.add_token(TokenType::Else),
                        "return" => self.add_token(TokenType::Return),
                        "int" => self.add_token(TokenType::Int),
                        "string" => self.add_token(TokenType::String),
                        "unit" => self.add_token(TokenType::Unit),
                        "bool" => self.add_token(TokenType::Bool),
                        "true" => self.add_token(TokenType::True),
                        "false" => self.add_token(TokenType::False),
                        "and" => self.add_token(TokenType::And),
                        "or" => self.add_token(TokenType::Or),
                        "while" => self.add_token(TokenType::While),
                        "for" => self.add_token(TokenType::For),
                        "in" => self.add_token(TokenType::In),
                        _ => self.add_token(TokenType::Ident(identifier)),
                    }
                } else {
                    error!(
                        "{}:{}:{}: Unexpected character: '{}'",
                        self.file_path, self.curr_line, self.curr_col, c
                    );
                    exit(1);
                }
            }
        }
    }

    pub fn tokenise(&mut self) {
        while !self.is_at_end() {
            self.start = self.curr;
            self.scan_token();
        }

        // TODO: Idk why I need an eof token
        // // For EOF token, we want the final position
        // self.tokens.push(Token::new(
        //     TokenType::EOF,
        //     Span::new(self.curr_line, self.curr_col, self.curr_line, self.curr_col),
        // ));
    }
}
