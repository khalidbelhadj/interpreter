use core::default::Default;
use log::error;
use std::{
    fmt::{write, Display},
    process::exit,
};

use crate::token::*;

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

        self.source.chars().nth(self.curr).unwrap_or('\0')
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
            '.' => self.add_token(TokenType::Dot),
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
                                self.file_path, self.start_line, self.start_col
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
                    error!(
                        "{}:{}:{}: Use `not` for negation",
                        self.file_path, self.start_line, self.start_col
                    );
                    exit(1);
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
                    self.add_token(TokenType::Geq);
                } else {
                    self.add_token(TokenType::Gt);
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    self.add_token(TokenType::Leq);
                } else {
                    self.add_token(TokenType::Lt);
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
                        self.file_path, self.start_line, self.start_col
                    );
                    exit(1);
                }

                self.advance();
                self.add_token(TokenType::StringLiteral(string));
            }
            _ => {
                if c.is_ascii_digit() || c == '.' {
                    let mut has_dot = false;
                    let mut number = c.to_string();
                    loop {
                        let curr = self.peek();

                        if !(curr.is_ascii_digit() || curr == '.') {
                            break;
                        }

                        has_dot = curr == '.' || has_dot;
                        self.advance();
                        number.push(curr);
                    }

                    if has_dot {
                        let f: Option<f64> = number.parse().ok();
                        if f.is_none() {
                            error!(
                                "{}:{}:{}: Could not parse float {number}",
                                self.file_path, self.start_line, self.start_col
                            );
                            exit(1);
                        }
                        self.add_token(TokenType::FloatLiteral(f.unwrap()))
                    } else {
                        let n: Option<i64> = number.parse().ok();
                        if n.is_none() {
                            error!(
                                "{}:{}:{}: Could not parse int {number}",
                                self.file_path, self.start_line, self.start_col
                            );
                            exit(1);
                        }
                        self.add_token(TokenType::IntegerLiteral(n.unwrap()))
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
                        "float" => self.add_token(TokenType::Float),
                        "string" => self.add_token(TokenType::String),
                        "unit" => self.add_token(TokenType::Unit),
                        "bool" => self.add_token(TokenType::Bool),
                        "true" => self.add_token(TokenType::True),
                        "false" => self.add_token(TokenType::False),
                        "and" => self.add_token(TokenType::And),
                        "or" => self.add_token(TokenType::Or),
                        "not" => self.add_token(TokenType::Not),
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
