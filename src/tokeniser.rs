#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Keywords
    Record,
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

impl ToString for Token {
    fn to_string(&self) -> String {
        return match self {
            Token::Record => "\"record\"",
            Token::Let => "\"let\"",
            Token::If => "\"if\"",
            Token::Else => "\"else\"",
            Token::Return => "\"return\"",
            Token::While => "\"while\"",
            Token::For => "\"for\"",
            Token::In => "\"in\"",
            Token::True => "\"true\"",
            Token::False => "\"false\"",
            Token::Int => "\"int\"",
            Token::String => "\"string\"",
            Token::Bool => "\"bool\"",
            Token::Unit => "\"unit\"",
            Token::And => "\"and\"",
            Token::Or => "\"or\"",
            Token::DotDot => "\"..\"",
            Token::LeftBracket => "\"[\"",
            Token::RightBracket => "\"]\"",
            Token::Not => "\"!\"",
            Token::Plus => "\"+\"",
            Token::Minus => "\"-\"",
            Token::Star => "\"*\"",
            Token::Slash => "\"/\"",
            Token::Equal => "\"=\"",
            Token::Colon => "\":\"",
            Token::Dot => "\".\"",
            Token::Comma => "\",\"",
            Token::Greater => "\">\"",
            Token::Less => "\"<\"",
            Token::GreaterEqual => "\">=\"",
            Token::LessEqual => "\"<=\"",
            Token::EqualEqual => "\"==\"",
            Token::NotEqual => "\"!=\"",
            Token::Semicolon => "\";\"",
            Token::LeftParen => todo!(),
            Token::RightParen => todo!(),
            Token::LeftBrace => todo!(),
            Token::RightBrace => todo!(),
            Token::Ident(_) => todo!(),
            Token::Integer(_) => todo!(),
            Token::StringLiteral(_) => todo!(),
            Token::EOF => todo!(),
        }
        .to_string();
    }
}

pub struct Tokeniser {
    file_name: String,
    source: String,
    pub tokens: Vec<Token>,
    current: usize,
    start: usize,

    line: usize,
    column: usize,
}

impl Tokeniser {
    pub fn new(file_name: String) -> Tokeniser {
        let source = match std::fs::read_to_string(file_name.clone()) {
            Ok(s) => s,
            Err(e) => panic!("Failed to read file: {}", e),
        };
        Tokeniser {
            file_name,
            source,
            tokens: Vec::new(),
            current: 0,
            start: 0,
            line: 1,
            column: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        self.column += 1;
        self.current += 1;
        self.source
            .chars()
            .nth(self.current - 1)
            .expect("Failed to advance")
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        self.source
            .chars()
            .nth(self.current)
            .expect("Failed to peek")
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '\n' => {
                self.line += 1;
                self.column = 1;
            }
            ' ' | '\r' | '\t' => {}
            '+' => self.tokens.push(Token::Plus),
            '-' => self.tokens.push(Token::Minus),
            '*' => self.tokens.push(Token::Star),
            '/' => self.tokens.push(Token::Slash),
            ':' => self.tokens.push(Token::Colon),
            ';' => self.tokens.push(Token::Semicolon),
            ',' => self.tokens.push(Token::Comma),
            '(' => self.tokens.push(Token::LeftParen),
            ')' => self.tokens.push(Token::RightParen),
            '{' => self.tokens.push(Token::LeftBrace),
            '}' => self.tokens.push(Token::RightBrace),
            '[' => self.tokens.push(Token::LeftBracket),
            ']' => self.tokens.push(Token::RightBracket),
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    self.tokens.push(Token::NotEqual);
                } else {
                    self.tokens.push(Token::Not);
                }
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    self.tokens.push(Token::EqualEqual);
                } else {
                    self.tokens.push(Token::Equal);
                }
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    self.tokens.push(Token::GreaterEqual);
                } else {
                    self.tokens.push(Token::Greater);
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    self.tokens.push(Token::LessEqual);
                } else {
                    self.tokens.push(Token::Less);
                }
            }
            '.' => {
                if self.peek() == '.' {
                    self.advance();
                    self.tokens.push(Token::DotDot);
                } else {
                    self.tokens.push(Token::Dot);
                }
            }
            '"' => {
                let mut string = String::new();
                while self.peek() != '"' && !self.is_at_end() {
                    string.push(self.advance());
                }

                if self.is_at_end() {
                    panic!(
                        "{}:{}:{}: Unterminated string",
                        self.file_name, self.line, self.start
                    );
                }

                self.advance();
                self.tokens.push(Token::StringLiteral(string));
            }
            _ => {
                if c.is_digit(10) {
                    let mut number = c.to_string();
                    while self.peek().is_digit(10) {
                        number.push(self.advance());
                    }

                    match number.parse() {
                        Ok(n) => self.tokens.push(Token::Integer(n)),
                        Err(_) => panic!(
                            "{}:{}:{}: Unable to parse number",
                            self.file_name, self.line, self.start
                        ),
                    }
                } else if c.is_alphabetic() {
                    let mut identifier = c.to_string();
                    while self.peek().is_alphanumeric() || self.peek() == '_' {
                        identifier.push(self.advance());
                    }

                    match identifier.as_str() {
                        "record" => self.tokens.push(Token::Record),
                        "let" => self.tokens.push(Token::Let),
                        "if" => self.tokens.push(Token::If),
                        "else" => self.tokens.push(Token::Else),
                        "return" => self.tokens.push(Token::Return),
                        "int" => self.tokens.push(Token::Int),
                        "string" => self.tokens.push(Token::String),
                        "unit" => self.tokens.push(Token::Unit),
                        "bool" => self.tokens.push(Token::Bool),
                        "true" => self.tokens.push(Token::True),
                        "false" => self.tokens.push(Token::False),
                        "and" => self.tokens.push(Token::And),
                        "or" => self.tokens.push(Token::Or),
                        "while" => self.tokens.push(Token::While),
                        "for" => self.tokens.push(Token::For),
                        "in" => self.tokens.push(Token::In),
                        _ => self.tokens.push(Token::Ident(identifier)),
                    }
                } else {
                    panic!(
                        "{}:{}:{}: Unexpected character: '{}'",
                        self.file_name, self.line, self.column, c
                    );
                }
            }
        }
    }

    pub fn tokenise(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::EOF);
    }
}
