#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    // Keywords
    Struct,
    Let,
    If,
    Else,
    Return,
    Void,
    Int,
    String,
    Print,

    // Operators
    Not,          // "!"
    Plus,         // "+"
    Minus,        // "-"
    Star,         // "*"
    Slash,        // "/"
    Equal,        // "="
    Colon,        // ":"
    Dot,          // "."
    Comma,        // ","
    Greater,      // ">"
    Less,         // "<"
    GreaterEqual, // ">="
    LessEqual,    // "<="
    EqualEqual,   // "=="
    NotEqual,     // "!="

    // Delimiters
    LeftParen,  // "("
    RightParen, // ")"
    LeftBrace,  // "{"
    RightBrace, // "}"

    // Literals
    Identifier(String),    // Variable/function/struct names
    Integer(i64),          // Integer literals
    StringLiteral(String), // String literals

    // Special
    EOF, // End of file
}

struct Tokeniser {
    source: String,     // The source code to tokenise
    tokens: Vec<Token>, // The tokens that have been found
    current: usize,     // The current character being tokenised
    start: usize,       // The start of the current token
}

impl Tokeniser {
    fn new(source: String) -> Tokeniser {
        Tokeniser {
            source,
            tokens: Vec::new(),
            current: 0,
            start: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
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
            '+' => self.tokens.push(Token::Plus),
            '-' => self.tokens.push(Token::Minus),
            '*' => self.tokens.push(Token::Star),
            '/' => self.tokens.push(Token::Slash),
            ':' => self.tokens.push(Token::Colon),
            '.' => self.tokens.push(Token::Dot),
            ',' => self.tokens.push(Token::Comma),
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
            '(' => self.tokens.push(Token::LeftParen),
            ')' => self.tokens.push(Token::RightParen),
            '{' => self.tokens.push(Token::LeftBrace),
            '}' => self.tokens.push(Token::RightBrace),
            '"' => {
                let mut string = String::new();
                while self.peek() != '"' && !self.is_at_end() {
                    string.push(self.advance());
                }

                if self.is_at_end() {
                    panic!("Unterminated string");
                }

                self.advance();
                self.tokens.push(Token::StringLiteral(string));
            }
            _ => {
                // parse number
                if c.is_digit(10) {
                    let mut number = c.to_string();
                    while self.peek().is_digit(10) {
                        number.push(self.advance());
                    }

                    self.tokens.push(Token::Integer(
                        number.parse().expect("Failed to parse number"),
                    ));
                } else if (c.is_alphabetic()) {
                    // parse identifier
                    let mut identifier = c.to_string();
                    while self.peek().is_alphanumeric() {
                        identifier.push(self.advance());
                    }

                    match identifier.as_str() {
                        "struct" => self.tokens.push(Token::Struct),
                        "let" => self.tokens.push(Token::Let),
                        "if" => self.tokens.push(Token::If),
                        "else" => self.tokens.push(Token::Else),
                        "return" => self.tokens.push(Token::Return),
                        "void" => self.tokens.push(Token::Void),
                        "int" => self.tokens.push(Token::Int),
                        "string" => self.tokens.push(Token::String),
                        "print" => self.tokens.push(Token::Print),
                        _ => self.tokens.push(Token::Identifier(identifier)),
                    }
                }
            }
        }
    }

    fn tokenise(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::EOF);
    }
}

type Program = Vec<Statement>;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Statement {
    StructDecl(StructDeclaration),
    FunDecl(FunctionDeclaration),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct StructDeclaration {
    name: String,
    fields: Vec<Field>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Field {
    name: String,
    ty: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Type {
    Int,
    String,
    Struct(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct FunctionDeclaration {
    name: String,
    params: Vec<Parameter>,
    ret_ty: Type,
    body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Parameter {
    name: String,
    ty: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expression {
    Literal(Literal),
    Variable(String),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Call(String, Vec<Expression>),
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn parse(&mut self) {
        while !self.is_at_end() {
            let token = self.tokens[self.current].clone();
            match token {
                Token::Struct => {
                    self.parse_struct();
                }
                Token::Identifier(_) => {
                    // self.parse_function();
                }
                Token::EOF => {
                    break;
                }
                _ => {
                    panic!("Unexpected token: {:?}", token);
                }
            }
        }
    }

    fn parse_struct(&mut self) {}
}

fn main() {
    // read from example.txt
    let source = std::fs::read_to_string("example.txt").expect("Failed to read file");

    // create a new tokeniser
    let mut tokeniser = Tokeniser::new(source);
    tokeniser.tokenise();

    // print the tokens
    for token in tokeniser.tokens {
        println!("{:?}", token);
    }
}
