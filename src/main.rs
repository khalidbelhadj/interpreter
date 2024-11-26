use std::{collections::HashMap, hash::Hash, os::macos::raw::stat};

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    // Keywords
    Struct,
    Let,
    If,
    Else,
    Return,
    Int,
    String,
    Bool,
    True,
    False,

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
    Semicolon,    // ;

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
            ';' => self.tokens.push(Token::Semicolon),
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
                        "int" => self.tokens.push(Token::Int),
                        "string" => self.tokens.push(Token::String),
                        "bool" => self.tokens.push(Token::Bool),
                        "true" => self.tokens.push(Token::True),
                        "false" => self.tokens.push(Token::False),
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

type Program = Vec<TopLevel>;

#[derive(Debug, PartialEq, Eq, Clone)]
enum TopLevel {
    StructDecl(StructDecl),
    FunDecl(FunDecl),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct StructDecl {
    name: String,
    fields: HashMap<String, Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Type {
    Unit,
    Int,
    String,
    Struct(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct FunDecl {
    name: String,
    params: HashMap<String, Type>,
    ret_ty: Type,
    body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Statement {
    VarDecl(String, Type, Expression),
    Assign(String, Expression),
    If(Expression, Vec<Statement>),
    IfElse(Expression, Vec<Statement>, Vec<Statement>),
    Call(String, Vec<Expression>),
    Return(Expression),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Expression {
    Literal(Literal),
    Variable(String),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Call(String, Vec<Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Literal {
    Integer(i64),
    String(String),
    Bool(bool),
    Struct(String, HashMap<String, Expression>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum UnaryOperator {
    Not,
    Negate,
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
    program: Program,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
            program: vec![],
        }
    }

    // Control
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek_next(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.current + n).map(|t| t.clone())
    }

    fn peek(&self) -> Token {
        self.peek_next(0).unwrap()
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn consume(&mut self, expected: Token) {
        if self.peek() == expected {
            self.current += 1;
        } else {
            panic!("Expected token {:?}, got {:?}", expected, self.peek());
        }
    }

    fn consume_identifier(&mut self) -> String {
        if let Token::Identifier(name) = self.peek() {
            self.current += 1;
            name.clone()
        } else {
            panic!("Expected identifier, got {:?}", self.peek());
        }
    }

    // Parsing
    fn parse(&mut self) {
        while !self.is_at_end() {
            let token = self.peek();
            match token {
                Token::Identifier(_) => {
                    let next = self.peek_next(1);
                    match next {
                        None => {
                            panic!("bruh")
                        }
                        Some(Token::LeftParen) => {
                            let func = self.parse_func();
                            self.program.push(TopLevel::FunDecl(func));
                        }
                        Some(Token::Struct) => {
                            let struct_decl = self.parse_struct();
                            self.program.push(TopLevel::StructDecl(struct_decl));
                        }
                        Some(_) => {}
                    }
                }
                Token::EOF => {
                    break;
                }
                _ => {
                    panic!(
                        "Expected top-level function of struct decleration, got {:?}",
                        token
                    );
                }
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        let ty = self.peek();
        match ty {
            Token::Int => {
                self.current += 1;
                Type::Int
            }
            Token::String => {
                self.current += 1;
                Type::String
            }
            Token::Identifier(name) => {
                self.current += 1;
                Type::Struct(name)
            }
            _ => panic!("Expected type, got {:?}", self.peek()),
        }
    }

    fn parse_struct(&mut self) -> StructDecl {
        let name = self.consume_identifier();
        self.consume(Token::Struct);
        self.consume(Token::LeftBrace);

        let mut fields = HashMap::new();
        loop {
            // Right brace or "<field> <type>"
            if self.peek() == Token::RightBrace {
                break;
            }

            let field_name = self.consume_identifier();
            let ty = self.parse_type();
            fields.insert(field_name, ty);

            // Right brace or comma
            if self.peek() == Token::RightBrace {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightBrace);
        StructDecl { name, fields }
    }

    fn parse_func(&mut self) -> FunDecl {
        let name = self.consume_identifier();
        self.consume(Token::LeftParen);

        let mut params = HashMap::new();
        loop {
            // Right paren or "<param> <type>"
            if self.peek() == Token::RightParen {
                break;
            }
            let param_name = self.consume_identifier();
            let ty = self.parse_type();
            params.insert(param_name, ty);

            // Right paren or comma
            if self.peek() == Token::RightParen {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightParen);

        let mut ret_ty = Type::Unit;

        if self.peek() != Token::LeftBrace {
            ret_ty = self.parse_type();
        }

        FunDecl {
            name,
            params,
            ret_ty,
            body: self.parse_block(),
        }
    }

    fn parse_arguments(&mut self) -> Vec<Expression> {
        self.consume(Token::LeftParen);

        let mut args = vec![];
        loop {
            // Right paren or "<param> <type>"
            if self.peek() == Token::RightParen {
                break;
            }

            let expr = self.parse_expression();
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

    fn parse_block(&mut self) -> Vec<Statement> {
        self.consume(Token::LeftBrace);

        let mut statements = vec![];
        while self.peek() != Token::RightBrace {
            let statement = self.peek();
            match statement {
                Token::Let => {
                    self.consume(Token::Let);
                    let name = self.consume_identifier();
                    let ty = self.parse_type();
                    self.consume(Token::Equal);
                    let expr = self.parse_expression();
                    self.consume(Token::Semicolon);
                    statements.push(Statement::VarDecl(name, ty, expr));
                }
                Token::Identifier(name) => {
                    self.advance();
                    match self.peek() {
                        Token::Equal => {
                            self.consume(Token::Equal);
                            let expr = self.parse_expression();
                            self.consume(Token::Semicolon);
                            statements.push(Statement::Assign(name, expr));
                        }
                        Token::LeftParen => {
                            let args = self.parse_arguments();
                            self.consume(Token::Semicolon);
                            statements.push(Statement::Call(name, args));
                        }
                        _ => {}
                    }
                }
                Token::If => {
                    self.consume(Token::If);
                    let cond = self.parse_expression();
                    let block1 = self.parse_block();

                    if self.peek() == Token::Else {
                        self.consume(Token::Else);
                        let block2 = self.parse_block();
                        statements.push(Statement::IfElse(cond, block1, block2));
                    } else {
                        statements.push(Statement::If(cond, block1));
                    }
                }
                Token::Return => {
                    self.consume(Token::Return);
                    let expr = self.parse_expression();

                    self.consume(Token::Semicolon);
                    statements.push(Statement::Return(expr));
                }
                _ => {
                    panic!("Expected statement or semi-colon, got {:?}", statement)
                }
            }
        }

        self.consume(Token::RightBrace);
        statements
    }

    fn parse_struct_args(&mut self) -> HashMap<String, Expression> {
        self.consume(Token::LeftBrace);
        let mut fields = HashMap::new();
        loop {
            // Right brace or "<field> = <expression>"
            if self.peek() == Token::RightBrace {
                break;
            }

            let field_name = self.consume_identifier();
            self.consume(Token::Equal);
            let expr = self.parse_expression();
            fields.insert(field_name, expr);

            // Right brace or comma
            if self.peek() == Token::RightBrace {
                break;
            }

            self.consume(Token::Comma);
        }

        self.consume(Token::RightBrace);
        fields
    }

    fn parse_expression(&mut self) -> Expression {
        match self.peek() {
            Token::Integer(n) => {
                self.advance();
                Expression::Literal(Literal::Integer(n))
            }
            Token::Identifier(name) => {
                self.advance();

                match self.peek() {
                    Token::LeftBrace => {
                        let fields = self.parse_struct_args();
                        return Expression::Literal(Literal::Struct(name, fields));
                    }
                    Token::LeftParen => {
                        todo!("Parse function call");
                    }
                    _ => Expression::Variable(name),
                }
            }
            Token::StringLiteral(s) => {
                self.advance();
                Expression::Literal(Literal::String(s))
            }
            Token::True => {
                self.advance();
                Expression::Literal(Literal::Bool(true))
            }
            Token::False => {
                self.advance();
                Expression::Literal(Literal::Bool(false))
            }
            _ => todo!(),
        }
    }
}

fn main() {
    // read from example.txt
    let source = std::fs::read_to_string("example.txt").expect("Failed to read file");

    // create a new tokeniser
    let mut tokeniser = Tokeniser::new(source);
    tokeniser.tokenise();

    println!("---------- Tokens ----------");
    for token in tokeniser.tokens.iter() {
        println!("{:?}", token)
    }
    println!();

    let mut parser = Parser::new(tokeniser.tokens);
    parser.parse();

    // print the ast
    println!("---------- AST ----------");
    println!("{:#?}", parser.program);
    println!();
}
