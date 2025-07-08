#![allow(unused)]
pub mod ast;
pub mod eval;
pub mod evaluator;
pub mod parser;
pub mod token;
pub mod tokeniser;
pub mod typer;

use crate::ast::ParseError;
use crate::ast::TypeErrorKind;
use crate::eval::Evaluator;
use crate::evaluator::*;
use crate::parser::*;
use crate::tokeniser::*;
use crate::typer::*;

use ansi_term::Color;
use clap::ArgAction;
use clap::Parser;
use env_logger::{Builder, Env};
use log::error;
use log::Level;
use std::io::Write;
use std::process::exit;
use std::time::SystemTime;

#[derive(clap::Parser, Debug)]
#[command(name = "interpreter")]
struct Args {
    /// Enable token parsing
    #[arg(long, action = ArgAction::Set, default_value_t = false)]
    tokens: bool,

    /// Enable AST parsing
    #[arg(long, action = ArgAction::Set, default_value_t = false)]
    ast: bool,

    /// Input file name
    #[arg(value_name = "FILE")]
    file_name: String,

    /// Enable symbol parsing
    #[arg(long, action = ArgAction::Set, default_value_t = false)]
    symbols: bool,

    /// Enable timing (default: true)
    #[arg(long, action = ArgAction::Set, default_value_t = true)]
    time: bool,
}

fn init_logger() {
    Builder::from_env(Env::default().default_filter_or("debug"))
        .format(|buf, record| {
            // Set the module name and level
            let module = record.module_path().unwrap_or("<unknown>");
            let level = record.level();

            // Colorize the log level
            let colored_level = match level {
                Level::Error => Color::Red.paint(level.to_string().to_lowercase()),
                Level::Warn => Color::Yellow.paint(level.to_string().to_lowercase()),
                Level::Debug => Color::Blue.paint(level.to_string().to_lowercase()),
                Level::Info => Color::Green.paint(level.to_string().to_lowercase()),
                Level::Trace => Color::White.paint(level.to_string().to_lowercase()),
            };

            // Extract file and line information
            let file = record.file().unwrap_or("<unknown>");
            let line = record.line().map_or(0, |l| l);

            // Format the log message with module, file:line, level, and message
            writeln!(buf, "[{}]{}", colored_level, record.args())
        })
        .init();
}

fn main() {
    init_logger();

    let args = Args::parse();
    let file_path = args.file_name;

    let compile_start = SystemTime::now();
    let start = SystemTime::now();
    let mut tokeniser = Tokeniser::from_file(file_path.clone());
    let tokenise_result = tokeniser.tokenise();
    if let Err(err) = tokenise_result {
        match err.kind {
            token::LexicalErrorKind::UnexpectedCharacter(c) => error!(
                "{}:{}:{}: Unexpected character {c}",
                file_path, err.span.start_line, err.span.start_column
            ),
            token::LexicalErrorKind::UnterminatedString => error!(
                "{}:{}:{}: Unterminated string",
                file_path, err.span.start_line, err.span.start_column
            ),
            token::LexicalErrorKind::UnterminatedMultilineComment => error!(
                "{}:{}:{}: Unterminated multi-line comment",
                file_path, err.span.start_line, err.span.start_column
            ),
            token::LexicalErrorKind::InvalidFloat => error!(
                "{}:{}:{}: Invalid float literal",
                file_path, err.span.start_line, err.span.start_column
            ),
            token::LexicalErrorKind::InvalidInt => error!(
                "{}:{}:{}: Invalid int literal",
                file_path, err.span.start_line, err.span.start_column
            ),
        }

        exit(1);
    }

    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();
    if args.time {
        println!("Tokenised in {}s", duration.as_secs_f64());
    }

    if args.tokens {
        for token in tokeniser.tokens.iter() {
            println!("{:?}", token)
        }
        println!();
    }

    let mut parser = parser::Parser::new(tokeniser.file_path, tokeniser.tokens);
    let start = SystemTime::now();
    let mut tokeniser = Tokeniser::from_file(file_path.clone());
    let parse_result = parser.parse();

    if parse_result.is_err() {
        let err = parse_result.unwrap_err();
        match err.kind {
            ast::ParseErrorKind::UnexpectedToken { expected, actual } => {
                error!(
                    "{}:{}:{}: Expected {}, got {}",
                    file_path, err.span.start_line, err.span.start_column, expected, actual
                )
            }
            ast::ParseErrorKind::NegativeArrayLength => error!(
                "{}:{}:{}: Array length must be non-negative",
                file_path, err.span.start_line, err.span.start_column
            ),
            ast::ParseErrorKind::NonIntLitArrayLength => error!(
                "{}:{}:{}: Array length must be an integer",
                file_path, err.span.start_line, err.span.start_column
            ),
            ast::ParseErrorKind::InvalidLeftHandSide => error!(
                "{}:{}:{}: Invalid left-hand side of assignment",
                file_path, err.span.start_line, err.span.start_column
            ),
            ast::ParseErrorKind::InvalidReferenceTarget => error!(
                "{}:{}:{}: Invalid reference target",
                file_path, err.span.start_line, err.span.start_column
            ),
        }
        exit(1);
    }

    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();

    if args.time {
        println!("Parserd in {}s", duration.as_secs_f64());
    }

    if args.ast {
        println!("{:#?}", parser.program);
        println!();
    }

    let mut typer = Typer::new(parser.program, file_path.clone());

    let mut parser = parser::Parser::new(tokeniser.file_path, tokeniser.tokens);
    let start = SystemTime::now();
    let mut tokeniser = Tokeniser::from_file(file_path.clone());
    typer.type_check();
    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();
    if args.time {
        println!("Type checked in {}s", duration.as_secs_f64());
    }

    for e in typer.errors.iter() {
        match &e.kind {
            TypeErrorKind::UnexpectedReturnType { expected, actual } => {
                error!(
                    "{}:{}:{}: Expected return type {}, got {}",
                    file_path, e.span.start_line, e.span.start_column, expected, actual
                )
            }
            TypeErrorKind::UnexpectedType { expected, actual } => error!(
                "{}:{}:{}: Expected type {}, got {}",
                file_path, e.span.start_line, e.span.start_column, expected, actual
            ),
            TypeErrorKind::UnexpectedArrayLength { expected, actual } => error!(
                "{}:{}:{}: Expected array of length {}, got {}",
                file_path, e.span.start_line, e.span.start_column, expected, actual
            ),
            TypeErrorKind::UnexpectedArrayType { expected, actual } => error!(
                "{}:{}:{}: Expected return type {}, got {}",
                file_path, e.span.start_line, e.span.start_column, expected, actual
            ),
            TypeErrorKind::ProcAlreadyDefined => error!(
                "{}:{}:{}: Procedure already defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::StructAlreadyDefined => error!(
                "{}:{}:{}: Struct already defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::VarAlreadyDefined => error!(
                "{}:{}:{}: Variable already defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::ProcNotDefined => error!(
                "{}:{}:{}: Procedure not defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::VarNotDefined => error!(
                "{}:{}:{}: Variable not defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::StructNotDefined => error!(
                "{}:{}:{}: Struct already defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::ProjectingNonStruct => error!(
                "{}:{}:{}: Projecting into non-struct expression",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::ProjectingNonStructRef => error!(
                "{}:{}:{}: Projecting into non-struct reference expression",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::DerefNonPointer => error!(
                "{}:{}:{}: Dereferencing non-reference type",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::IndexingNonArray => error!(
                "{}:{}:{}: Indexing into non-index type",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::ArithWithNonNumber { ty } => error!(
                "{}:{}:{}: Arithmaeic operation on non-number type {}",
                file_path, e.span.start_line, e.span.start_column, ty
            ),
            TypeErrorKind::ScopeNotDefined => error!(
                "{}:{}:{}: Scope not defined",
                file_path, e.span.start_line, e.span.start_column
            ),
            TypeErrorKind::StructFielNotFound { field, struct_name } => error!(
                "{}:{}:{}: Field {} not found in struct {}",
                file_path, e.span.start_line, e.span.start_column, field, struct_name
            ),
            TypeErrorKind::WrongArgCount {
                expected,
                actual,
                name,
            } => error!(
                "{}:{}:{}: {actual} arguments provided where {expected} required",
                file_path, e.span.start_line, e.span.start_column,
            ),
            TypeErrorKind::NotAbleToInferType => error!(
                "{}:{}:{}: Not able to infer type of expression",
                file_path, e.span.start_line, e.span.start_column,
            ),
            TypeErrorKind::UnreachableCodeAfterReturn => error!(
                "{}:{}:{}: Unreachable code after returning from procedure",
                file_path, e.span.start_line, e.span.start_column,
            ),
        }
    }

    let compile_end = SystemTime::now();
    let duration = compile_end
        .duration_since(compile_start)
        .unwrap()
        .as_secs_f64();

    if args.symbols {
        println!("Structs");
        for (name, fields) in typer.structs.iter() {
            println!("    - {:?} {:?}", name, fields);
        }

        println!("Procs");
        for (name, (params, ret_ty)) in typer.procs.iter() {
            println!("    - {:?} {:?} {:?}", name, params, ret_ty);
        }
        println!();
    }

    if !typer.errors.is_empty() {
        println!(
            "Could not compile {} due to {} previous errors",
            typer.file_path,
            typer.errors.len()
        );
        exit(1);
    }

    if args.time {
        println!("Compiled in {}s", duration);
    }

    println!("=========================\n");
    let mut evaluator = Evaluator::new();
    let result = evaluator.eval_program(&typer.program);
    match result {
        Ok(_) => {}
        Err(e) => {
            println!("{e}");
        }
    }
}
