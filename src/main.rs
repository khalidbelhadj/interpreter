#![allow(unused)]
pub mod ast;
pub mod eval;
pub mod evaluator;
pub mod parser;
pub mod token;
pub mod tokeniser;
pub mod typer;

use crate::eval::Evaluator;
use crate::evaluator::*;
use crate::parser::*;
use crate::tokeniser::*;
use crate::typer::*;

use ansi_term::Color;
use clap::ArgAction;
use clap::Parser;
use env_logger::{Builder, Env};
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
            writeln!(
                buf,
                "[{}:{}][{}]: {}",
                file,
                line,
                colored_level,
                record.args()
            )
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
    tokeniser.tokenise();
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
    parser.parse();
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
        println!("{}", e);
    }

    if !typer.errors.is_empty() {
        println!(
            "Could not compile {} due to {} previous errors",
            typer.file_path,
            typer.errors.len()
        );
        exit(1);
    }

    let compile_end = SystemTime::now();
    let duration = compile_end
        .duration_since(compile_start)
        .unwrap()
        .as_secs_f64();

    if args.time {
        println!("Compiled in {}s", duration);
    }

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

    let mut evaluator = Evaluator::new();
    let result = evaluator.eval_program(&typer.program);
    match result {
        Ok(_) => {}
        Err(e) => {
            println!("{e}");
        }
    }
}
