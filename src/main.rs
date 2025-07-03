#![allow(unused)]
pub mod eval;
pub mod evaluator;
pub mod parser;
pub mod tokeniser;
pub mod typer;

use crate::eval::Evaluator;
use crate::evaluator::*;
use crate::parser::*;
use crate::tokeniser::*;
use crate::typer::*;

use ansi_term::Color;
use env_logger::{Builder, Env};
use log::Level;
use std::io::Write;
use std::process::exit;

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

    // Read file path from command line arguments
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];

    // println!("---------- Tokens ----------");
    let mut tokeniser = Tokeniser::from_file(file_path.clone());
    tokeniser.tokenise();

    // for token in tokeniser.tokens.iter() {
    //     println!("{:?}", token)
    // }
    // println!();

    // println!("---------- Parsing ----------");
    let mut parser = Parser::new(tokeniser.file_path, tokeniser.tokens);
    parser.parse();

    // println!("{:#?}", parser.program);
    // println!();

    // println!("---------- Typechecking ----------");
    let mut typer = Typer::new(parser.program, file_path.clone());
    typer.type_check();
    for e in typer.errors.iter() {
        println!("{}", e);
    }

    if typer.errors.len() != 0 {
        exit(1);
    }

    // println!("Structs:");
    // for (name, fields) in typer.structs.iter() {
    //     println!("    - {:?} {:?}", name, fields);
    // }

    // println!("Functions:");
    // for (name, (params, ret_ty)) in typer.funcs.iter() {
    //     println!("    - {:?} {:?} {:?}", name, params, ret_ty);
    // }
    // println!();

    // println!("---------- Evaluation ----------");

    let mut evaluator = Evaluator::new();
    let result = evaluator.eval_program(&typer.program);
    match result {
        Ok(_) => {}
        Err(e) => {
            println!("{e}");
        }
    }
}
