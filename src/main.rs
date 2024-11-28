pub mod evaluator;
pub mod parser;
pub mod tokeniser;
pub mod typer;

use crate::evaluator::*;
use crate::parser::*;
use crate::tokeniser::*;
use crate::typer::*;

fn main() {
    println!("---------- Tokens ----------");
    let mut tokeniser = Tokeniser::new("example.txt".into());
    tokeniser.tokenise();

    for token in tokeniser.tokens.iter() {
        println!("{:?}", token)
    }
    println!();

    println!("---------- Parsing ----------");
    let mut parser = Parser::new(tokeniser.tokens);
    parser.parse();

    println!("{:#?}", parser.program);
    println!();

    println!("---------- Typechecking ----------");
    let mut typer = Typer::new(parser.program);
    typer.tycheck();

    println!("Structs:");
    for (name, fields) in typer.records.iter() {
        println!("    - {:?} {:?}", name, fields);
    }

    println!("Functions:");
    for (name, (params, ret_ty)) in typer.funcs.iter() {
        println!("    - {:?} {:?} {:?}", name, params, ret_ty);
    }
    println!();

    println!("---------- Evaluation ----------");

    let mut evaluator = Evaluator::new(typer.program);
    evaluator.eval();
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
