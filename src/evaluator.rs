use std::collections::HashMap;

use crate::parser::*;

#[derive(Debug, Clone)]
enum Value {
    Unit,
    Int(i64),
    Bool(bool),
    Str(String),
}

type Scope = HashMap<String, Value>;

pub struct Evaluator {
    program: Program,
    funcs: HashMap<String, (Vec<String>, Block)>,
    vars: Vec<Scope>,
}

impl Evaluator {
    pub fn new(program: Program) -> Evaluator {
        Evaluator {
            program,
            funcs: HashMap::new(),
            vars: vec![],
        }
    }

    pub fn eval(&mut self) {
        // add all functions to the funcs map
        for top_level in self.program.iter() {
            match top_level {
                TopLevel::FunDecl(FunDecl {
                    name, body, params, ..
                }) => {
                    let param_names: Vec<String> =
                        params.iter().map(|(name, _)| name.clone()).collect();
                    self.funcs.insert(name.clone(), (param_names, body.clone()));
                }
                _ => {}
            }
        }

        let main = self.funcs.get("main").expect("No main function").clone().1;
        self.eval_block(&main, vec![]);
    }

    fn get_var(&mut self, name: &str) -> &mut Value {
        for scope in self.vars.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return value;
            }
        }

        panic!("Variable not found: {:?}", name);
    }

    fn eval_block(&mut self, block: &Block, args: Vec<(String, Value)>) -> Value {
        let mut scope = HashMap::new();

        for (name, value) in args.iter() {
            scope.insert(name.clone(), value.clone());
        }

        self.vars.push(scope);

        for stmt in block.statements.iter() {
            match stmt {
                Stmt::VarDecl(name, _, expr) => {
                    let value = self.eval_expr(expr);
                    let scope = self.vars.last_mut().expect("Scope not defined");
                    scope.insert(name.clone(), value);
                }
                Stmt::Assign(name, expr) => {
                    let value = self.eval_expr(expr);
                    let v = self.get_var(name);
                    *v = value;
                }
                Stmt::If(expr, block) => {
                    let cond = self.eval_expr(expr);
                    match cond {
                        Value::Bool(b) => {
                            if b {
                                self.eval_block(block, vec![]);
                            }
                        }
                        _ => panic!("Expected bool"),
                    }
                }
                Stmt::IfElse(expr, if_block, else_block) => {
                    let cond = self.eval_expr(expr);
                    match cond {
                        Value::Bool(b) => {
                            if b {
                                self.eval_block(if_block, vec![]);
                            } else {
                                self.eval_block(else_block, vec![]);
                            }
                        }
                        _ => panic!("Expected bool"),
                    }
                }
                Stmt::Call(func, args) => {
                    if func == "print" {
                        for arg in args.iter() {
                            let value = self.eval_expr(arg);
                            println!("{:?}", value);
                        }
                    }
                }
                Stmt::Ret(expr) => {
                    return self.eval_expr(expr);
                }
            }
        }

        self.vars.pop();
        return Value::Unit;
    }

    fn eval_expr(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Unit => Value::Unit,
            Expr::Lit(Lit::Bool(b)) => Value::Bool(b.clone()),
            Expr::Lit(Lit::Int(i)) => Value::Int(*i),
            Expr::Lit(Lit::Str(s)) => Value::Str(s.clone()),
            Expr::Lit(Lit::Record(r)) => todo!(),
            Expr::Var(x) => self.get_var(x).clone(),
            Expr::Bin(e1, op, e2) => match op {
                BinOp::Add => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 + i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::Sub => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 - i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::Mul => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 * i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::Div => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Int(i1 / i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::And => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 && b2),
                        _ => panic!("Expected bool"),
                    }
                }
                BinOp::Or => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 || b2),
                        _ => panic!("Expected bool"),
                    }
                }
                BinOp::Eq => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(i1 == i2),
                        (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 == b2),
                        _ => panic!("Expected int or bool"),
                    }
                }
                BinOp::Neq => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(i1 != i2),
                        (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(b1 != b2),
                        _ => panic!("Expected int or bool"),
                    }
                }
                BinOp::Lt => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(i1 < i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::Leq => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(i1 <= i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::Gt => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(i1 > i2),
                        _ => panic!("Expected int"),
                    }
                }
                BinOp::Geq => {
                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(i1 >= i2),
                        _ => panic!("Expected int"),
                    }
                }
            },
            Expr::Un(op, e) => match op {
                UnOp::Not => {
                    let v = self.eval_expr(e);
                    match v {
                        Value::Bool(b) => Value::Bool(!b),
                        _ => panic!("Expected bool"),
                    }
                }
                UnOp::Negate => {
                    let v = self.eval_expr(e);
                    match v {
                        Value::Int(i) => Value::Int(-i),
                        _ => panic!("Expected int"),
                    }
                }
            },
            Expr::Call(func, args) => {
                let mut arg_values = vec![];
                for arg in args.iter() {
                    arg_values.push(self.eval_expr(arg));
                }

                let (params, block) = self.funcs.get(func).expect("Function not found").clone();
                return self.eval_block(
                    &block,
                    params
                        .iter()
                        .zip(arg_values)
                        .map(|(x, y)| (x.clone(), y))
                        .collect(),
                );
            }
        }
    }
}
