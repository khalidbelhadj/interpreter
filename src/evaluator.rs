use std::collections::HashMap;

use crate::parser::*;

#[derive(Debug, Clone)]
enum Value {
    Unit,
    Int(i64),
    Bool(bool),
    Str(String),
    Record(HashMap<String, Value>),
    Array(Vec<Value>),
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Int(i) => i.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Str(s) => s.clone(),
            Value::Record(r) => {
                let mut s = "{".to_string();
                for (i, (name, value)) in r.iter().enumerate() {
                    s.push_str(&format!("{}: {}", name, value.to_string()));
                    if i < r.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str("}");
                s
            }
            Value::Array(a) => {
                let mut s = "[".to_string();
                for (i, value) in a.iter().enumerate() {
                    s.push_str(&value.to_string());
                    if i < a.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str("]");
                s
            }
        }
    }
}

type Scope = HashMap<String, Value>;

pub struct Evaluator {
    program: Program,
    funs: HashMap<String, (Vec<String>, Block)>,
    vars: Vec<Scope>,
}

impl Evaluator {
    pub fn new(program: Program) -> Evaluator {
        Evaluator {
            program,
            funs: HashMap::new(),
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
                    self.funs.insert(name.clone(), (param_names, body.clone()));
                }
                _ => {}
            }
        }

        let main = self.funs.get("main").expect("No main function").clone().1;
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
                Stmt::Assign(target, expr) => {
                    let value = self.eval_expr(expr);
                    match target {
                        AssignTarget::Var(name) => {
                            let var = self.get_var(name);
                            *var = value;
                        }
                        AssignTarget::Proj(rec, field) => {
                            let record = match self.get_var(rec) {
                                Value::Record(r) => r,
                                _ => panic!("Expected record"),
                            };

                            let value = record.get_mut(field).expect("Field not found");
                            *value = value.clone();
                        }
                        AssignTarget::Index(array, idx) => {
                            let idx = match self.eval_expr(idx) {
                                Value::Int(i) => i as usize,
                                _ => panic!("Expected int"),
                            };

                            let array = match self.get_var(array) {
                                Value::Array(a) => a,
                                _ => panic!("Expected array"),
                            };

                            array[idx] = value;
                        }
                    }
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
                Stmt::While(cond, block) => loop {
                    if let Value::Bool(b) = self.eval_expr(cond) {
                        if !b {
                            break;
                        }

                        self.eval_block(block, vec![]);
                    } else {
                        panic!("Expected bool");
                    }
                },
                Stmt::For(var, start, end, block) => {
                    let start = self.eval_expr(start);
                    let end = self.eval_expr(end);

                    match (start, end) {
                        (Value::Int(start), Value::Int(end)) => {
                            // Create a new scope for the loop variable
                            let mut scope = HashMap::new();
                            scope.insert(var.clone(), Value::Int(start));
                            self.vars.push(scope);

                            loop {
                                let value = self.get_var(var).clone();
                                if let Value::Int(value) = value {
                                    if value >= end {
                                        break;
                                    }

                                    self.eval_block(block, vec![]);
                                    let value = self.get_var(var).clone();
                                    if let Value::Int(value) = value {
                                        let value = value + 1;
                                        let scope =
                                            self.vars.last_mut().expect("Scope not defined");
                                        scope.insert(var.clone(), Value::Int(value));
                                    }
                                } else {
                                    panic!("Expected int");
                                }
                            }

                            self.vars.pop();
                        }
                        _ => panic!("Expected int"),
                    }
                }
                Stmt::Call(func, args) => {
                    if func == "print" {
                        for (i, arg) in args.iter().enumerate() {
                            let value = self.eval_expr(arg);
                            print!("{}", value.to_string());
                            if i < args.len() - 1 {
                                print!(" ");
                            }
                        }
                        println!();
                        continue;
                    }

                    let mut arg_values = vec![];
                    for arg in args.iter() {
                        arg_values.push(self.eval_expr(arg));
                    }

                    let (params, block) = self.funs.get(func).expect("Function not found").clone();
                    self.eval_block(
                        &block,
                        params
                            .iter()
                            .zip(arg_values)
                            .map(|(x, y)| (x.clone(), y))
                            .collect(),
                    );
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
            Expr::Lit(Lit::Record(r)) => {
                let mut record = HashMap::new();
                for (name, expr) in r.iter() {
                    record.insert(name.clone(), self.eval_expr(expr));
                }
                Value::Record(record)
            }
            Expr::Lit(Lit::Array(a)) => {
                let mut array = vec![];
                for expr in a.iter() {
                    array.push(self.eval_expr(expr));
                }
                Value::Array(array)
            }
            Expr::Var(x) => self.get_var(x).clone(),
            Expr::Bin(e1, op, e2) => {
                if op.is_arithmetic() {
                    let op_fun = |v1: i64, v2: i64| match op {
                        BinOp::Add => v1 + v2,
                        BinOp::Sub => v1 - v2,
                        BinOp::Mul => v1 * v2,
                        BinOp::Div => v1 / v2,
                        _ => panic!("Expected arithmetic operator"),
                    };

                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    return match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Int(op_fun(i1, i2)),
                        _ => panic!("Expected int"),
                    };
                }

                if op.is_logical() {
                    let op_fun = |v1: bool, v2: bool| match op {
                        BinOp::And => v1 && v2,
                        BinOp::Or => v1 || v2,
                        _ => panic!("Expected logical operator"),
                    };

                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    return match (v1, v2) {
                        (Value::Bool(b1), Value::Bool(b2)) => Value::Bool(op_fun(b1, b2)),
                        _ => panic!("Expected bool"),
                    };
                }

                if op.is_comparison() {
                    let op_fun = |v1: i64, v2: i64| match op {
                        BinOp::Eq => v1 == v2,
                        BinOp::Neq => v1 != v2,
                        BinOp::Lt => v1 < v2,
                        BinOp::Leq => v1 <= v2,
                        BinOp::Gt => v1 > v2,
                        BinOp::Geq => v1 >= v2,
                        _ => panic!("Expected comparison operator"),
                    };

                    let v1 = self.eval_expr(e1);
                    let v2 = self.eval_expr(e2);

                    return match (v1, v2) {
                        (Value::Int(i1), Value::Int(i2)) => Value::Bool(op_fun(i1, i2)),
                        _ => panic!("Expected int"),
                    };
                }

                panic!("Unknown operator");
            }
            Expr::Call(fun, args) => {
                let mut arg_values = vec![];
                for arg in args.iter() {
                    arg_values.push(self.eval_expr(arg));
                }

                let (params, block) = self.funs.get(fun).expect("Function not found").clone();
                return self.eval_block(
                    &block,
                    params
                        .iter()
                        .zip(arg_values)
                        .map(|(x, y)| (x.clone(), y))
                        .collect(),
                );
            }
            Expr::Proj(var, field) => match self.get_var(var) {
                Value::Record(r) => r.get(field).expect("Field not found").clone(),
                Value::Array(elems) => {
                    if field == "len" {
                        return Value::Int(elems.len() as i64);
                    }
                    panic!("Expected len")
                }
                _ => panic!("Expected record"),
            },

            Expr::Index(var, idx) => {
                let array = match self.get_var(var) {
                    Value::Array(a) => a.clone(),
                    _ => panic!("Expected array"),
                };

                let idx = match self.eval_expr(idx) {
                    Value::Int(i) => i as usize,
                    _ => panic!("Expected int"),
                };

                array.get(idx).expect("Index out of bounds").clone()
            }
        }
    }
}
