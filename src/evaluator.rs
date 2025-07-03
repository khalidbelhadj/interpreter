// use core::error;
// use std::fmt::Display;
// use std::ops::Add;
// use std::process::exit;
// use std::sync::Arc;
// use std::thread::panicking;
// use std::{collections::HashMap, vec};

// use crate::parser::*;
// use crate::tokeniser::Span;

// use log::{debug, error, info};

// #[derive(Debug, Clone)]
// enum Value {
//     Unit,
//     Int(i64),
//     Bool(bool),
//     String(String),
//     Struct(HashMap<String, Value>),
//     Array(Vec<Value>),
//     Ref(String),
// }

// #[derive(Debug, Clone)]
// enum Addressing {
//     Var { name: String },
//     Proj { lhs: Box<Addressing>, field: String },
// }

// impl Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let string = match self {
//             Value::Unit => "unit".to_string(),
//             Value::Int(i) => i.to_string(),
//             Value::Bool(b) => b.to_string(),
//             Value::String(s) => s.clone(),
//             Value::Struct(r) => {
//                 let mut s = "{".to_string();
//                 for (i, (name, value)) in r.iter().enumerate() {
//                     s.push_str(&format!("{}: {}", name, value));
//                     if i < r.len() - 1 {
//                         s.push_str(", ");
//                     }
//                 }
//                 s.push('}');
//                 s
//             }
//             Value::Array(a) => {
//                 let mut s = "[".to_string();
//                 for (i, value) in a.iter().enumerate() {
//                     s.push_str(&value.to_string());
//                     if i < a.len() - 1 {
//                         s.push_str(", ");
//                     }
//                 }
//                 s.push(']');
//                 s
//             }
//             Value::Ref(_) => todo!(),
//         };

//         write!(f, "{string}")
//     }
// }

// type Scope = HashMap<String, Value>;

// struct ProcScope {
//     scopes: Vec<Scope>,
// }

// impl ProcScope {
//     fn new() -> Self {
//         ProcScope { scopes: vec![] }
//     }
// }

// pub struct Evaluator {
//     program: Program,
//     funs: HashMap<String, (Vec<String>, Block)>,
//     stack: Vec<Value>,
//     vars: Vec<ProcScope>,
//     file_path: String,
// }

// impl Evaluator {
//     pub fn new(program: Program, file_path: String) -> Evaluator {
//         Evaluator {
//             program,
//             funs: HashMap::new(),
//             stack: vec![],
//             vars: vec![],
//             file_path,
//         }
//     }

//     pub fn eval(&mut self) {
//         // Add all functions to the funcs map
//         // TODO: Do we need to add struct decls?
//         for top_level in self.program.iter() {
//             if let TopLevel::ProcDecl(ProcDecl {
//                 name,
//                 block,
//                 params,
//                 span,
//                 ret_ty,
//             }) = top_level
//             {
//                 let param_names: Vec<String> =
//                     params.iter().map(|(name, _)| name.clone()).collect();
//                 self.funs.insert(name.clone(), (param_names, block.clone()));
//             }
//         }

//         let main = self.funs.get("main");
//         if main.is_none() {
//             error!("No main function");
//             exit(1)
//         }

//         let main_fn = main.unwrap().clone().1;
//         self.eval_proc(&main_fn, vec![]);
//     }

//     fn get_var(&mut self, name: &str) -> &mut Value {
//         let frame = self.vars.last_mut();
//         if frame.is_none() {
//             debug!("{}:{}:{} No frame defined", self.file_path, 0, 0);
//             panic!()
//         }

//         let frame_value = frame.unwrap();

//         for scope in frame_value.scopes.iter_mut().rev() {
//             if let Some(value) = scope.get_mut(name) {
//                 return value;
//             }
//         }

//         error!(
//             "{}:{}:{} Variable not found: {}",
//             self.file_path, 0, 0, name
//         );
//         panic!()
//     }

//     fn eval_lvalue(&self, expr: &Expr) -> Addressing {
//         match expr {
//             Expr::Var { name, span } => Addressing::Var {
//                 name: name.to_string(),
//             },
//             _ => todo!(),
//         }
//     }

//     fn eval_proc(&mut self, block: &Block, args: Vec<(String, Value)>) -> Value {
//         let proc_scope = ProcScope::new();

//         self.vars.push(proc_scope);
//         let (value, returns) = self.eval_block(block, args);
//         self.vars.pop();

//         value
//     }

//     fn push_scope(&mut self) {
//         let proc_scope = self.vars.last_mut();

//         if proc_scope.is_none() {
//             debug!("{}:{}:{} No frame defined", self.file_path, -1, 0);
//             panic!()
//         }

//         let proc_scope = proc_scope.unwrap();
//         let scope: HashMap<String, Value> = HashMap::new();

//         proc_scope.scopes.push(scope);
//     }

//     fn pop_scope(&mut self) {
//         let proc_scope = self.vars.last_mut();

//         if proc_scope.is_none() {
//             debug!("{}:{}:{} No frame defined", self.file_path, 0, 0);
//             panic!()
//         }

//         let proc_scope = proc_scope.unwrap();
//         proc_scope.scopes.pop();
//     }

//     fn add_to_scope(&mut self, name: String, value: Value) {
//         let proc_scope = self.vars.last_mut();
//         if proc_scope.is_none() {
//             debug!("{}:{}:{} No frame defined", self.file_path, 0, 0);
//             panic!()
//         }

//         let proc_scope = proc_scope.unwrap();

//         let scope = proc_scope.scopes.last_mut();
//         if scope.is_none() {
//             debug!("{}:{}:{} No scope defined", self.file_path, 0, 0);
//             panic!()
//         }

//         let scope_value = scope.unwrap();
//         scope_value.insert(name, value);
//     }

//     fn create_var(&mut self, value: Value) -> String {

//     }

//     fn eval_block(&mut self, block: &Block, args: Vec<(String, Value)>) -> (Value, bool) {
//         self.push_scope();

//         for (name, value) in args.iter() {
//             self.add_to_scope(name.clone(), value.clone());
//         }

//         let mut return_value = Value::Unit;
//         let mut block_returns = false;

//         for stmt in block.statements.iter() {
//             match stmt {
//                 Stmt::VarDecl {
//                     name,
//                     ty,
//                     expr,
//                     span,
//                 } => {
//                     let value = self.eval_expr(expr);
//                     self.add_to_scope(name.to_string(), value);
//                 }
//                 Stmt::Assign { lhs, rhs, span } => {
//                     let value = self.eval_expr(rhs);
//                     let lvalue = self.eval_lvalue(lhs);
//                     self.assign(lvalue, value)
//                 }
//                 Stmt::If {
//                     cond,
//                     then_block,
//                     span,
//                 } => {
//                     let cond = self.eval_expr(cond);
//                     match cond {
//                         Value::Bool(b) => {
//                             if b {
//                                 let (value, returns) = self.eval_block(block, vec![]);
//                                 if returns {
//                                     (return_value, block_returns) = (value, returns);
//                                     break;
//                                 }
//                             }
//                         }
//                         _ => panic!(
//                             "{}:{}:{} Expected bool",
//                             self.file_path, span.start_line, span.start_column
//                         ),
//                     }
//                 }
//                 Stmt::IfElse {
//                     cond,
//                     then_block,
//                     else_block,
//                     span,
//                 } => {
//                     let cond = self.eval_expr(cond);
//                     match cond {
//                         Value::Bool(b) => {
//                             if b {
//                                 let (value, returns) = self.eval_block(then_block, vec![]);
//                                 if returns {
//                                     (return_value, block_returns) = (value, returns);
//                                     break;
//                                 }
//                             } else {
//                                 let (value, returns) = self.eval_block(else_block, vec![]);
//                                 if returns {
//                                     (return_value, block_returns) = (value, returns);
//                                     break;
//                                 }
//                             }
//                         }
//                         _ => panic!(
//                             "{}:{}:{} Expected bool",
//                             self.file_path, span.start_line, span.start_column
//                         ),
//                     }
//                 }
//                 Stmt::While { cond, block, span } => loop {
//                     if let Value::Bool(b) = self.eval_expr(cond) {
//                         if !b {
//                             break;
//                         }

//                         self.eval_block(block, vec![]);
//                     } else {
//                         panic!(
//                             "{}:{}:{} Expected bool",
//                             self.file_path, span.start_line, span.start_column
//                         );
//                     }
//                 },
//                 Stmt::For {
//                     name,
//                     from,
//                     to,
//                     block,
//                     span,
//                 } => {
//                     let from = self.eval_expr(from);
//                     let to = self.eval_expr(to);

//                     match (from, to) {
//                         (Value::Int(start), Value::Int(end)) => {
//                             // Create a new scope for the loop variable
//                             self.push_scope();
//                             self.add_to_scope(name.clone(), Value::Int(start));

//                             loop {
//                                 let value = self.get_var(name).clone();
//                                 if let Value::Int(value) = value {
//                                     if value >= end {
//                                         break;
//                                     }

//                                     self.eval_block(block, vec![]);
//                                     let value = self.get_var(name).clone();
//                                     if let Value::Int(value) = value {
//                                         let value = value + 1;
//                                         self.add_to_scope(name.clone(), Value::Int(value));
//                                     }
//                                 } else {
//                                     panic!(
//                                         "{}:{}:{} Expected int",
//                                         self.file_path, span.start_line, span.start_column
//                                     );
//                                 }
//                             }

//                             self.pop_scope();
//                         }
//                         _ => panic!(
//                             "{}:{}:{} Expected int",
//                             self.file_path, span.start_line, span.start_column
//                         ),
//                     }
//                 }
//                 Stmt::Call { name, args, span } => {
//                     if name == "#print" {
//                         for (i, arg) in args.iter().enumerate() {
//                             let value = self.eval_expr(arg);
//                             print!("{}", value);
//                             if i < args.len() - 1 {
//                                 print!(" ");
//                             }
//                         }
//                         println!();
//                         continue;
//                     }

//                     let mut arg_values = vec![];
//                     for arg in args.iter() {
//                         arg_values.push(self.eval_expr(arg));
//                     }

//                     let (params, block) = self.funs.get(name).expect("Function not found").clone();
//                     self.eval_proc(
//                         &block,
//                         params
//                             .iter()
//                             .zip(arg_values)
//                             .map(|(x, y)| (x.clone(), y))
//                             .collect(),
//                     );
//                 }
//                 Stmt::Ret { expr, span } => {
//                     (return_value, block_returns) = (self.eval_expr(expr), true);
//                     break;
//                 }
//             }
//         }

//         self.pop_scope();
//         (return_value, block_returns)
//     }

//     fn eval_expr(&mut self, expr: &Expr) -> Value {
//         match expr {
//             Expr::Unit(span) => Value::Unit,
//             Expr::Lit(Lit::Bool(b, _)) => Value::Bool(*b),
//             Expr::Lit(Lit::Int(i, _)) => Value::Int(*i),
//             Expr::Lit(Lit::Str(s, _)) => Value::String(s.clone()),
//             Expr::Lit(Lit::Struct(r, _)) => {
//                 let mut struct_val = HashMap::new();
//                 for (name, expr) in r.iter() {
//                     struct_val.insert(name.clone(), self.eval_expr(expr));
//                 }
//                 Value::Struct(struct_val)
//             }
//             Expr::Lit(Lit::Array(a, _)) => {
//                 let mut array = vec![];
//                 for expr in a.iter() {
//                     array.push(self.eval_expr(expr));
//                 }
//                 Value::Array(array)
//             }
//             Expr::Bin { lhs, op, rhs, span } => {
//                 let v1 = self.eval_expr(lhs);
//                 let v2 = self.eval_expr(rhs);

//                 if op.is_arithmetic() {
//                     match (v1, v2) {
//                         (Value::Int(i1), Value::Int(i2)) => {
//                             let result = match op {
//                                 BinOp::Add => i1 + i2,
//                                 BinOp::Sub => i1 - i2,
//                                 BinOp::Mul => i1 * i2,
//                                 BinOp::Div => i1 / i2,
//                                 _ => panic!(
//                                     "{}:{}:{} Expected arithmetic operator",
//                                     self.file_path, span.start_line, span.start_column
//                                 ),
//                             };
//                             Value::Int(result)
//                         }
//                         _ => panic!(
//                             "{}:{}:{} Expected int",
//                             self.file_path, span.start_line, span.start_column
//                         ),
//                     }
//                 } else if op.is_logical() {
//                     match (v1, v2) {
//                         (Value::Bool(b1), Value::Bool(b2)) => {
//                             let result = match op {
//                                 BinOp::And => b1 && b2,
//                                 BinOp::Or => b1 || b2,
//                                 _ => panic!(
//                                     "{}:{}:{} Expected logical operator",
//                                     self.file_path, span.start_line, span.start_column
//                                 ),
//                             };
//                             Value::Bool(result)
//                         }
//                         _ => panic!(
//                             "{}:{}:{} Expected bool",
//                             self.file_path, span.start_line, span.start_column
//                         ),
//                     }
//                 } else if op.is_comparison() {
//                     match (v1, v2) {
//                         (Value::Int(i1), Value::Int(i2)) => {
//                             let result = match op {
//                                 BinOp::Eq => i1 == i2,
//                                 BinOp::Neq => i1 != i2,
//                                 BinOp::Lt => i1 < i2,
//                                 BinOp::Leq => i1 <= i2,
//                                 BinOp::Gt => i1 > i2,
//                                 BinOp::Geq => i1 >= i2,
//                                 _ => panic!(
//                                     "{}:{}:{} Expected comparison operator",
//                                     self.file_path, span.start_line, span.start_column
//                                 ),
//                             };
//                             Value::Bool(result)
//                         }
//                         _ => panic!(
//                             "{}:{}:{} Expected int",
//                             self.file_path, span.start_line, span.start_column
//                         ),
//                     }
//                 } else {
//                     panic!(
//                         "{}:{}:{} Unknown operator",
//                         self.file_path, span.start_line, span.start_column
//                     );
//                 }
//             }
//             Expr::Call { name, args, span } => {
//                 if name == "#length" {
//                     let arg = args.first().expect("Argument not found");
//                     let arg = self.eval_expr(arg);
//                     if let Value::Array(a) = arg {
//                         return Value::Int(a.len() as i64);
//                     } else {
//                         panic!(
//                             "{}:{}:{} Expected array",
//                             self.file_path, span.start_line, span.start_column
//                         );
//                     }
//                 }

//                 let mut arg_values = vec![];
//                 for arg in args.iter() {
//                     arg_values.push(self.eval_expr(arg));
//                 }

//                 let (params, block) = self.funs.get(name).expect("Function not found").clone();
//                 self.eval_proc(
//                     &block,
//                     params
//                         .iter()
//                         .zip(arg_values)
//                         .map(|(x, y)| (x.clone(), y))
//                         .collect(),
//                 )
//             }
//             Expr::Var { name: _, span: _ }
//             | Expr::Proj {
//                 expr: _,
//                 field: _,
//                 span: _,
//             }
//             | Expr::Index {
//                 expr: _,
//                 index: _,
//                 span: _,
//             } => self.eval_lvalue(expr).clone(),
//             Expr::Ref(expr) => {
//                 // Things we can reference, for now only vars
//                 match *expr.clone() {
//                     Expr::Var { name, span } => Value::Ref(name),
//                     _ => {
//                         error!("{}:{}:{} Expected array", self.file_path, 0, 0);
//                         panic!()
//                     }
//                 }
//             }
//         }
//     }

//     fn addressing_get(&mut self, addressing: Addressing) -> &mut Value {
//         match addressing {
//             Addressing::Var { name } => {
//                 let curr_value = self.get_var(&name);
//                 return curr_value;
//             }
//             Addressing::Proj { lhs, field } => {
//                 let x = self.addressing_get(*lhs);
//                 if let Value::Struct(s) = x {
//                     return s.get_mut(&field).unwrap();
//                 }
//                 panic!()
//             }
//         }
//     }

//     fn assign(&mut self, addressing: Addressing, value: Value) {
//         let curr_value = self.addressing_get(addressing);
//         *curr_value = value;
//     }
// }
