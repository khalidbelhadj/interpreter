use crate::ast::*;
use crate::token::*;
use crate::typer::SymbolTable;
use std::array;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::format;
use std::fmt::Display;
use std::process::exit;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Struct(HashMap<String, Rc<RefCell<Value>>>),
    Array(Vec<Rc<RefCell<Value>>>),
    Ref(Rc<RefCell<Value>>),
    Deref(Rc<RefCell<Value>>),
}

impl Value {
    pub fn new_ref(value: Value) -> Rc<RefCell<Value>> {
        Rc::new(RefCell::new(value))
    }

    pub fn clone_deep(&self) -> Value {
        use Value::*;
        match self {
            Struct(r) => {
                let new_map: HashMap<_, _> = r
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::new_ref(v.borrow().clone())))
                    .collect();
                Struct(new_map)
            }
            Ref(value) => Ref(Rc::clone(value)),
            _ => self.clone(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Value::Unit => "unit".to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Str(s) => s.clone(),
            Value::Struct(r) => {
                let mut s = "{".to_string();
                for (i, (name, value)) in r.iter().enumerate() {
                    s.push_str(&format!("{}: {}", name, value.borrow()));
                    if i < r.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push('}');
                s
            }
            Value::Array(a) => {
                let mut s = "[".to_string();
                for (i, value) in a.iter().enumerate() {
                    s.push_str(&value.borrow().to_string());
                    if i < a.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push(']');
                s
            }
            Value::Ref(value) => format!("&{}", value.borrow()),
            Value::Deref(value) => format!("{}", value.borrow()),
        };

        write!(f, "{string}")
    }
}

#[derive(Debug)]
pub struct StackFrame {
    function_name: String,
    return_value: Option<Value>,
    scopes: Vec<HashMap<String, Rc<RefCell<Value>>>>,
}

impl StackFrame {
    pub(crate) fn new(name: String) -> Self {
        StackFrame {
            function_name: name,
            return_value: None,
            scopes: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn define(&mut self, name: String, value: Rc<RefCell<Value>>) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name, value);
        }
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value_ref) = scope.get(name) {
                return Some(value_ref.clone());
            }
        }
        None
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), String> {
        for scope in self.scopes.iter().rev() {
            if let Some(value_ref) = scope.get(name) {
                *value_ref.borrow_mut() = value;
                return Ok(());
            }
        }
        Err(format!(
            "Could not assign to variable '{}', not defined",
            name
        ))
    }
}

pub struct Evaluator {
    call_stack: Vec<StackFrame>,
    table: SymbolTable,
}

impl Evaluator {
    pub fn new(table: SymbolTable) -> Self {
        Evaluator {
            call_stack: Vec::new(),
            table,
        }
    }

    fn value_from_type(&mut self, ty: Type) -> Result<Value, String> {
        match ty {
            Type::Unit => Ok(Value::Unit),
            Type::Int => Ok(Value::Int(0)),
            Type::Float => Ok(Value::Float(0.0)),
            Type::Str => Ok(Value::Str("".to_string())),
            Type::Bool => Ok(Value::Bool(false)),
            Type::Struct(s) => {
                let Some(struct_decl) = self.table.structs.get_mut(&s) else {
                    return Err("No struct".to_string());
                };

                let mut map = HashMap::new();

                for (k, v) in struct_decl.fields.clone().iter() {
                    match &v.1 {
                        Some(expr) => {
                            let value = Value::new_ref(self.eval_expr(expr)?);
                            map.insert(k.to_string(), value);
                        }
                        None => {
                            let value = Value::new_ref(self.value_from_type(v.0.clone())?);
                            map.insert(k.to_string(), value);
                        }
                    }
                }
                Ok(Value::Struct(map))
            }
            Type::Array(ty, array_length) => match array_length {
                ArrayLength::Fixed(length) => {
                    let mut result = Vec::new();
                    for _ in 0..length {
                        let r = self.value_from_type(*ty.clone())?;
                        result.push(Value::new_ref(r));
                    }
                    Ok(Value::Array(result))
                }
                ArrayLength::Dynamic => Ok(Value::Array(Vec::new())),
            },
            Type::Ref(_) => unreachable!(),
        }
    }

    pub fn current_frame(&mut self) -> Result<&mut StackFrame, String> {
        self.call_stack
            .last_mut()
            .ok_or("No stack frame".to_string())
    }

    pub fn eval_program(&mut self, program: &Program) -> Result<Value, String> {
        if let Some(main_func) = self.table.procs.get("main").cloned() {
            self.call_proc(&main_func, vec![])
        } else {
            Err("No main program".to_string())
        }
    }

    pub fn get_lvalue_ref(&mut self, expr: &Expr) -> Result<Rc<RefCell<Value>>, String> {
        match expr {
            Expr::Var { name, .. } => self
                .current_frame()?
                .lookup(name)
                .ok_or_else(|| format!("Variable '{}' not found", name)),
            Expr::Proj { expr, field, .. } => {
                let base_ref = self.get_lvalue_ref(expr)?;
                let base_value = base_ref.borrow();

                match &*base_value {
                    Value::Struct(fields) => fields
                        .get(field)
                        .cloned()
                        .ok_or_else(|| format!("Field '{}' not found", field)),
                    Value::Ref(value) => {
                        let value = value.borrow().clone();
                        if let Value::Struct(fields) = value {
                            return fields
                                .get(field)
                                .cloned()
                                .ok_or_else(|| format!("Field '{}' not found", field));
                        }
                        Err("bruh".to_string())
                    }
                    _ => Err("Cannot access field of non-struct".to_string()),
                }
            }
            Expr::Index { expr, index, .. } => {
                let base_ref = self.get_lvalue_ref(expr)?;
                let index_value = self.eval_expr(index)?;

                let base_value = base_ref.borrow();
                match (&*base_value, index_value) {
                    (Value::Array(arr), Value::Int(idx)) => {
                        if idx < 0 || idx as usize >= arr.len() {
                            return Err("Array index out of bounds".to_string());
                        }
                        Ok(arr[idx as usize].clone())
                    }
                    _ => Err("Invalid array indexing".to_string()),
                }
            }
            Expr::Deref(expr) => {
                let res = self.get_lvalue_ref(expr);
                match res {
                    Ok(inner) => {
                        if let Value::Ref(inner_inner) = inner.borrow().clone() {
                            Ok(Rc::clone(&inner_inner))
                        } else {
                            Err("".to_string())
                        }
                    }
                    _ => Err("Dereferencing a non-ref type".to_string()),
                }
            }
            _ => Err("Invalid lvalue expression".to_string()),
        }
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>, String> {
        match stmt {
            Stmt::VarDecl { name, expr, .. } => {
                let value = self.eval_expr(expr)?;
                let value_ref = Value::new_ref(value);
                self.current_frame()?.define(name.clone(), value_ref);
                Ok(None)
            }

            Stmt::Assign { lhs, rhs, .. } => {
                let rhs_value = self.eval_expr(rhs)?;
                let lvalue_ref = self.get_lvalue_ref(lhs)?;

                *lvalue_ref.borrow_mut() = rhs_value;
                Ok(None)
            }

            Stmt::If {
                cond, then_block, ..
            } => {
                let cond_value = self.eval_expr(cond)?;

                match cond_value {
                    Value::Bool(true) => self.eval_block(then_block),
                    Value::Bool(false) => Ok(None),
                    _ => exit(1),
                }
            }

            Stmt::IfElse {
                cond,
                then_block,
                else_block,
                ..
            } => {
                let cond_value = self.eval_expr(cond)?;

                match cond_value {
                    Value::Bool(true) => self.eval_block(then_block),
                    Value::Bool(false) => self.eval_block(else_block),
                    _ => exit(1),
                }
            }

            Stmt::While { cond, block, .. } => {
                loop {
                    let cond_value = self.eval_expr(cond)?;

                    match cond_value {
                        Value::Bool(true) => break,
                        Value::Bool(false) => {
                            if let Some(return_value) = self.eval_block(block)? {
                                return Ok(Some(return_value));
                            }
                        }
                        _ => exit(1),
                    }
                }
                Ok(None)
            }

            Stmt::For {
                name: iterator_name,
                range,
                block,
                ..
            } => {
                if let Expr::Call(Call { name, args, span }) = range {
                    if name != "#range" {
                        return Err("For loop only with #range".to_string());
                    }

                    let (mut start_value, mut end_value) = (Value::Int(0), Value::Int(0));
                    match args.len() {
                        1 => {
                            end_value = self.eval_expr(&args[0])?;
                        }
                        2 => {
                            start_value = self.eval_expr(&args[0])?;
                            end_value = self.eval_expr(&args[1])?;
                        }
                        _ => return Err("Wrong number of args for #range".to_string()),
                    };

                    if let (Value::Int(start), Value::Int(end)) = (start_value, end_value) {
                        // Run the loop
                        for i in start..end {
                            let loop_var = Value::new_ref(Value::Int(i));
                            self.current_frame()?
                                .define(iterator_name.clone(), loop_var);

                            if let Some(return_value) = self.eval_block(block)? {
                                self.current_frame()?.pop_scope();
                                return Ok(Some(return_value));
                            }
                        }
                    }
                } else {
                    return Err("For loop only with #range".to_string());
                }

                Ok(None)
            }
            Stmt::Call(call) => self.eval_call(call),
            Stmt::Ret { expr, .. } => {
                let return_value = self.eval_expr(expr)?;
                Ok(Some(return_value))
            }
        }
    }

    pub fn eval_call(&mut self, call: &Call) -> Result<Option<Value>, String> {
        let Call { name, args, .. } = call;

        if name == "#print" {
            for (i, arg) in args.iter().enumerate() {
                let value = self.eval_expr(arg)?;
                print!("{}", value);
                if i < args.len() - 1 {
                    print!(" ");
                }
            }
            println!();
            return Ok(None);
        }

        if name == "#length" {
            if args.len() != 1 {
                return Err("Wrong number of arguments provided to #length".to_string());
            }

            let arg = args[0].clone();
            let value = self.eval_expr(&arg)?;
            return match value {
                Value::Array(inner) => Ok(Some(Value::Int(inner.len() as i64))),
                Value::Str(inner) => Ok(Some(Value::Int(inner.len() as i64))),
                _ => Err("Cannot find length of argument".to_string()),
            };
        }

        let arg_values = self.eval_args(args)?;
        if let Some(func) = self.table.procs.get(name).cloned() {
            self.call_proc(&func, arg_values)?;
        } else {
            return Err(format!("Procedure '{}' not found", name));
        }
        Ok(None)
    }

    pub fn eval_block(&mut self, block: &Block) -> Result<Option<Value>, String> {
        self.current_frame()?.push_scope();

        let mut result = None;
        for stmt in &block.statements {
            if let Some(return_value) = self.eval_stmt(stmt)? {
                result = Some(return_value);
                break;
            }
        }

        self.current_frame()?.pop_scope();
        Ok(result)
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Unit(_) => Ok(Value::Unit),
            Expr::Lit(lit) => self.eval_literal(lit),
            Expr::MakeArray { ty, expr, span } => {
                let length = self.eval_expr(expr)?;
                match length {
                    Value::Int(i) => Ok(self.value_from_type(Type::Array(
                        Box::new(ty.clone()),
                        ArrayLength::Fixed(i as usize),
                    ))?),
                    _ => Err("bruh".to_string()),
                }
            }
            Expr::Var { name, .. } => {
                let value_ref = self
                    .current_frame()?
                    .lookup(name)
                    .ok_or_else(|| format!("Variable '{}' not found", name))?;
                let x = Ok(value_ref.borrow().clone());
                x
            }
            Expr::Binary { lhs, op, rhs, .. } => {
                self.eval_binary_op(*lhs.clone(), op.clone(), *rhs.clone())
            }
            Expr::Unary { op, rhs, span } => self.eval_unary_op(op.clone(), *rhs.clone()),
            Expr::Call(call) => self.eval_call(call).map(|x| x.unwrap_or(Value::Unit)),
            Expr::Proj { expr, field, .. } => {
                let base_value = self.eval_expr(expr)?;
                match base_value {
                    Value::Struct(fields) => {
                        let field_ref = fields
                            .get(field)
                            .ok_or_else(|| format!("Field '{}' not found", field))?;
                        Ok(field_ref.borrow().clone())
                    }
                    Value::Ref(value) => {
                        let value = value.borrow().clone();
                        if let Value::Struct(fields) = value {
                            let field_ref = fields
                                .get(field)
                                .ok_or_else(|| format!("Field '{}' not found", field))?;
                            return Ok(field_ref.borrow().clone());
                        }
                        Err("bruh".to_string())
                    }
                    _ => Err("Cannot access field of non-struct".to_string()),
                }
            }
            Expr::Index { expr, index, .. } => {
                let base_value = self.eval_expr(expr)?;
                let index_value = self.eval_expr(index)?;

                match (base_value, index_value) {
                    (Value::Array(arr), Value::Int(idx)) => {
                        if idx < 0 || idx as usize >= arr.len() {
                            return Err("Array index out of bounds".to_string());
                        }
                        Ok(arr[idx as usize].borrow().clone())
                    }
                    _ => Err("Invalid array indexing".to_string()),
                }
            }
            Expr::Ref(expr) => {
                let lvalue_ref = self.get_lvalue_ref(expr)?;
                Ok(Value::Ref(lvalue_ref))
            }
            Expr::Deref(expr) => {
                let lvalue_ref = self.get_lvalue_ref(expr)?;
                let ref_value = lvalue_ref.borrow().clone();
                let x = match ref_value {
                    Value::Ref(inner) => Ok(inner.borrow().clone()),
                    _ => {
                        return Err("lskdfj".to_string());
                    }
                };
                x
            }
        }
    }

    fn call_proc(&mut self, proc: &ProcDecl, args: Vec<Value>) -> Result<Value, String> {
        let frame = StackFrame::new(proc.name.clone());
        self.call_stack.push(frame);
        self.current_frame()?.push_scope();

        let param_names: Vec<String> = proc.params.iter().map(|x| x.0.clone()).collect();
        if param_names.len() != args.len() {
            self.current_frame()?.pop_scope();
            self.call_stack.pop();

            return Err(format!(
                "Function '{}' expects {} arguments, got {}",
                proc.name,
                param_names.len(),
                args.len()
            ));
        }

        for (param_name, arg_value) in param_names.iter().zip(args.iter()) {
            let arg_ref = Value::new_ref(arg_value.clone_deep());
            self.current_frame()?.define(param_name.clone(), arg_ref);
        }

        let result = self.eval_block(&proc.block)?;

        self.current_frame()?.pop_scope();
        self.call_stack.pop();
        Ok(result.unwrap_or(Value::Unit))
    }

    fn eval_args(&mut self, args: &[Expr]) -> Result<Vec<Value>, String> {
        args.iter().map(|arg| self.eval_expr(arg)).collect()
    }

    fn eval_literal(&mut self, lit: &Lit) -> Result<Value, String> {
        match lit {
            Lit::Int(n, _) => Ok(Value::Int(*n)),
            Lit::Float(f, _) => Ok(Value::Float(*f)),
            Lit::Str(s, _) => Ok(Value::Str(s.clone())),
            Lit::Bool(b, _) => Ok(Value::Bool(*b)),

            Lit::Struct(fields, _) => {
                let mut struct_fields = HashMap::new();
                for (field_name, field_expr) in fields {
                    let field_value = self.eval_expr(field_expr)?;
                    let field_ref = Value::new_ref(field_value);
                    struct_fields.insert(field_name.clone(), field_ref);
                }
                Ok(Value::Struct(struct_fields))
            }

            Lit::Array(elements, _) => {
                let mut array_elements = Vec::new();
                for element in elements {
                    let element_value = self.eval_expr(element)?;
                    let element_ref = Value::new_ref(element_value);
                    array_elements.push(element_ref);
                }
                Ok(Value::Array(array_elements))
            }
        }
    }

    fn eval_unary_op(&mut self, op: UnaryOp, rhs: Expr) -> Result<Value, String> {
        let right_val = self.eval_expr(&rhs)?;
        match (&op, &right_val) {
            (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("Invalid unary operation: {:?} {:?}", op, right_val)),
        }
    }

    fn eval_binary_op(&mut self, lhs: Expr, op: BinaryOp, rhs: Expr) -> Result<Value, String> {
        use crate::ast::BinaryOp::*;
        let left_val = self.eval_expr(&lhs)?;
        let right_val = self.eval_expr(&rhs)?;

        match (&left_val, &op, &right_val) {
            (Value::Int(a), Add, Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Int(a), Sub, Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Int(a), Mul, Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Int(a), Div, Value::Int(b)) => {
                if *b == 0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (Value::Float(a), Add, Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Float(a), Sub, Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Float(a), Mul, Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Float(a), Div, Value::Float(b)) => {
                if *b == 0.0 {
                    Err("Division by zero".to_string())
                } else {
                    Ok(Value::Float(a / b))
                }
            }
            (Value::Int(a), Eq, Value::Int(b)) => Ok(Value::Bool(a == b)),
            (Value::Int(a), Neq, Value::Int(b)) => Ok(Value::Bool(a != b)),
            (Value::Int(a), Lt, Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Int(a), Leq, Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::Int(a), Gt, Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Int(a), Geq, Value::Int(b)) => Ok(Value::Bool(a >= b)),

            (Value::Float(a), Eq, Value::Float(b)) => Ok(Value::Bool(a == b)),
            (Value::Float(a), Neq, Value::Float(b)) => Ok(Value::Bool(a != b)),
            (Value::Float(a), Lt, Value::Float(b)) => Ok(Value::Bool(a < b)),
            (Value::Float(a), Leq, Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (Value::Float(a), Gt, Value::Float(b)) => Ok(Value::Bool(a > b)),
            (Value::Float(a), Geq, Value::Float(b)) => Ok(Value::Bool(a >= b)),

            (Value::Bool(a), And, Value::Bool(b)) => Ok(Value::Bool(*a && *b)),
            (Value::Bool(a), Or, Value::Bool(b)) => Ok(Value::Bool(*a || *b)),
            (Value::Bool(a), Eq, Value::Bool(b)) => Ok(Value::Bool(a == b)),
            (Value::Bool(a), Neq, Value::Bool(b)) => Ok(Value::Bool(a != b)),

            (Value::Str(a), Add, Value::Str(b)) => Ok(Value::Str(format!("{}{}", a, b))),
            (Value::Str(a), Eq, Value::Str(b)) => Ok(Value::Bool(a == b)),
            (Value::Str(a), Neq, Value::Str(b)) => Ok(Value::Bool(a != b)),

            _ => Err(format!(
                "Invalid binary operation: {:?} {:?} {:?}",
                left_val, op, right_val
            )),
        }
    }
}
