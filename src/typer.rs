use core::cell::Ref;
use std::any::Any;
use std::clone;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::process::exit;

use crate::parser::*;
use crate::tokeniser::Span;

use log::{debug, error, info, warn, Level};

type Scope = HashMap<String, Type>;

type TypeError = String;

pub struct Typer {
    pub program: Program,
    pub scopes: Vec<Scope>,
    pub structs: HashMap<String, HashMap<String, Type>>,
    pub procs: HashMap<String, (HashMap<String, Type>, Type)>,
    pub file_path: String,
    pub errors: Vec<TypeError>,
}

impl Typer {
    pub fn new(program: Program, file_path: String) -> Self {
        Typer {
            program,
            scopes: Vec::new(),
            structs: HashMap::new(),
            procs: HashMap::new(),
            file_path,
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, message: String) {
        self.errors.push(message);
    }

    pub fn get_proc(&self, name: &str) -> Option<(HashMap<String, Type>, Type)> {
        self.procs.get(name).cloned()
    }

    pub fn add_proc(
        &mut self,
        name: String,
        params: HashMap<String, Type>,
        ret_ty: Type,
    ) -> Option<(HashMap<String, Type>, Type)> {
        if self.procs.contains_key(&name) {
            self.add_error(format!(
                "{}:{}:{} Procedure {:?} already defined",
                self.file_path, 0, 0, name
            ));
            return None;
        }

        let proc = (params.clone(), ret_ty.clone());
        self.procs.insert(name, proc.clone());
        Some(proc)
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_type(&mut self, name: &str, ty: Type) {
        let scope = self.scopes.last_mut().expect("Scope not defined");
        if scope.contains_key(name) {
            self.add_error(format!(
                "{}:{}:{} Variable {:?} already defined",
                self.file_path, 0, 0, name
            ));
            return;
        }
        scope.insert(name.to_string(), ty);
    }

    pub fn type_check(&mut self) {
        let top_levels = self.program.clone();
        for top_level in top_levels.iter() {
            self.type_check_toplevel(top_level);
        }
    }

    fn type_check_toplevel(&mut self, top_level: &TopLevel) {
        match top_level {
            TopLevel::ProcDecl(ProcDecl {
                name,
                params,
                ret_ty,
                block,
                span,
            }) => {
                // Gather the parameter types

                if *name == "main" {
                    if !params.is_empty() {
                        self.add_error(format!(
                            "{}:{}:{} Main procedure does not accept parameters",
                            self.file_path, span.start_line, span.start_column
                        ));
                        return;
                    }

                    if *ret_ty != Type::Unit {
                        self.add_error(format!(
                            "{}:{}:{} Main procedure must return unit type",
                            self.file_path, span.start_line, span.start_column
                        ));
                        return;
                    }

                    self.type_check_block(HashMap::new(), block, Type::Unit);
                    return;
                }

                let mut param_types = HashMap::new();
                for (name, ty) in params.iter() {
                    param_types.insert(name.clone(), ty.clone());
                }

                // Add procedure to the procedure map
                self.add_proc(name.clone(), param_types.clone(), ret_ty.clone());

                // Check if the procedure body produces the correct return type
                self.type_check_block(param_types.clone(), block, ret_ty.clone());
            }
            TopLevel::StructDecl(StructDecl {
                name,
                fields,
                span: _,
            }) => {
                self.structs.insert(name.clone(), fields.clone());
            }
        }
    }

    fn get_struct_field_type(&self, name: String, field: String) -> Option<Type> {
        let fields = self.structs.get(&name);
        let field_ty = fields.and_then(|f| f.get(&field).cloned());
        field_ty
    }

    fn type_infer(&mut self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Lit(Lit::Bool(val, lit_span)) => Some(Type::Bool),
            Expr::Lit(Lit::Int(val, lit_span)) => Some(Type::Int),
            Expr::Lit(Lit::Float(val, lit_span)) => Some(Type::Float),
            Expr::Lit(Lit::Str(val, lit_span)) => Some(Type::Str),
            Expr::Call { name, args, span } => {
                let (params, ret_ty) = self
                    .get_proc(name)
                    .unwrap_or_else(|| panic!("Procedure {} not defined", name))
                    .clone();

                if params.len() != args.len() {
                    self.add_error(format!(
                        "{}:{}:{} Wrong number of arguments provided for procedure {}",
                        self.file_path, span.start_line, span.start_column, name,
                    ));
                    return None;
                }

                // Iterate over params and check that args match
                for (i, (arg, param)) in args.iter().zip(params).enumerate() {
                    let (_, ty) = param;
                    self.type_check_expr(arg, &ty);
                }

                Some(ret_ty)
            }
            // TODO: only allow ref of allocated variables, no literals
            Expr::Ref(expr) => self.type_infer(expr).map(|t| Type::Ref(Box::new(t))),
            Expr::Proj { expr, field, span } => {
                let ty = self.type_infer(expr)?;
                match ty {
                    Type::Struct(name) => self.get_struct_field_type(name, field.clone()),
                    Type::Ref(inner_ty) => {
                        if let Type::Struct(name) = *inner_ty {
                            let field_ty = self.get_struct_field_type(name, field.clone());
                            return field_ty;
                        }

                        self.add_error(format!(
                            "{}:{}:{} Projecting from non-struct reference {:?}",
                            self.file_path,
                            expr.span().start_line,
                            expr.span().start_column,
                            inner_ty
                        ));
                        None
                    }
                    _ => {
                        self.add_error(format!(
                            "{}:{}:{} Projecting from non-struct {:?}",
                            self.file_path,
                            expr.span().start_line,
                            expr.span().start_column,
                            ty
                        ));
                        None
                    }
                }
            }
            Expr::Index { expr, index, span } => {
                let ty = self.type_infer(expr)?;
                match ty {
                    Type::Array(elem_ty, size) => Some(*elem_ty),
                    _ => {
                        self.add_error(format!(
                            "{}:{}:{} Indexing into non-array {:?}",
                            self.file_path,
                            expr.span().start_line,
                            expr.span().start_column,
                            ty
                        ));
                        None
                    }
                }
            }
            Expr::Var { name, span } => {
                for scope in self.scopes.iter().rev() {
                    if let Some(ty) = scope.get(name) {
                        return Some(ty.clone());
                    }
                }

                self.add_error(format!(
                    "{}:{}:{} Variable {:?} not defined",
                    self.file_path, span.start_line, span.start_column, name
                ));
                None
            }
            Expr::Deref(expr) => {
                let inner_ty = self.type_infer(expr);
                match inner_ty {
                    Some(Type::Ref(t)) => Some(*t),
                    _ => {
                        let span = expr.span();
                        self.add_error(format!(
                            "{}:{}:{} Dereferencing non-pointer {:?}",
                            self.file_path, span.start_line, span.start_column, expr
                        ));
                        None
                    }
                }
            }
            Expr::Binary { lhs, op, rhs, span } => {
                if op.is_logical() {
                    self.type_check_expr(lhs, &Type::Bool);
                    self.type_check_expr(rhs, &Type::Bool);
                    return Some(Type::Bool);
                }

                let lhs_ty = self.type_infer(lhs)?;

                if op.is_arithmetic() {
                    return if lhs_ty == Type::Int {
                        self.type_check_expr(rhs, &Type::Int);
                        Some(Type::Int)
                    } else if lhs_ty == Type::Float {
                        self.type_check_expr(rhs, &Type::Float);
                        Some(Type::Float)
                    } else {
                        self.add_error(format!(
                            "{}:{}:{} Arithmetic operation applied to non-number type",
                            self.file_path, span.start_line, span.start_column
                        ));
                        None
                    };
                }

                if op.is_comparison() {
                    self.type_check_expr(rhs, &lhs_ty);
                    return Some(Type::Bool);
                }

                None
            }
            _ => {
                self.add_error(format!(
                    "{}:{}:{} Could not infer type from expression {}",
                    self.file_path,
                    expr.span().start_line,
                    expr.span().start_column,
                    expr
                ));
                None
            }
        }
    }

    fn type_check_block(
        &mut self,
        scope: HashMap<String, Type>,
        block: &Block,
        ret_ty: Type,
    ) -> bool {
        self.enter_scope();

        for (name, ty) in scope.iter() {
            self.add_type(name, ty.clone());
        }

        let mut has_returned = false;

        let stmt_stack = block.statements.clone();

        for (i, stmt) in block.statements.iter().enumerate() {
            if has_returned && i != stmt_stack.len() - 1 {
                self.add_error(format!(
                    "{}:{}:{} Unreachable code after return statement",
                    self.file_path,
                    stmt.span().start_line,
                    stmt.span().start_column
                ));
                return has_returned;
            }

            match stmt {
                Stmt::VarDecl {
                    name,
                    ty,
                    expr,
                    span,
                } => {
                    self.type_check_expr(expr, ty);
                    self.add_type(name, ty.clone());
                }
                Stmt::Assign { lhs, rhs, span } => {
                    let lhs_ty = self.type_infer(lhs);
                    if lhs_ty.is_none() {
                        return false;
                    }
                    self.type_check_expr(rhs, &lhs_ty.unwrap());
                }
                Stmt::Call { name, args, span } => {
                    if name == "#print" {
                        continue;
                    }

                    if name == "#length" {
                        let arg = args.first().expect("Argument not found");
                        self.type_check_expr(
                            arg,
                            &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                        );
                    }

                    let proc_ty = self.get_proc(name);

                    if proc_ty.is_none() {
                        self.add_error(format!(
                            "{}:{}:{} Procedure {} not defined",
                            self.file_path, span.start_line, span.start_column, name
                        ));
                        return false;
                    }

                    let (params, ret_ty) = proc_ty.unwrap();

                    // iterate over params and check that args match
                    for (i, arg) in args.iter().enumerate() {
                        let param = params.iter().nth(i);
                        match param {
                            Some((_, ty)) => {
                                self.type_check_expr(arg, ty);
                            }
                            None => {
                                self.add_error(format!(
                                    "{}:{}:{} Too many arguments for procedure {}",
                                    self.file_path, span.start_line, span.start_column, name
                                ));
                                // TODO: not sure if this is correct
                                return false;
                            }
                        }
                    }
                }
                Stmt::If {
                    cond,
                    then_block,
                    span,
                } => {
                    self.type_check_expr(cond, &Type::Bool);
                    self.type_check_block(HashMap::new(), then_block, ret_ty.clone());
                }
                Stmt::IfElse {
                    cond,
                    then_block,
                    else_block,
                    span,
                } => {
                    self.type_check_expr(cond, &Type::Bool);

                    let if_returns =
                        self.type_check_block(HashMap::new(), then_block, ret_ty.clone());
                    let else_returns =
                        self.type_check_block(HashMap::new(), else_block, ret_ty.clone());
                    has_returned = if_returns && else_returns;
                }
                Stmt::While { cond, block, span } => {
                    self.type_check_expr(cond, &Type::Bool);
                    self.type_check_block(HashMap::new(), block, ret_ty.clone());
                }
                Stmt::For {
                    name,
                    from,
                    to,
                    block,
                    span,
                } => {
                    self.type_check_expr(from, &Type::Int);
                    self.type_check_expr(to, &Type::Int);
                    let mut new_scope = HashMap::new();
                    new_scope.insert(name.clone(), Type::Int);
                    self.type_check_block(new_scope, block, ret_ty.clone());
                }
                Stmt::Ret { expr, span } => {
                    self.type_check_expr(expr, &ret_ty);
                    has_returned = true;
                }
            }
        }

        self.exit_scope();

        has_returned
    }

    fn type_check_expr(&mut self, expr: &Expr, target: &Type) {
        match (expr, target) {
            (Expr::Var { name, span }, Type::Array(elem_ty, size)) => {
                let actual_ty = self.type_infer(expr);
                if actual_ty.is_none() {
                    return;
                }

                match actual_ty.unwrap() {
                    Type::Array(ty, s) => {
                        match (size, s) {
                            (ArrayLength::Dynamic, ArrayLength::Dynamic) => {
                                // do nothing
                            }
                            (ArrayLength::Dynamic, ArrayLength::Fixed(s)) => {
                                // do nothing
                            }
                            (ArrayLength::Fixed(s1), ArrayLength::Fixed(s2)) => {
                                if *s1 != s2 {
                                    self.add_error(format!(
                                        "{}:{}:{} Expected array of length {:?}, got length {:?}",
                                        self.file_path, span.start_line, span.start_column, s1, s2
                                    ));
                                }
                            }
                            _ => {
                                self.add_error(format!(
                                    "{}:{}:{} Expected dynamic array, got {:?}",
                                    self.file_path, span.start_line, span.start_column, size
                                ));
                            }
                        }
                    }
                    _ => {
                        self.add_error(format!(
                            "{}:{}:{} Expected array, got {:?}",
                            self.file_path, span.start_line, span.start_column, name
                        ));
                    }
                }
            }
            (Expr::Var { name, span }, ty) => {
                let t = self.type_infer(expr).unwrap();
                if t.clone() != *ty {
                    self.add_error(format!(
                        "{}:{}:{} Type mismatch, expected {:?} got {:?}",
                        self.file_path,
                        span.start_line,
                        span.start_column,
                        ty.clone(),
                        t
                    ));
                }
            }
            (Expr::Lit(Lit::Struct(lit, lit_span)), Type::Struct(name)) => {
                let fields = self.structs.get(name).expect("Struct not defined").clone();
                for (field, ty) in fields.iter() {
                    let value = lit.get(field).expect("Field not found");
                    self.type_check_expr(value, ty);
                }
            }

            (Expr::Call { name, args, span }, ty) => {
                if name == "#length" {
                    let arg = args.first().expect("Argument not found");
                    self.type_check_expr(
                        arg,
                        &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                    );
                    return;
                }

                let actual_ty = self.type_infer(expr);

                let (params, ret_ty) = self
                    .get_proc(name)
                    .unwrap_or_else(|| panic!("Procedure {} not defined", name))
                    .clone();

                // iterate over params and check that args match
                for (i, arg) in args.iter().enumerate() {
                    let (_, ty) = params
                        .iter()
                        .nth(i)
                        .expect("Missing parameter in procedure call");
                    self.type_check_expr(arg, ty);
                }

                if ret_ty != *ty {
                    self.add_error(format!(
                        "{}:{}:{} Expected {:?}, got {:?}",
                        self.file_path, span.start_line, span.start_column, ty, ret_ty
                    ));
                }
            }
            (Expr::Proj { expr, field, span }, ty) => {
                let rec_ty = self.type_infer(expr);
            }
            (Expr::Lit(Lit::Array(lit, span)), Type::Array(ty, size)) => {
                match size {
                    ArrayLength::Dynamic => {}
                    ArrayLength::Fixed(s) => {
                        if s != &lit.len() {
                            self.add_error(format!(
                                "{}:{}:{} Expected array of length {:?}, got length {:?}",
                                self.file_path,
                                span.start_line,
                                span.start_column,
                                s,
                                lit.len()
                            ));
                            return;
                        }
                    }
                }

                for elem in lit.iter() {
                    self.type_check_expr(elem, ty);
                }
            }
            (Expr::Index { expr, index, span }, ty) => {
                let var_ty = self.type_infer(expr).unwrap();
                match var_ty {
                    Type::Array(elem_ty, _) => {
                        if elem_ty.as_ref() != ty {
                            self.add_error(format!(
                                "{}:{}:{} Expected {:?}, got {:?}",
                                self.file_path, span.start_line, span.start_column, ty, elem_ty
                            ));
                            return;
                        }
                    }
                    _ => {
                        self.add_error(format!(
                            "{}:{}:{} Expected array, got {:?}",
                            self.file_path, span.start_line, span.start_column, var_ty
                        ));
                        return;
                    }
                }

                self.type_check_expr(index, &Type::Int);
            }

            (Expr::Ref(expr), Type::Ref(ty)) => {
                self.type_check_expr(expr, ty);
            }
            (Expr::Deref(expr), Type::Ref(ty)) => {
                self.type_check_expr(expr, &Type::Ref(ty.clone()))
            }
            _ => {
                let actual_ty = self.type_infer(expr);
                if actual_ty.is_none() || actual_ty.unwrap() != *target {
                    self.add_error(format!(
                        "{}:{}:{} Expected {:?}, got expression {}",
                        self.file_path,
                        expr.span().start_line,
                        expr.span().start_column,
                        target,
                        expr
                    ));
                }
            }
        };
    }
}

// Add helper methods to get spans from AST nodes
impl Stmt {
    fn span(&self) -> &Span {
        match self {
            Stmt::VarDecl {
                span,
                name,
                ty,
                expr,
            } => span,
            Stmt::Assign { span, lhs, rhs } => span,
            Stmt::If {
                cond,
                then_block,
                span,
            } => span,
            Stmt::IfElse {
                cond,
                then_block,
                else_block,
                span,
            } => span,
            Stmt::While { cond, block, span } => span,
            Stmt::For {
                name,
                from,
                to,
                block,
                span,
            } => span,
            Stmt::Call { name, args, span } => span,
            Stmt::Ret { expr, span } => span,
        }
    }
}

impl Lit {
    fn span(&self) -> &Span {
        match self {
            Lit::Int(_, span) => span,
            Lit::Float(_, span) => span,
            Lit::Str(_, span) => span,
            Lit::Bool(_, span) => span,
            Lit::Struct(hash_map, span) => span,
            Lit::Array(exprs, span) => span,
        }
    }
}

impl Expr {
    fn span(&self) -> &Span {
        match self {
            Expr::Unit(span) => span,
            Expr::Ref(expr) => expr.span(),
            Expr::Deref(expr) => expr.span(),
            Expr::Var { name, span } => span,
            Expr::Binary { lhs, op, rhs, span } => span,
            Expr::Unary { op, rhs, span } => span,
            Expr::Call { name, args, span } => span,
            Expr::Proj { expr, field, span } => span,
            Expr::Index { expr, index, span } => span,
            Expr::Lit(lit) => lit.span(),
        }
    }
}
