use core::cell::Ref;
use std::any::Any;
use std::clone;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::process::exit;
use std::thread::spawn;

use crate::ast::*;
use crate::token::*;

use clap::builder::BoolValueParser;
use log::{debug, error, info, warn, Level};

type Scope = HashMap<String, Type>;

pub struct Typer {
    pub program: Program,
    pub scopes: Vec<Scope>,
    pub structs: HashMap<String, HashMap<String, Type>>,
    pub procs: HashMap<String, (Vec<(String, Type)>, Type)>,
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

    pub fn type_check(&mut self) {
        let top_levels = self.program.clone();
        for top_level in top_levels.iter() {
            self.type_check_toplevel(top_level);
        }
    }

    fn type_check_toplevel(&mut self, top_level: &TopLevel) {
        match top_level {
            TopLevel::ProcDecl(decl) => {
                if decl.name == "main" {
                    if !decl.params.is_empty() {
                        self.add_error(
                            decl.span.clone(),
                            TypeErrorKind::WrongArgCount {
                                name: "main".to_string(),
                                expected: 0,
                                actual: decl.params.len(),
                            },
                        );
                        return;
                    }

                    if decl.ret_ty != Type::Unit {
                        self.add_error(
                            decl.span.clone(),
                            TypeErrorKind::UnexpectedReturnType {
                                expected: TypeSet::One(Type::Unit),
                                actual: decl.ret_ty.clone(),
                            },
                        );
                        return;
                    }

                    self.type_check_block(Vec::new(), &decl.block, Type::Unit);
                    return;
                }

                let mut param_types = Vec::new();
                for (name, ty) in decl.params.iter() {
                    param_types.push((name.clone(), ty.clone()));
                }

                // Add procedure to the procedure map
                self.add_proc(decl);

                // Check if the procedure body produces the correct return type
                self.type_check_block(param_types.clone(), &decl.block, decl.ret_ty.clone());
            }
            TopLevel::StructDecl(decl) => {
                self.add_struct(decl);
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
            Expr::Unit(_) => Some(Type::Unit),
            Expr::Lit(Lit::Bool(val, lit_span)) => Some(Type::Bool),
            Expr::Lit(Lit::Int(val, lit_span)) => Some(Type::Int),
            Expr::Lit(Lit::Float(val, lit_span)) => Some(Type::Float),
            Expr::Lit(Lit::Str(val, lit_span)) => Some(Type::Str),
            Expr::MakeArray { ty, expr, span } => {
                Some(Type::Array(Box::new(ty.clone()), ArrayLength::Dynamic))
            }
            Expr::Call { name, args, span } => {
                let proc = self.get_proc(name);
                if proc.is_none() {
                    self.add_error(span.clone(), TypeErrorKind::ProcNotDefined);
                    return None;
                }
                let (params, ret_ty) = proc.unwrap();

                if params.len() != args.len() {
                    self.add_error(
                        span.clone(),
                        TypeErrorKind::WrongArgCount {
                            name: name.clone(),
                            expected: 0,
                            actual: params.len(),
                        },
                    );
                    return None;
                }

                for (i, arg) in args.iter().enumerate() {
                    let (_, ty) = params.get(i).unwrap();
                    self.type_check_expr(arg, ty);
                }

                Some(ret_ty)
            }
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

                        self.add_error(expr.span().clone(), TypeErrorKind::ProjectingNonStructRef);
                        None
                    }
                    _ => {
                        self.add_error(expr.span().clone(), TypeErrorKind::ProjectingNonStruct);
                        None
                    }
                }
            }
            Expr::Index { expr, index, span } => {
                let ty = self.type_infer(expr)?;
                match ty {
                    Type::Array(elem_ty, size) => Some(*elem_ty),
                    _ => {
                        self.add_error(expr.span().clone(), TypeErrorKind::IndexingNonArray);
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

                self.add_error(span.clone(), TypeErrorKind::VarNotDefined);
                None
            }
            Expr::Deref(expr) => {
                let inner_ty = self.type_infer(expr);
                match inner_ty {
                    Some(Type::Ref(t)) => Some(*t),
                    _ => {
                        self.add_error(expr.span().clone(), TypeErrorKind::DerefNonPointer);
                        None
                    }
                }
            }
            Expr::Unary { op, rhs, span } => match op {
                UnaryOp::Not => {
                    self.type_check_expr(rhs, &Type::Bool);
                    Some(Type::Bool)
                }
            },
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
                        self.add_error(
                            span.clone(),
                            TypeErrorKind::ArithWithNonNumber { ty: lhs_ty },
                        );
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
                self.add_error(expr.span().clone(), TypeErrorKind::NotAbleToInferType);
                None
            }
        }
    }

    fn type_check_block(
        &mut self,
        scope: Vec<(String, Type)>,
        block: &Block,
        ret_ty: Type,
    ) -> bool {
        self.enter_scope();

        for (name, ty) in scope.iter() {
            self.add_var(name, ty.clone());
        }

        let mut has_returned = false;

        let stmt_stack = block.statements.clone();

        for (i, stmt) in block.statements.iter().enumerate() {
            if has_returned && i != stmt_stack.len() - 1 {
                self.add_error(
                    stmt.span().clone(),
                    TypeErrorKind::UnreachableCodeAfterReturn,
                );
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
                    self.add_var(name, ty.clone());
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
                        let arg = args.first();
                        if arg.is_none() {
                            self.add_error(
                                span.clone(),
                                TypeErrorKind::WrongArgCount {
                                    name: "#length".to_string(),
                                    expected: 1,
                                    actual: 0,
                                },
                            );
                            return false;
                        }

                        self.type_check_expr(
                            arg.unwrap(),
                            &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                        );
                    }

                    let proc_ty = self.get_proc(name);

                    if proc_ty.is_none() {
                        self.add_error(span.clone(), TypeErrorKind::ProcNotDefined);
                        return false;
                    }

                    let (params, ret_ty) = proc_ty.unwrap();

                    if args.len() != params.len() {
                        self.add_error(
                            span.clone(),
                            TypeErrorKind::WrongArgCount {
                                name: name.clone(),
                                expected: params.len(),
                                actual: args.len(),
                            },
                        );
                        return false;
                    }

                    for (i, arg) in args.iter().enumerate() {
                        let (_, ty) = params.get(i).unwrap();
                        self.type_check_expr(arg, ty);
                    }
                }
                Stmt::If {
                    cond,
                    then_block,
                    span,
                } => {
                    self.type_check_expr(cond, &Type::Bool);
                    self.type_check_block(Vec::new(), then_block, ret_ty.clone());
                }
                Stmt::IfElse {
                    cond,
                    then_block,
                    else_block,
                    span,
                } => {
                    self.type_check_expr(cond, &Type::Bool);

                    let if_returns = self.type_check_block(Vec::new(), then_block, ret_ty.clone());
                    let else_returns =
                        self.type_check_block(Vec::new(), else_block, ret_ty.clone());
                    has_returned = if_returns && else_returns;
                }
                Stmt::While { cond, block, span } => {
                    self.type_check_expr(cond, &Type::Bool);
                    self.type_check_block(Vec::new(), block, ret_ty.clone());
                }
                Stmt::For {
                    name,
                    range,
                    block,
                    span,
                } => {
                    self.type_check_block(vec![(name.clone(), Type::Int)], block, ret_ty.clone());
                }
                Stmt::Ret { expr, span } => {
                    self.type_check_expr(expr, &ret_ty);
                    has_returned = true;
                }
            }
        }

        self.exit_scope();

        if !has_returned && ret_ty != Type::Unit {
            self.add_error(
                block.span.clone(),
                TypeErrorKind::UnexpectedReturnType {
                    expected: TypeSet::One(ret_ty),
                    actual: Type::Unit,
                },
            );
        }

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
                                    self.add_error(
                                        span.clone(),
                                        TypeErrorKind::UnexpectedArrayLength {
                                            expected: *s1,
                                            actual: s2,
                                        },
                                    );
                                }
                            }
                            (ArrayLength::Fixed(s), ArrayLength::Dynamic) => {
                                todo!()
                            }
                        }
                    }
                    actual_ty => {
                        self.add_error(
                            span.clone(),
                            TypeErrorKind::UnexpectedType {
                                expected: Type::Array(elem_ty.clone(), size.clone()),
                                actual: actual_ty.clone(),
                            },
                        );
                    }
                }
            }
            (Expr::Var { name, span }, ty) => {
                let t = self.type_infer(expr).unwrap();
                if t.clone() != *ty {
                    self.add_error(
                        span.clone(),
                        TypeErrorKind::UnexpectedType {
                            expected: ty.clone(),
                            actual: t.clone(),
                        },
                    );
                }
            }
            (Expr::Lit(Lit::Struct(lit, lit_span)), Type::Struct(name)) => {
                let fields = self.structs.get(name);
                if fields.is_none() {
                    self.add_error(lit_span.clone(), TypeErrorKind::StructNotDefined);
                    return;
                }

                let fields = fields.unwrap().clone();
                for (field, ty) in fields.iter() {
                    let value = lit.get(field);
                    if value.is_none() {
                        self.add_error(
                            lit_span.clone(),
                            TypeErrorKind::StructFielNotFound {
                                field: field.clone(),
                                struct_name: name.clone(),
                            },
                        );
                    }
                    self.type_check_expr(value.unwrap(), ty);
                }
            }
            (
                Expr::MakeArray {
                    ty: elem_ty,
                    expr,
                    span,
                },
                Type::Array(ty, ArrayLength::Dynamic),
            ) => {
                if *elem_ty != **ty {
                    self.add_error(
                        span.clone(),
                        TypeErrorKind::UnexpectedArrayType {
                            expected: *ty.clone(),
                            actual: elem_ty.clone(),
                        },
                    );
                }
                self.type_check_expr(expr, &Type::Int);
            }
            (Expr::Call { name, args, span }, ty) => {
                if name == "#length" {
                    let arg = args.first();
                    if arg.is_none() {
                        self.add_error(
                            span.clone(),
                            TypeErrorKind::WrongArgCount {
                                name: "#length".to_string(),
                                expected: 1,
                                actual: 0,
                            },
                        );
                    }

                    self.type_check_expr(
                        arg.unwrap(),
                        &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                    );
                    return;
                }

                let actual_ty = self.type_infer(expr);

                let proc = self.get_proc(name);
                if proc.is_none() {
                    self.add_error(span.clone(), TypeErrorKind::ProcNotDefined);
                    return;
                }
                let (params, ret_ty) = proc.unwrap();

                // iterate over params and check that args match
                for (i, arg) in args.iter().enumerate() {
                    let (_, ty) = params.get(i).unwrap();
                    self.type_check_expr(arg, ty);
                }

                if ret_ty != *ty {
                    self.add_error(
                        span.clone(),
                        TypeErrorKind::UnexpectedType {
                            expected: ty.clone(),
                            actual: ret_ty.clone(),
                        },
                    );
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
                            self.add_error(
                                span.clone(),
                                TypeErrorKind::UnexpectedArrayLength {
                                    expected: *s,
                                    actual: lit.len(),
                                },
                            );
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
                            self.add_error(
                                span.clone(),
                                TypeErrorKind::UnexpectedArrayType {
                                    expected: ty.clone(),
                                    actual: *elem_ty.clone(),
                                },
                            );
                            return;
                        }
                    }
                    _ => {
                        self.add_error(span.clone(), TypeErrorKind::IndexingNonArray);
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
                if actual_ty.is_none() || actual_ty.clone().unwrap() != *target {
                    self.add_error(
                        expr.span().clone(),
                        TypeErrorKind::UnexpectedType {
                            expected: target.clone(),
                            actual: actual_ty.unwrap().clone(),
                        },
                    );
                }
            }
        };
    }

    fn add_error(&mut self, span: Span, kind: TypeErrorKind) {
        self.errors.push(TypeError { span, kind });
    }

    pub fn get_proc(&self, name: &str) -> Option<(Vec<(String, Type)>, Type)> {
        self.procs.get(name).cloned()
    }

    pub fn add_proc(&mut self, decl: &ProcDecl) {
        if self.procs.contains_key(&decl.name) {
            self.add_error(decl.span.clone(), TypeErrorKind::ProcAlreadyDefined);
            return;
        }

        let proc = (decl.params.clone(), decl.ret_ty.clone());
        self.procs.insert(decl.name.clone(), proc.clone());
    }

    pub fn add_struct(&mut self, decl: &StructDecl) {
        if self.structs.contains_key(&decl.name) {
            self.add_error(decl.span.clone(), TypeErrorKind::StructAlreadyDefined);
            return;
        }

        let fields = decl
            .fields
            .iter()
            .map(|(k, v)| (k.clone(), v.0.clone()))
            .collect();
        self.structs.insert(decl.name.clone(), fields);
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_var(&mut self, name: &str, ty: Type) {
        let scope = self.scopes.last_mut();
        if scope.is_none() {
            self.add_error(Span::empty(), TypeErrorKind::ScopeNotDefined);
            return;
        }
        let scope = scope.unwrap();
        if scope.contains_key(name) {
            self.add_error(Span::empty(), TypeErrorKind::VarAlreadyDefined);
            return;
        }
        scope.insert(name.to_string(), ty);
    }
}
