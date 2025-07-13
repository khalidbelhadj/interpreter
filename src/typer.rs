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

#[derive(Debug)]
pub struct SymbolTable {
    pub structs: HashMap<String, StructDecl>,
    pub procs: HashMap<String, ProcDecl>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            structs: HashMap::new(),
            procs: HashMap::new(),
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

type Scope = HashMap<String, Type>;

#[derive(Debug)]
pub struct ProcScope {
    scopes: Vec<Scope>,
}

impl ProcScope {
    fn new() -> ProcScope {
        ProcScope { scopes: Vec::new() }
    }
}

pub struct Typer {
    pub program: Program,
    pub scopes: Vec<Scope>,
    pub table: SymbolTable,
    pub file_path: String,
    pub errors: Vec<TypeError>,
}

impl Typer {
    pub fn new(program: Program, file_path: String) -> Self {
        Typer {
            program,
            scopes: Vec::new(),
            table: SymbolTable::new(),
            file_path,
            errors: Vec::new(),
        }
    }

    pub fn type_check(&mut self) {
        let top_levels = self.program.top_levels.clone();
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
                            decl.span,
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
                            decl.span,
                            TypeErrorKind::UnexpectedReturnType {
                                expected: TypeSet::One(Type::Unit),
                                actual: decl.ret_ty.clone(),
                            },
                        );
                        return;
                    }
                }

                let mut param_types = Vec::new();
                for (name, ty) in decl.params.iter() {
                    param_types.push((name.clone(), ty.clone()));
                }

                // Add procedure to the procedure map
                self.add_proc(decl);

                // Check if the procedure body produces the correct return type
                self.type_check_proc(param_types.clone(), &decl.block, decl.ret_ty.clone());
            }
            TopLevel::StructDecl(decl) => {
                self.add_struct(decl);
            }
        }
    }

    fn get_struct_field_type(&mut self, name: String, field: String) -> Option<Type> {
        let decl = self.table.structs.get(&name);
        match decl {
            Some(decl) => match decl.fields.get(&field) {
                Some(ty) => Some(ty.0.clone()),
                None => {
                    // TODO: Empty span
                    self.add_error(
                        Span::empty(),
                        TypeErrorKind::StructFieldMissingInLiteral {
                            field: field.clone(),
                            struct_name: decl.name.clone(),
                        },
                    );
                    None
                }
            },
            None => {
                // TODO: Empty span
                self.add_error(Span::empty(), TypeErrorKind::StructNotDefined);
                None
            }
        }
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
            Expr::Call(Call { name, args, span }) => {
                let Some(ProcDecl { params, ret_ty, .. }) = self.table.procs.get(name).cloned()
                else {
                    self.add_error(*span, TypeErrorKind::ProcNotDefined);
                    return None;
                };

                if params.len() != args.len() {
                    self.add_error(
                        *span,
                        TypeErrorKind::WrongArgCount {
                            name: name.clone(),
                            expected: 0,
                            actual: params.len(),
                        },
                    );
                    return None;
                }

                for (i, arg) in args.iter().enumerate() {
                    let Some((_, ty)) = params.get(i) else {
                        unreachable!()
                    };
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
                            self.get_struct_field_type(name, field.clone())
                        } else {
                            self.add_error(expr.span(), TypeErrorKind::ProjectingNonStructRef);
                            None
                        }
                    }
                    _ => {
                        self.add_error(expr.span(), TypeErrorKind::ProjectingNonStruct);
                        None
                    }
                }
            }
            Expr::Index { expr, index, span } => {
                let ty = self.type_infer(expr)?;
                match ty {
                    Type::Array(elem_ty, size) => Some(*elem_ty),
                    _ => {
                        self.add_error(expr.span(), TypeErrorKind::IndexingNonArray);
                        None
                    }
                }
            }
            Expr::Var { name, span } => self.lookup(name.to_string()),
            Expr::Deref(expr) => {
                let inner_ty = self.type_infer(expr);
                match inner_ty {
                    Some(Type::Ref(t)) => Some(*t),
                    _ => {
                        self.add_error(expr.span(), TypeErrorKind::DerefNonPointer);
                        None
                    }
                }
            }
            Expr::Unary { op, rhs, span } => match op {
                UnaryOp::Not => {
                    self.type_check_expr(rhs, &Type::Bool);
                    Some(Type::Bool)
                }
                UnaryOp::Minus => match self.type_infer(rhs) {
                    Some(Type::Int) => Some(Type::Int),
                    Some(Type::Float) => Some(Type::Float),
                    Some(ty) => {
                        self.add_error(*span, TypeErrorKind::ArithWithNonNumber { ty });
                        None
                    }
                    _ => None,
                },
                UnaryOp::Plus => todo!(),
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
                        self.add_error(*span, TypeErrorKind::ArithWithNonNumber { ty: lhs_ty });
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
                self.add_error(expr.span(), TypeErrorKind::NotAbleToInferType);
                None
            }
        }
    }

    fn type_check_proc(&mut self, args: Vec<(String, Type)>, block: &Block, ret_ty: Type) {
        self.type_check_block(args, block, ret_ty);
    }

    fn type_check_block(
        &mut self,
        scope: Vec<(String, Type)>,
        block: &Block,
        ret_ty: Type,
    ) -> bool {
        self.enter_scope();

        for (name, ty) in scope.iter() {
            self.define(name, ty.clone());
        }

        let mut has_returned = false;

        let stmt_stack = block.statements.clone();

        for (i, stmt) in block.statements.iter().enumerate() {
            if has_returned && i != stmt_stack.len() - 1 {
                self.add_error(*stmt.span(), TypeErrorKind::UnreachableCodeAfterReturn);
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
                    self.define(name, ty.clone());
                }
                Stmt::Assign { lhs, rhs, span } => {
                    let Some(lhs_ty) = self.type_infer(lhs) else {
                        return false;
                    };
                    self.type_check_expr(rhs, &lhs_ty);
                }
                Stmt::Call(Call { name, args, span }) => {
                    if name == "#print" {
                        for arg in args.iter() {
                            self.type_infer(arg);
                        }
                        continue;
                    }

                    if name == "#stack" {
                        continue;
                    }

                    if name == "#length" {
                        let Some(arg) = args.first() else {
                            self.add_error(
                                *span,
                                TypeErrorKind::WrongArgCount {
                                    name: "#length".to_string(),
                                    expected: 1,
                                    actual: 0,
                                },
                            );
                            return false;
                        };

                        self.type_check_expr(
                            arg,
                            &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                        );
                    }

                    let Some(ProcDecl { params, ret_ty, .. }) = self.table.procs.get(name).cloned()
                    else {
                        self.add_error(*span, TypeErrorKind::ProcNotDefined);
                        return false;
                    };

                    if params.len() != args.len() {
                        self.add_error(
                            *span,
                            TypeErrorKind::WrongArgCount {
                                name: name.clone(),
                                expected: 0,
                                actual: params.len(),
                            },
                        );
                        return false;
                    }

                    for (i, arg) in args.iter().enumerate() {
                        let (_, ty) = params.get(i).unwrap_or_else(|| unreachable!());
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
                block.span,
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
            (Expr::Var { name, span }, Type::Array(elem_ty, expected_size)) => {
                let Some(actual_ty) = self.type_infer(expr) else {
                    return;
                };

                match actual_ty {
                    Type::Array(ty, actual_size) => {
                        match (expected_size, actual_size) {
                            (ArrayLength::Dynamic, ArrayLength::Dynamic) => {
                                // do nothing
                            }
                            (ArrayLength::Dynamic, ArrayLength::Fixed(s)) => {
                                // do nothing
                            }
                            (ArrayLength::Fixed(s1), ArrayLength::Fixed(s2)) => {
                                if *s1 != s2 {
                                    self.add_error(
                                        *span,
                                        TypeErrorKind::UnexpectedArrayLength {
                                            expected: *s1,
                                            actual: s2,
                                        },
                                    );
                                }
                            }
                            (ArrayLength::Fixed(s), ArrayLength::Dynamic) => {
                                // TODO: manage expected fixed given dynamic
                                todo!()
                            }
                        }
                    }
                    actual_ty => {
                        self.add_error(
                            *span,
                            TypeErrorKind::UnexpectedType {
                                expected: Type::Array(elem_ty.clone(), expected_size.clone()),
                                actual: actual_ty.clone(),
                            },
                        );
                    }
                }
            }
            (Expr::Var { name, span }, ty) => {
                let Some(t) = self.type_infer(expr) else {
                    return;
                };
                if t.clone() != *ty {
                    self.add_error(
                        *span,
                        TypeErrorKind::UnexpectedType {
                            expected: ty.clone(),
                            actual: t.clone(),
                        },
                    );
                }
            }
            (Expr::Lit(Lit::Struct(lit_name, lit, lit_span)), Type::Struct(name)) => {
                if lit_name != name {
                    self.add_error(
                        *lit_span,
                        TypeErrorKind::UnexpectedType {
                            expected: Type::Struct(name.to_string()),
                            actual: Type::Struct(lit_name.to_string()),
                        },
                    );
                    return;
                }

                let Some(decl) = self.table.structs.get(name) else {
                    self.add_error(*lit_span, TypeErrorKind::StructNotDefined);
                    return;
                };

                // TODO: is this clone needed?
                for (field, (ty, _)) in decl.fields.clone() {
                    if let Some(expr) = lit.get(&field) {
                        self.type_check_expr(expr, &ty);
                    } else {
                        // Use default value instead

                        // self.add_error(
                        //     *lit_span,
                        //     TypeErrorKind::StructFieldMissingInLiteral {
                        //         field: field.clone(),
                        //         struct_name: name.clone(),
                        //     },
                        // );
                    };
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
                        *span,
                        TypeErrorKind::UnexpectedArrayType {
                            expected: *ty.clone(),
                            actual: elem_ty.clone(),
                        },
                    );
                }
                self.type_check_expr(expr, &Type::Int);
            }
            (Expr::Call(Call { name, args, span }), ty) => {
                if name == "#length" {
                    let Some(arg) = args.first() else {
                        self.add_error(
                            *span,
                            TypeErrorKind::WrongArgCount {
                                name: "#length".to_string(),
                                expected: 1,
                                actual: 0,
                            },
                        );
                        return;
                    };

                    self.type_check_expr(
                        arg,
                        &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                    );
                }

                if let Some(actual_ty) = self.type_infer(expr) {
                    if actual_ty != *ty {
                        self.add_error(
                            *span,
                            TypeErrorKind::UnexpectedType {
                                expected: ty.clone(),
                                actual: actual_ty.clone(),
                            },
                        );
                    }
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
                                *span,
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
                match self.type_infer(expr) {
                    Some(Type::Array(elem_ty, _)) => {
                        if elem_ty.as_ref() != ty {
                            self.add_error(
                                *span,
                                TypeErrorKind::UnexpectedArrayType {
                                    expected: ty.clone(),
                                    actual: *elem_ty.clone(),
                                },
                            );
                            return;
                        }
                    }
                    _ => {
                        self.add_error(*span, TypeErrorKind::IndexingNonArray);
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
                if let Some(inferred_ty) = actual_ty {
                    if inferred_ty != *target {
                        self.add_error(
                            expr.span(),
                            TypeErrorKind::UnexpectedType {
                                expected: target.clone(),
                                actual: inferred_ty.clone(),
                            },
                        );
                    }
                }
            }
        };
    }

    fn add_error(&mut self, span: Span, kind: TypeErrorKind) {
        self.errors.push(TypeError { span, kind });
    }

    pub fn add_proc(&mut self, decl: &ProcDecl) {
        if self.table.procs.contains_key(&decl.name) {
            self.add_error(decl.span, TypeErrorKind::ProcAlreadyDefined);
            return;
        }

        self.table.procs.insert(decl.name.clone(), decl.clone());
    }

    pub fn add_struct(&mut self, decl: &StructDecl) {
        if self.table.structs.contains_key(&decl.name) {
            self.add_error(decl.span, TypeErrorKind::StructAlreadyDefined);
            return;
        }

        self.table.structs.insert(decl.name.clone(), decl.clone());
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        let Some(_) = self.scopes.pop() else {
            // TODO: Empty span
            self.add_error(Span::empty(), TypeErrorKind::ScopeNotDefined);
            return;
        };
    }

    fn define(&mut self, name: &str, ty: Type) {
        let Some(scope) = self.scopes.last_mut() else {
            // TODO: Empty span
            self.add_error(Span::empty(), TypeErrorKind::ScopeNotDefined);
            return;
        };

        if scope.contains_key(name) {
            // TODO: Empty span
            self.add_error(Span::empty(), TypeErrorKind::VarAlreadyDefined);
            return;
        }
        scope.insert(name.to_string(), ty);
    }

    fn lookup(&mut self, name: String) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(value_ref) = scope.get(&name) {
                return Some(value_ref.clone());
            }
        }

        // TODO: Empty span
        self.add_error(Span::empty(), TypeErrorKind::VarNotDefined);
        None
    }
}
