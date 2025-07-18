// use core::cell::Ref;
use std::any::Any;
use std::clone;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::process::exit;
use std::thread::spawn;

use crate::ast::*;
use crate::error::*;
use crate::token::*;

use log::{debug, error, info, warn, Level};

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
                // Special case for main function
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

                self.add_proc(decl);
                self.type_check_proc(param_types.clone(), &decl.block, decl.ret_ty.clone());
            }
            TopLevel::StructDecl(decl) => {
                self.add_struct(decl);

                // Check the default values
                let StructDecl { name, fields, span } = decl;
                for (ty, default_value) in fields.values() {
                    let Some(expr) = default_value else { continue };
                    self.type_check_expr(expr, ty);
                }
            }
        }
    }

    fn type_infer_struct_field(&mut self, name: String, field: String) -> Option<Type> {
        let decl = self.table.structs.get(&name);
        match decl {
            Some(decl) => match decl.fields.get(&field) {
                Some(ty) => Some(ty.0.clone()),
                None => {
                    // @empty-span
                    self.add_error(
                        Span::empty(),
                        TypeErrorKind::UnkownStrtructField {
                            field: field.clone(),
                            struct_name: decl.name.clone(),
                        },
                    );
                    None
                }
            },
            None => {
                // @empty-span
                self.add_error(Span::empty(), TypeErrorKind::StructNotDefined);
                None
            }
        }
    }

    fn type_check_call(&mut self, call: &Call) {
        let Call { name, args, span } = call;
        if name == "#print" {
            for arg in args.iter() {
                self.type_infer(arg);
            }
            return;
        }

        if name == "#println" {
            for arg in args.iter() {
                self.type_infer(arg);
            }
            return;
        }

        if name == "#stack" {
            return;
        }

        if name == "#sleep" {
            let Some(arg) = args.first() else {
                self.add_error(
                    *span,
                    TypeErrorKind::WrongArgCount {
                        name: "#sleep".to_string(),
                        expected: 1,
                        actual: 0,
                    },
                );
                return;
            };

            self.type_check_expr(arg, &Type::Int);
            return;
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
                return;
            };

            match self.type_infer(arg) {
                Some(Type::Array(elem_ty, _)) | Some(Type::Slice(elem_ty)) => {}
                Some(_) => {
                    self.add_error(*span, TypeErrorKind::LengthOfNonArray);
                }
                _ => {}
            }

            return;
        }

        let Some(ProcDecl { params, ret_ty, .. }) = self.table.procs.get(&name.clone()).cloned()
        else {
            self.add_error(*span, TypeErrorKind::ProcNotDefined);
            return;
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
            return;
        }

        for (i, arg) in args.iter().enumerate() {
            let Some((_, ty)) = params.get(i) else {
                unreachable!()
            };
            self.type_check_expr(arg, ty);
        }
    }

    fn type_infer(&mut self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Unit(_) => Some(Type::Unit),
            Expr::Lit(Lit::Bool(_, _)) => Some(Type::Bool),
            Expr::Lit(Lit::Int(_, _)) => Some(Type::Int),
            Expr::Lit(Lit::Float(_, _)) => Some(Type::Float),
            Expr::Lit(Lit::Str(_, _)) => Some(Type::Str),
            Expr::Lit(Lit::Struct(name, lit, lit_span)) => {
                let Some(decl) = self.table.structs.get(name) else {
                    self.add_error(*lit_span, TypeErrorKind::StructNotDefined);
                    return None;
                };

                // TODO: is this clone needed?
                for (field, (ty, _)) in decl.fields.clone() {
                    if let Some(expr) = lit.get(&field) {
                        self.type_check_expr(expr, &ty);
                    };
                }

                Some(Type::Struct(name.clone()))
            }
            // TODO: Check that expr is int
            Expr::MakeSlice { ty, expr, span } => Some(Type::Slice(Box::new(ty.clone()))),
            Expr::Call(call) => {
                self.type_check_call(call);
                None
            }
            Expr::Ref(expr) => self.type_infer(expr).map(|t| Type::Ref(Box::new(t))),
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
            Expr::Proj { expr, field, span } => {
                let ty = self.type_infer(expr)?;
                match ty {
                    Type::Struct(name) => self.type_infer_struct_field(name, field.clone()),
                    Type::Ref(inner_ty) => {
                        if let Type::Struct(name) = *inner_ty {
                            self.type_infer_struct_field(name, field.clone())
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
                    Type::Slice(elem_ty) => Some(*elem_ty),
                    _ => {
                        self.add_error(expr.span(), TypeErrorKind::IndexingNonArray);
                        None
                    }
                }
            }
            Expr::Var { name, span } => self.lookup(name.to_string(), *span),
            Expr::Unary { op, rhs, span } => match op {
                UnaryOp::Not => {
                    self.type_check_expr(rhs, &Type::Bool);
                    Some(Type::Bool)
                }
                UnaryOp::Minus | UnaryOp::Plus => match self.type_infer(rhs) {
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
                    // @invalid-annotations
                    self.type_check_expr(expr, ty);
                    self.define(name, ty.clone());
                }
                Stmt::Assign { lhs, rhs, span } => {
                    let Some(lhs_ty) = self.type_infer(lhs) else {
                        return false;
                    };
                    self.type_check_expr(rhs, &lhs_ty);
                }
                Stmt::Call(call) => {
                    self.type_check_call(call);
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
                    let Expr::Call(Call {
                        name: range_name,
                        args,
                        span,
                    }) = range
                    else {
                        // TODO: Error here
                        continue;
                    };

                    // TODO: Verify range name
                    match args.len() {
                        1 => {
                            self.type_check_expr(&args[0], &Type::Int);
                        }
                        2 => {
                            self.type_check_expr(&args[0], &Type::Int);
                            self.type_check_expr(&args[1], &Type::Int);
                        }
                        _ => {
                            // TODO: Eitehr 1 or 2, not just 1
                            self.add_error(
                                *span,
                                TypeErrorKind::WrongArgCount {
                                    name: "#range".to_string(),
                                    expected: 1,
                                    actual: args.len(),
                                },
                            );
                            continue;
                        }
                    }

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
            (Expr::Lit(Lit::Array(lit, span)), ty) => match ty {
                Type::Array(elem_ty, _) | Type::Slice(elem_ty) => {
                    for elem in lit.iter() {
                        self.type_check_expr(elem, elem_ty);
                    }
                }
                _ => {
                    self.add_error(
                        expr.span(),
                        TypeErrorKind::UnexpectedType {
                            expected: target.clone(),
                            // TODO: Change
                            actual: TypeSet::Text("array literal".to_string()),
                        },
                    );
                }
            },
            _ => {
                let actual_ty = self.type_infer(expr);
                // @invalid-annotations
                if let Some(inferred_ty) = actual_ty {
                    if inferred_ty != *target {
                        self.add_error(
                            expr.span(),
                            TypeErrorKind::UnexpectedType {
                                expected: target.clone(),
                                actual: TypeSet::One(inferred_ty.clone()),
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
            // @empty-span
            self.add_error(Span::empty(), TypeErrorKind::ScopeNotDefined);
            return;
        };
    }

    fn define(&mut self, name: &str, ty: Type) {
        let Some(scope) = self.scopes.last_mut() else {
            // @empty-span
            self.add_error(Span::empty(), TypeErrorKind::ScopeNotDefined);
            return;
        };

        if scope.contains_key(name) {
            // @empty-span
            self.add_error(Span::empty(), TypeErrorKind::VarAlreadyDefined);
            return;
        }
        scope.insert(name.to_string(), ty);
    }

    fn lookup(&mut self, name: String, span: Span) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(value_ref) = scope.get(&name) {
                return Some(value_ref.clone());
            }
        }

        // @empty-span
        self.add_error(span, TypeErrorKind::VarNotDefined { name: name.clone() });
        None
    }
}
