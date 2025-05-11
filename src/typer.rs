use std::collections::{HashMap, HashSet};

use crate::parser::*;

type Scope = HashMap<String, Type>;

pub struct Typer {
    pub program: Program,
    pub vars: Vec<Scope>,
    pub structs: HashMap<String, HashMap<String, Type>>,
    pub funcs: HashMap<String, (HashMap<String, Type>, Type)>,
}

impl Typer {
    pub fn new(program: Program) -> Self {
        Typer {
            program,
            vars: vec![],
            structs: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn tycheck(&mut self) {
        let top_levels = self.program.clone();
        for top_level in top_levels.iter() {
            self.tycheck_toplevel(top_level);
        }
    }

    fn tycheck_toplevel(&mut self, top_level: &TopLevel) {
        match top_level {
            TopLevel::FunDecl(FunDecl {
                name,
                params,
                ret_ty,
                body,
            }) => {
                if self.funcs.contains_key(name) {
                    panic!("Function {:?} already defined", name);
                }

                let mut param_types = HashMap::new();
                for (name, ty) in params.iter() {
                    param_types.insert(name.clone(), ty.clone());
                }

                self.funcs
                    .insert(name.clone(), (param_types.clone(), ret_ty.clone()));

                let body_tys = self.tyinfer_block(body, Some(param_types.clone()));

                assert!(body_tys.len() >= 1);

                if body_tys.len() != 1 {
                    panic!("Expected {:?}, got {:?}", ret_ty, body_tys);
                }

                let body_ty = body_tys.iter().next().expect("No return type");
                if body_ty != ret_ty {
                    panic!("Expected {:?}, got {:?}", ret_ty, body_ty);
                }
            }
            TopLevel::RecDecl(RecDecl { name, fields }) => {
                self.structs.insert(name.clone(), fields.clone());
            }
        }
    }

    fn get_type(&self, name: &str) -> Option<&Type> {
        for scope in self.vars.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }

    fn tyinfer_block(
        &mut self,
        block: &Block,
        params: Option<HashMap<String, Type>>,
    ) -> HashSet<Type> {
        self.vars.push(HashMap::new());
        let scope = self.vars.last_mut().expect("Scope not defined");

        if let Some(params) = params {
            for (name, ty) in params.iter() {
                scope.insert(name.clone(), ty.clone());
            }
        }

        let mut ret_tys = HashSet::new();
        let mut early_return = false;

        for (i, stmt) in block.statements.iter().enumerate() {
            match stmt {
                Stmt::VarDecl(name, ty, expr) => {
                    self.tycheck_expr(expr, ty);

                    if let Some(_) = self.get_type(name) {
                        panic!("Variable {:?} already defined", name);
                    }

                    let scope = self.vars.last_mut().expect("Scope not defined");
                    scope.insert(name.clone(), ty.clone());
                }
                Stmt::Assign(target, expr) => match target {
                    AssignTarget::Var(name) => {
                        let ty = self.get_type(name).cloned();
                        match ty {
                            Some(t) => {
                                self.tycheck_expr(expr, &t);
                            }
                            None => panic!("Variable {:?} not defined", name),
                        }
                    }
                    AssignTarget::Proj(rec, label) => {
                        let rec_ty = self.get_type(rec).expect("Variable not defined");
                        if let Type::Struct(name) = rec_ty {
                            let field_ty = self
                                .structs
                                .get(name)
                                .expect("Struct not defined")
                                .get(label)
                                .expect("Field not found")
                                .clone();
                            self.tycheck_expr(expr, &field_ty);
                        } else {
                            panic!("Expected struct, got {:?}", rec_ty);
                        }
                    }
                    AssignTarget::Index(elems, index) => {
                        let array_ty = self.get_type(elems).expect("Variable not defined").clone();
                        if let Type::Array(ty, _) = array_ty {
                            self.tycheck_expr(index, &Type::Int);
                            self.tycheck_expr(expr, &ty);
                        } else {
                            panic!("Expected array, got {:?}", array_ty);
                        }
                    }
                },
                Stmt::Call(name, args) => {
                    if name == "#print" {
                        continue;
                    }

                    if name == "#length" {
                        let arg = args.get(0).expect("Argument not found");
                        self.tycheck_expr(
                            arg,
                            &Type::Array(Box::new(Type::Int), ArrayLength::Dynamic),
                        );
                    }

                    let (params, _) = self
                        .funcs
                        .get(name)
                        .expect(format!("Function {} not defined", name).as_str())
                        .clone();

                    // iterate over params and check that args match
                    for (i, arg) in args.iter().enumerate() {
                        let (_, ty) = params.iter().nth(i).expect("Param not found");
                        self.tycheck_expr(arg, ty);
                    }
                }
                Stmt::If(expr, block) => {
                    self.tycheck_expr(expr, &Type::Bool);
                    let block_tys = self.tyinfer_block(block, None);
                    ret_tys.extend(block_tys);
                }
                Stmt::IfElse(expr, if_block, else_block) => {
                    self.tycheck_expr(expr, &Type::Bool);

                    let if_tys = self.tyinfer_block(if_block, None);

                    let else_tys = self.tyinfer_block(else_block, None);

                    if if_block.returns && else_block.returns {
                        ret_tys.extend(if_tys);
                        ret_tys.extend(else_tys);
                        early_return = true;

                        if i != block.statements.len() - 1 {
                            panic!("Unreachable code");
                        }

                        break;
                    }

                    if if_block.returns {
                        ret_tys.extend(if_tys);
                    }

                    if else_block.returns {
                        ret_tys.extend(else_tys);
                    }
                }
                Stmt::While(cond, block) => {
                    self.tycheck_expr(cond, &Type::Bool);
                    let block_tys = self.tyinfer_block(block, None);

                    if block.returns {
                        ret_tys.extend(block_tys);
                    }
                }
                Stmt::For(var, from, to, block) => {
                    self.tycheck_expr(from, &Type::Int);
                    self.tycheck_expr(to, &Type::Int);

                    let scope = self.vars.last_mut().expect("Scope not defined");
                    scope.insert(var.clone(), Type::Int);

                    let block_tys = self.tyinfer_block(block, None);

                    if block.returns {
                        ret_tys.extend(block_tys);
                    }
                }
                Stmt::Ret(expr) => {
                    let ty = self.tyinfer(expr);
                    ret_tys.insert(ty);
                    early_return = true;
                    if i != block.statements.len() - 1 {
                        panic!("Unreachable code");
                    }

                    break;
                }
            }
        }

        if !early_return {
            ret_tys.insert(Type::Unit);
        }

        self.vars.pop();
        return ret_tys;
    }

    fn tyinfer(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Unit => Type::Unit,
            Expr::Lit(Lit::Bool(_)) => Type::Bool,
            Expr::Lit(Lit::Int(_)) => Type::Int,
            Expr::Lit(Lit::Str(_)) => Type::Str,
            Expr::Var(x) => match self.get_type(x) {
                Some(t) => t.clone(),
                None => panic!("Variable {:?} not defined", x),
            },
            Expr::Bin(e1, BinOp::Add | BinOp::Sub | BinOp::Div | BinOp::Mul, e2) => {
                self.tycheck_expr(e1, &Type::Int);
                self.tycheck_expr(e2, &Type::Int);
                Type::Int
            }
            Expr::Bin(e1, BinOp::And | BinOp::Or, e2) => {
                let e1_ty = self.tyinfer(e1);
                self.tycheck_expr(e2, &e1_ty);
                Type::Bool
            }

            Expr::Bin(e1, op, e2) => {
                if op.is_arithmetic() {
                    self.tycheck_expr(e1, &Type::Int);
                    self.tycheck_expr(e2, &Type::Int);
                    return Type::Int;
                }

                if op.is_comparison() {
                    self.tycheck_expr(e1, &Type::Int);
                    self.tycheck_expr(e2, &Type::Int);
                    return Type::Bool;
                }

                if op.is_logical() {
                    self.tycheck_expr(e1, &Type::Bool);
                    self.tycheck_expr(e2, &Type::Bool);
                    return Type::Bool;
                }

                panic!("Type mismatch");
            }
            // Expr::Un(un_op, expr) => todo!(),
            // Expr::Call(_, vec) => todo!(),
            _ => todo!(),
        }
    }

    fn tycheck_expr(&mut self, expr: &Expr, target: &Type) {
        match (expr, target) {
            (Expr::Unit, Type::Unit) => {}
            (Expr::Lit(Lit::Bool(_)), Type::Bool) => {}
            (Expr::Lit(Lit::Int(_)), Type::Int) => {}
            (Expr::Lit(Lit::Str(_)), Type::Str) => {}
            (Expr::Bin(e1, op, e2), ty) => {
                if op.is_arithmetic() && ty == &Type::Int {
                    self.tycheck_expr(e1, &Type::Int);
                    self.tycheck_expr(e2, &Type::Int);
                    return;
                }

                if op.is_comparison() && ty == &Type::Bool {
                    self.tycheck_expr(e1, &Type::Int);
                    self.tycheck_expr(e2, &Type::Int);
                    return;
                }

                if op.is_logical() && ty == &Type::Bool {
                    self.tycheck_expr(e1, &Type::Bool);
                    self.tycheck_expr(e2, &Type::Bool);
                    return;
                }

                panic!("Type mismatch");
            }
            (Expr::Var(x), Type::Array(elem_ty, size)) => {
                match self.get_type(x) {
                    Some(Type::Array(ty, s)) => {
                        if ty != elem_ty {
                            panic!("Expected {:?}, got {:?}", elem_ty, ty);
                        }
                        match (size, s) {
                            (ArrayLength::Dynamic, ArrayLength::Dynamic) => {
                                // do nothing
                            }
                            (ArrayLength::Dynamic, ArrayLength::Fixed(s)) => {
                                // do nothing
                            }
                            (ArrayLength::Fixed(s1), ArrayLength::Fixed(s2)) => {
                                if s1 != s2 {
                                    panic!(
                                        "Expected array of length {:?}, got length {:?}",
                                        s1, s2
                                    );
                                }
                            }
                            _ => panic!("Expected dynamic array, got {:?}", size),
                        }
                    }
                    _ => panic!("Expected array, got {:?}", x),
                }
            }
            (Expr::Var(x), ty) => match self.get_type(x) {
                Some(t) => {
                    if t != ty {
                        panic!("Type mismatch");
                    }
                }
                None => panic!("Variable {:?} not defined", x),
            },
            (Expr::Lit(Lit::Struct(lit)), Type::Struct(name)) => {
                let fields = self.structs.get(name).expect("Struct not defined").clone();
                for (field, ty) in fields.iter() {
                    let value = lit.get(field).expect("Field not found");
                    self.tycheck_expr(value, ty);
                }
            }

            (Expr::Call(func, args), ty) => {
                let (params, ret_ty) = self.funcs.get(func).expect("Function not defined").clone();

                // iterate over params and check that args match
                for (i, arg) in args.iter().enumerate() {
                    let (_, ty) = params
                        .iter()
                        .nth(i)
                        .expect("Missing parameter in function call");
                    self.tycheck_expr(arg, ty);
                }

                if ret_ty != *ty {
                    panic!("Expected {:?}, got {:?}", ty, ret_ty);
                }
            }
            (Expr::Proj(rec, field), ty) => match self.get_type(rec) {
                Some(Type::Struct(name)) => {
                    let fields = self.structs.get(name).expect("Struct not defined").clone();
                    let field_ty = fields.get(field).expect("Field not found");
                    if field_ty != ty {
                        panic!("Expected {:?}, got {:?}", ty, field_ty);
                    }
                }
                Some(Type::Array(elem_ty, size)) => {
                    if field != "len" {
                        panic!("Expected len, got {:?}", field);
                    }

                    if ty != &Type::Int {
                        panic!("Expected Int, got {:?}", ty);
                    }
                }
                _ => panic!("Expected struct, got {:?}", rec),
            },
            (Expr::Lit(Lit::Array(lit)), Type::Array(ty, size)) => {
                match size {
                    ArrayLength::Dynamic => {}
                    ArrayLength::Fixed(s) => {
                        if s != &lit.len() {
                            panic!(
                                "Expected array of length {:?}, got length {:?}",
                                s,
                                lit.len()
                            );
                        }
                    }
                }

                for elem in lit.iter() {
                    self.tycheck_expr(elem, ty);
                }
            }
            (Expr::Index(var, idx), ty) => {
                let var_ty = self.get_type(var).expect("Variable not defined");
                match var_ty {
                    Type::Array(elem_ty, _) => {
                        if elem_ty.as_ref() != ty {
                            panic!("Expected {:?}, got {:?}", ty, elem_ty);
                        }
                    }
                    _ => panic!("Expected array, got {:?}", var_ty),
                }

                self.tycheck_expr(idx, &Type::Int);
            }
            _ => panic!("Expected {:?}, got expression {:?}", target, expr),
        };
    }
}
