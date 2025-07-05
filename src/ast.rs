use core::{error, panic};
use std::{collections::HashMap, fmt::Display, process::exit};

use crate::token::*;
use log::{debug, error};

pub type Program = Vec<TopLevel>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopLevel {
    StructDecl(StructDecl),
    ProcDecl(ProcDecl),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: HashMap<String, Type>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ProcDecl {
    pub name: String,
    pub params: HashMap<String, Type>,
    pub ret_ty: Type,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    VarDecl {
        name: String,
        ty: Type,
        expr: Expr,
        span: Span,
    },
    Assign {
        lhs: Expr,
        rhs: Expr,
        span: Span,
    },
    If {
        cond: Expr,
        then_block: Block,
        span: Span,
    },
    IfElse {
        cond: Expr,
        then_block: Block,
        else_block: Block,
        span: Span,
    },
    While {
        cond: Expr,
        block: Block,
        span: Span,
    },
    For {
        name: String,
        range: Expr,
        block: Block,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    Ret {
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Unit(Span),
    Lit(Lit),
    Var {
        name: String,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
        span: Span,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    MakeArray {
        ty: Type,
        expr: Box<Expr>,
        span: Span,
    },
    Proj {
        expr: Box<Expr>,
        field: String,
        span: Span,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Ref(Box<Expr>),
    Deref(Box<Expr>),
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Leq => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Geq => ">=",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
        };
        write!(f, "{}", op_str)
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            UnaryOp::Not => "not",
        };
        write!(f, "{}", op_str)
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(n, _) => write!(f, "{}", n),
            Lit::Float(n, _) => write!(f, "{}", n),
            Lit::Str(s, _) => write!(f, "\"{}\"", s),
            Lit::Bool(b, _) => write!(f, "{}", b),
            Lit::Struct(s, _) => write!(f, "{:?}", s),
            Lit::Array(a, _) => write!(f, "{:?}", a),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Unit(_) => write!(f, "()"),
            Expr::Lit(lit) => write!(f, "{}", lit),
            Expr::Var { name, .. } => write!(f, "{}", name),
            Expr::Binary { lhs, op, rhs, .. } => {
                write!(f, "({} {} {})", lhs, op, rhs)
            }
            Expr::Unary { op, rhs, span } => {
                write!(f, "{}{}", op, rhs)
            }
            Expr::Call { name, args, .. } => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Proj { expr, field, .. } => write!(f, "{}.{}", expr, field),
            Expr::Index { expr, index, .. } => write!(f, "{}[{}]", expr, index),
            Expr::Ref(expr) => write!(f, "&{}", expr),
            Expr::Deref(expr) => write!(f, "*{}", expr),
            Expr::MakeArray { ty, expr, span } => todo!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Unit,
    Int,
    Float,
    Str,
    Bool,
    Struct(String),
    Array(Box<Type>, ArrayLength),
    Ref(Box<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ArrayLength {
    Fixed(usize),
    Dynamic,
}

#[derive(Debug, Clone)]
pub enum Lit {
    Int(i64, Span),
    Float(f64, Span),
    Str(String, Span),
    Bool(bool, Span),
    Struct(HashMap<String, Expr>, Span),
    Array(Vec<Expr>, Span),
}

impl PartialEq for Lit {
    fn eq(&self, other: &Lit) -> bool {
        match (self, other) {
            (Lit::Float(f1, _), Lit::Float(f2, _)) => f1 == f2,
            (x, y) => x == y,
        }
    }
}
impl Eq for Lit {}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,

    And,
    Or,
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            Expr::Var { name: _, span: _ }
                | Expr::Proj {
                    expr: _,
                    field: _,
                    span: _
                }
                | Expr::Index {
                    expr: _,
                    index: _,
                    span: _
                }
                | Expr::Deref(_)
        )
    }

    pub fn is_rvalue(&self) -> bool {
        !self.is_lvalue()
    }
}

impl BinaryOp {
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div
        )
    }

    pub fn is_logical(&self) -> bool {
        matches!(self, BinaryOp::And | BinaryOp::Or)
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Lt
                | BinaryOp::Leq
                | BinaryOp::Gt
                | BinaryOp::Geq
        )
    }
}
