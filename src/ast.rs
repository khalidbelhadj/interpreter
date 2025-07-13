use core::{error, panic};
use std::{
    collections::HashMap,
    fmt::{write, Debug, Display},
    process::exit,
};

use crate::token::*;
use log::{debug, error};

#[derive(Debug, Clone)]
pub struct Program {
    pub top_levels: Vec<TopLevel>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TopLevel {
    StructDecl(StructDecl),
    ProcDecl(ProcDecl),
}

impl Display for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevel::StructDecl(struct_decl) => write!(f, "{struct_decl:#?}"),
            TopLevel::ProcDecl(proc_decl) => write!(f, "{proc_decl:#?}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: HashMap<String, (Type, Option<Expr>)>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ProcDecl {
    pub name: String,
    pub params: Vec<(String, Type)>,
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
    Ret {
        expr: Expr,
        span: Span,
    },
    Call(Call),
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
    Call(Call),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub name: String,
    pub args: Vec<Expr>,
    pub span: Span,
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
            UnaryOp::Minus => "minus",
            UnaryOp::Plus => "plus",
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
            Lit::Struct(n, s, _) => write!(f, "{:?}", s),
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
            Expr::Call(Call { name, args, .. }) => {
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Type::Unit => "unit".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Str => "string".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Struct(s) => s.to_string(),
            Type::Array(ty, ArrayLength::Dynamic) => format!("[{ty}]"),
            Type::Array(ty, ArrayLength::Fixed(size)) => format!("[{ty}, {size}]",),
            Type::Ref(ty) => format!("&{ty}"),
        };
        write!(f, "{string}")
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ArrayLength {
    Fixed(usize),
    Dynamic,
}

#[derive(Clone)]
pub enum Lit {
    Int(i64, Span),
    Float(f64, Span),
    Str(String, Span),
    Bool(bool, Span),
    Struct(String, HashMap<String, Expr>, Span),
    Array(Vec<Expr>, Span),
}

impl Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i, span) => write!(f, "{i}"),
            Lit::Float(fl, span) => write!(f, "{fl}"),
            Lit::Str(s, span) => write!(f, "{s}"),
            Lit::Bool(b, span) => write!(f, "{b}"),
            Lit::Struct(name, hash_map, span) => write!(f, "{hash_map:#?}"),
            Lit::Array(vec, span) => write!(f, "{vec:#?}"),
        }
    }
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
    Minus,
    Plus,
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

    pub fn span(&self) -> Span {
        match self {
            Expr::Unit(span) => *span,
            Expr::Ref(expr) => expr.span(),
            Expr::Deref(expr) => expr.span(),
            Expr::Var { name, span } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call(Call { span, .. }) => *span,
            Expr::Proj { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Lit(lit) => *lit.span(),
            Expr::MakeArray { span, .. } => *span,
        }
    }
}

// Add helper methods to get spans from AST nodes
impl Stmt {
    pub fn span(&self) -> &Span {
        match self {
            Stmt::VarDecl { span, .. } => span,
            Stmt::Assign { span, .. } => span,
            Stmt::If { span, .. } => span,
            Stmt::IfElse { span, .. } => span,
            Stmt::While { span, .. } => span,
            Stmt::For { span, .. } => span,
            Stmt::Call(Call { span, .. }) => span,
            Stmt::Ret { span, .. } => span,
        }
    }
}

impl Lit {
    pub fn span(&self) -> &Span {
        match self {
            Lit::Int(_, span) => span,
            Lit::Float(_, span) => span,
            Lit::Str(_, span) => span,
            Lit::Bool(_, span) => span,
            Lit::Struct(name, hash_map, span) => span,
            Lit::Array(exprs, span) => span,
        }
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

#[derive(Debug)]
pub enum ParseErrorKind {
    UnexpectedToken {
        expected: TokenSet,
        actual: TokenType,
    },
    NegativeArrayLength,
    NonIntLitArrayLength,
    InvalidLeftHandSide,
    InvalidReferenceTarget,
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TokenSet {
    One(TokenType),
    Many(Vec<TokenType>),
    Text(String),
}

impl Display for TokenSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenSet::One(token_type) => write!(f, "{}", token_type),
            TokenSet::Many(token_types) => todo!(),
            TokenSet::Text(text) => write!(f, "{text}"),
        }
    }
}

pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

pub enum TypeErrorKind {
    // Unexpected
    UnexpectedReturnType {
        expected: TypeSet,
        actual: Type,
    },
    UnexpectedType {
        expected: Type,
        actual: Type,
    },
    UnexpectedArrayLength {
        expected: usize,
        actual: usize,
    },

    UnexpectedArrayType {
        expected: Type,
        actual: Type,
    },

    // Definition
    ProcAlreadyDefined,
    StructAlreadyDefined,
    VarAlreadyDefined,
    ProcNotDefined,
    VarNotDefined,
    StructNotDefined,

    // Unary operation
    ProjectingNonStruct,
    ProjectingNonStructRef,
    DerefNonPointer,
    IndexingNonArray,

    // Binary operations
    ArithWithNonNumber {
        ty: Type,
    },

    // Other
    ScopeNotDefined,
    StructFieldMissingInLiteral {
        field: String,
        struct_name: String,
    },
    WrongArgCount {
        name: String,
        expected: usize,
        actual: usize,
    },
    NotAbleToInferType,
    UnreachableCodeAfterReturn,
    MissingField {
        field: String,
    },
}

pub enum TypeSet {
    One(Type),
    Text(String),
}

impl Display for TypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSet::One(inner) => write!(f, "{:?}", inner),
            TypeSet::Text(t) => write!(f, "{:?}", t),
        }
    }
}
