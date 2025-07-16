use std::fmt::Display;

use crate::{ast::*, token::*};

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
        actual: TypeSet,
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
    VarNotDefined {
        name: String,
    },
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
    UnkownStrtructField {
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
    LengthOfNonArray,
}

pub enum TypeSet {
    One(Type),
    Text(String),
}

impl Display for TypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSet::One(inner) => write!(f, "{}", inner),
            TypeSet::Text(t) => write!(f, "{}", t),
        }
    }
}
