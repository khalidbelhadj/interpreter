use std::fmt::format;

use crate::ast::*;

fn type_to_ctype(ty: &Type) -> String {
    match ty {
        Type::Unit => "void".to_string(),
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Str => "string_t".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Struct(name) => format!("struct {name}"),
        Type::Array(ty, _) => format!("{}*", type_to_ctype(ty)),
        Type::Slice(ty) => format!("{}*", type_to_ctype(ty)),
        Type::Ref(ty) => format!("{}*", type_to_ctype(ty)),
    }
}

pub fn generate_c_code(table: SymbolTable) -> String {
    let mut output = String::new();

    for (_, decl) in table.structs {
        let StructDecl { name, fields, span } = decl;

        output.push_str(format!("typedef struct {name} {{\n").as_str());
        {
            for (field, (ty, _)) in fields {
                output.push_str(format!("{} {field};\n", type_to_ctype(&ty)).as_str());
            }
        }
        output.push_str(format!("}} {name};\n").as_str());
    }

    for (_, decl) in table.procs {
        let ProcDecl {
            name,
            params,
            ret_ty,
            ret_ty_span,
            block,
            span,
        } = decl;

        output.push_str(format!("{} {}(", type_to_ctype(&ret_ty), name).as_str());

        for (name, ty, _) in params {
            output.push_str(format!("{} {}, ", name, type_to_ctype(&ty)).as_str());
        }
        output.push_str(format!(") {{\n").as_str());
        {
            for stmt in block.statements {
                match stmt {
                    Stmt::VarDecl {
                        name,
                        ty,
                        expr,
                        span,
                    } => {
                        output.push_str(
                            format!("{} {} = {};\n", type_to_ctype(&ty), name, expr).as_str(),
                        );
                    }
                    Stmt::Assign { lhs, rhs, span } => todo!(),
                    Stmt::If {
                        cond,
                        then_block,
                        span,
                    } => todo!(),
                    Stmt::IfElse {
                        cond,
                        then_block,
                        else_block,
                        span,
                    } => todo!(),
                    Stmt::While { cond, block, span } => todo!(),
                    Stmt::For {
                        name,
                        range,
                        block,
                        span,
                    } => todo!(),
                    Stmt::Ret { expr, span } => todo!(),
                    Stmt::Call(call) => todo!(),
                }
            }
        }
        output.push_str(format!("}}\n").as_str());
    }

    format!(
        "
#include <stdio.h>

{output}
",
    )
}
