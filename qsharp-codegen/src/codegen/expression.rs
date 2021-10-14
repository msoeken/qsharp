use qsharp_ast::{
    ast::{Expression, ResultValue},
    utilities::Mapper,
};
use quote::{format_ident, quote};

use super::Codegen;

pub(crate) fn visit_expression(
    mapper: &mut Codegen,
    expression: &Expression,
) -> proc_macro2::TokenStream {
    expression_to_rust(expression, mapper, false, 0)
}

fn expression_to_rust(
    expr: &Expression,
    mapper: &mut Codegen,
    is_adjoint: bool,
    controlled: usize,
) -> proc_macro2::TokenStream {
    match expr {
        Expression::UnitValue => quote! { () },
        Expression::Identifier(name, type_parameters) => {
            // TODO use QualifiedName's to_rust function
            let mut name = name.join("::");
            assert!(controlled <= 1);
            if controlled == 1 {
                name += "_ctl";
            }
            if is_adjoint {
                name += "_adj";
            }
            let name = format_ident!("{}", name);

            // TODO check that these type_parameters are from a callable (to inject Sim)
            if let Some(type_parameters) = type_parameters {
                let types = type_parameters.iter().map(|t| mapper.visit_type_kind(t));
                quote! { #name::<Sim, #(#types),*> }
            } else {
                quote! { #name }
            }
        }
        Expression::IntLiteral(value) => quote! { #value },
        Expression::DoubleLiteral(value) => quote! { #value },
        Expression::BoolLiteral(value) => quote! { #value },
        Expression::StringLiteral(value) => quote! { String::from(#value) },
        Expression::ResultLiteral(result) => {
            if *result == ResultValue::One {
                quote! { true }
            } else {
                quote! { false }
            }
        }
        Expression::ArrayLiteral(exprs) => {
            let exprs = exprs.iter().map(|expr| mapper.visit_expression(expr));
            quote! { std::rc::Rc::new(vec![#(#exprs),*]) }
        }
        Expression::TupleLiteral(exprs) => {
            let exprs = exprs.iter().map(|expr| mapper.visit_expression(expr));
            quote! { (#(#exprs),*) }
        }
        Expression::PosPrefix(expr) => {
            let expr = mapper.visit_expression(expr);
            quote! { +(#expr) }
        }
        Expression::NegPrefix(expr) => {
            let expr = mapper.visit_expression(expr);
            quote! { -(#expr) }
        }
        Expression::LogicalNot(expr) => {
            let expr = mapper.visit_expression(expr);
            quote! { !(#expr) }
        }
        Expression::BitwiseNot(expr) => {
            let expr = mapper.visit_expression(expr);
            quote! { ~(#expr) }
        }
        Expression::Adjoint(expr) => expression_to_rust(expr, mapper, !is_adjoint, controlled),
        Expression::Controlled(expr) => {
            expression_to_rust(expr, mapper, is_adjoint, controlled + 1)
        }
        Expression::Range(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs..=#rhs) }
        }
        Expression::Equality(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs == #rhs) }
        }
        Expression::Inequality(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs != #rhs) }
        }
        Expression::LessThan(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs < #rhs) }
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs <= #rhs) }
        }
        Expression::GreaterThan(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs > #rhs) }
        }
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs >= #rhs) }
        }
        Expression::RightShift(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs >> #rhs) }
        }
        Expression::LeftShift(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs << #rhs) }
        }
        Expression::Addition(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs + #rhs) }
        }
        Expression::Subtraction(lhs, rhs) => {
            let lhs = mapper.visit_expression(lhs);
            let rhs = mapper.visit_expression(rhs);
            quote! { (#lhs - #rhs) }
        }
        Expression::Call(caller, args) => {
            let caller = expression_to_rust(caller, mapper, is_adjoint, controlled);

            let mut arg_inits = vec![];
            let mut arg_names = vec![];

            for (idx, arg) in args.iter().enumerate() {
                let arg_name = format_ident!("__arg__{}", idx);
                let arg = mapper.visit_expression(arg);
                arg_inits.push(quote! { let #arg_name = #arg.clone(); });
                arg_names.push(arg_name);
            }

            quote! {
                {
                    #(#arg_inits)*
                    #caller(sim__, #(#arg_names),*)
                }
            }
        }
        Expression::ArrayItem(expr, index) => {
            let expr = mapper.visit_expression(expr);
            let index = mapper.visit_array_item_index(index);

            quote! { #expr[#index] }
        }
        _ => todo!("{:?}", expr),
    }
}
