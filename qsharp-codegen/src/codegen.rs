use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::Punct;
use qsharp_ast::ast::{
    Access, ArrayItemIndex, Callable, CallableBody, Expression, Namespace, NamespaceItem,
    Parameter, Program, QualifiedName, QubitAllocationKind, QubitInitializer, ResultValue, Scope,
    SpecializationGenerator, SpecializationKind, SpecializationParameter, Statement, SymbolBinding,
    TypeKind,
};
use quote::{format_ident, quote, TokenStreamExt};

mod parameter;
mod symbol_binding;

use crate::{
    codegen::{parameter::unzip_parameters, symbol_binding::flatten_bindings},
    traits::ToRust,
};

impl ToRust for QualifiedName {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self.namespace() {
            Some(namespace) => {
                let mut tokenstream = proc_macro2::TokenStream::new();

                for item in namespace {
                    tokenstream.append(format_ident!("{}", item));
                    tokenstream.append(Punct::new(':', proc_macro2::Spacing::Joint));
                    tokenstream.append(Punct::new(':', proc_macro2::Spacing::Joint));
                }
                tokenstream.append(format_ident!("{}", self.name()));

                tokenstream
            }
            None => {
                let name = format_ident!("{}", self.name());
                quote! { #name }
            }
        }
    }
}

impl ToRust for ArrayItemIndex {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            ArrayItemIndex::Simple(expr) => {
                let expr = expr.to_rust();
                quote! { #expr as usize }
            }
            _ => todo!(),
        }
    }
}

impl ToRust for Callable {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        // TODO incomplete!

        // at this point we ignore self.characteristics() and assume that it has
        // been correctly resolved with semantic analysis.  Also we expect that
        // all specializations are given explicitly and we do not have a
        // callable with a simple body.

        if self.type_parameters().is_some() {
            todo!();
        }

        let prefix = self.prefix();
        let return_type = self.return_type().to_rust();
        let modifier = match prefix.access() {
            Access::Internal => quote! {},
            Access::Default => quote! { pub },
        };
        let parameters = self.parameters().to_rust();

        if let CallableBody::Multiple(specializations) = self.body() {
            let mut code = quote! {};

            for specialization in specializations {
                if let SpecializationGenerator::Provided(args, scope) = specialization.generator() {
                    match specialization.kind() {
                        SpecializationKind::Body => {
                            let name = format_ident!("{}", self.name());
                            let body = scope.to_rust();

                            code = quote! {
                                #code
                                #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #parameters) -> #return_type #body
                            }
                        }
                        SpecializationKind::Adjoint => {
                            let name = format_ident!("{}_adj", self.name());
                            let body = scope.to_rust();

                            code = quote! {
                                #code
                                #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #parameters) -> #return_type #body
                            }
                        }
                        SpecializationKind::Controlled => {
                            // get parameter name for controls from args (normalization guarantees that it exists)
                            let ctls_name = match args {
                                Some(vec) => match &vec[0] {
                                    SpecializationParameter::Identifier(name) => {
                                        format_ident!("{}", name)
                                    }
                                    _ => unreachable!(),
                                },
                                _ => unreachable!(),
                            };

                            let name = format_ident!("{}_ctl", self.name());
                            let body = scope.to_rust();
                            let qubit_array_type = TypeKind::array(TypeKind::qubit()).to_rust();
                            let parameters = generate_parameters_directly(self.parameters());

                            code = quote! {
                                #code
                                #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #ctls_name: #qubit_array_type, #parameters) -> #return_type #body
                            }
                        }
                        SpecializationKind::ControlledAdjoint => {
                            // get parameter name for controls from args (normalization guarantees that it exists)
                            let ctls_name = match args {
                                Some(vec) => match &vec[0] {
                                    SpecializationParameter::Identifier(name) => {
                                        format_ident!("{}", name)
                                    }
                                    _ => unreachable!(),
                                },
                                _ => unreachable!(),
                            };

                            let name = format_ident!("{}_ctl_adj", self.name());
                            let body = scope.to_rust();
                            let qubit_array_type = TypeKind::array(TypeKind::qubit()).to_rust();
                            let parameters = generate_parameters_directly(self.parameters());

                            code = quote! {
                                #code
                                #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #ctls_name: #qubit_array_type, #parameters) -> #return_type #body
                            }
                        }
                    }
                } else {
                    // all specializations should be resolved at this point
                    unreachable!();
                }
            }
            code
        } else {
            // TODO this should be unreachable after semantic analysis is complete
            let name = format_ident!("{}", self.name());
            let body = self.body().to_rust();

            quote! {
                #modifier fn #name(#parameters) -> #return_type #body
            }
        }
    }
}

impl ToRust for CallableBody {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            CallableBody::Simple(scope) => scope.to_rust(),
            _ => todo!(),
        }
    }
}

fn expression_to_rust(
    expr: &Expression,
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
                let types = type_parameters.iter().map(|t| t.to_rust());
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
            let exprs = exprs.iter().map(|expr| expr.to_rust());
            quote! { std::rc::Rc::new(vec![#(#exprs),*]) }
        }
        Expression::TupleLiteral(exprs) => {
            let exprs = exprs.iter().map(|expr| expr.to_rust());
            quote! { (#(#exprs),*) }
        }
        Expression::PosPrefix(expr) => {
            let expr = expr.to_rust();
            quote! { +(#expr) }
        }
        Expression::NegPrefix(expr) => {
            let expr = expr.to_rust();
            quote! { -(#expr) }
        }
        Expression::LogicalNot(expr) => {
            let expr = expr.to_rust();
            quote! { !(#expr) }
        }
        Expression::BitwiseNot(expr) => {
            let expr = expr.to_rust();
            quote! { ~(#expr) }
        }
        Expression::Adjoint(expr) => expression_to_rust(expr, !is_adjoint, controlled),
        Expression::Controlled(expr) => expression_to_rust(expr, is_adjoint, controlled + 1),
        Expression::Equality(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs == #rhs) }
        }
        Expression::Inequality(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs != #rhs) }
        }
        Expression::LessThan(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs < #rhs) }
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs <= #rhs) }
        }
        Expression::GreaterThan(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs > #rhs) }
        }
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs >= #rhs) }
        }
        Expression::RightShift(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs >> #rhs) }
        }
        Expression::LeftShift(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs << #rhs) }
        }
        Expression::Addition(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs + #rhs) }
        }
        Expression::Subtraction(lhs, rhs) => {
            let lhs = lhs.to_rust();
            let rhs = rhs.to_rust();
            quote! { (#lhs - #rhs) }
        }
        Expression::Call(caller, args) => {
            let caller = expression_to_rust(caller, is_adjoint, controlled);

            let mut arg_inits = vec![];
            let mut arg_names = vec![];

            for (idx, arg) in args.iter().enumerate() {
                let arg_name = format_ident!("__arg__{}", idx);
                let arg = arg.to_rust();
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
            let expr = expr.to_rust();
            let index = index.to_rust();

            quote! { #expr[#index] }
        }
        _ => todo!("{:?}", expr),
    }
}

impl ToRust for Expression {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        expression_to_rust(self, false, 0)
    }
}

impl ToRust for Namespace {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        let name = self.name().to_rust();
        let items = self.items().iter().map(|item| item.to_rust());

        quote! {
            #[allow(non_snake_case, unused_variables, dead_code, clippy::clone_on_copy, clippy::redundant_clone, clippy::unused_unit)]
            mod #name {
                use crate::*;
                use qsharp_runtime::*;
                use Microsoft::Quantum::Core::*;

                #(#items)*
            }
        }
    }
}

impl ToRust for NamespaceItem {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            NamespaceItem::OpenDirective(name, alias) => {
                // TODO
                assert!(alias.is_none());

                let name = name.to_rust();

                quote! { use #name::*; }
            }
            NamespaceItem::Callable(callable) => callable.to_rust(),
            _ => todo!(),
        }
    }
}

fn generate_parameters_directly(parameter: &Rc<Parameter>) -> proc_macro2::TokenStream {
    let (names, kind) = unzip_parameters(parameter);
    quote! { #names: #kind }
}

impl ToRust for Parameter {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            Parameter::Item(name, kind) => {
                let name = format_ident!("{}", name);
                let kind = kind.to_rust();
                quote! { #name: #kind }
            }
            Parameter::Tuple(items) => {
                // TODO use generate_parameters_directly here
                let items = items.iter().map(|item| {
                    let (names, kind) = unzip_parameters(item);
                    quote! { #names: #kind }
                });

                quote! { #(#items),* }
            }
        }
    }
}

impl ToRust for Program {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        let namespaces = self.namespaces().iter().map(|ns| ns.to_rust());

        quote! {
            #(#namespaces)*
        }
    }
}

impl ToRust for Scope {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        let statements = self
            .statements()
            .iter()
            .map(|stmt| stmt.to_rust())
            .collect_vec();

        quote! {
            {
                #(#statements)*
            }
        }
    }
}

impl ToRust for Statement {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            Statement::Let(binding, expr) => {
                let binding = binding.to_rust();
                let expr = expr.to_rust();
                quote! { let #binding = #expr; }
            }
            Statement::Return(expr) => {
                let expr = expr.to_rust();
                quote! { return #expr; }
            }
            Statement::Expression(expr) => {
                let expr = expr.to_rust();
                quote! { #expr; }
            }
            Statement::QubitAllocation(
                QubitAllocationKind::Use,
                binding,
                initializer,
                Some(scope),
            ) => {
                let flattened = flatten_bindings(binding, initializer);

                let allocate = flattened.iter().map(|(name, init)| {
                    let ident = format_ident!("{}", name);
                    match init {
                        QubitInitializer::Single => quote! { let #ident = sim__.allocate(); },
                        QubitInitializer::Register(expr) => {
                            let expr = expr.to_rust();
                            quote! { let #ident = sim__.allocate_many(#expr as usize); }
                        }
                        _ => unreachable!(),
                    }
                });

                let release = flattened.iter().map(|(name, init)| {
                    let ident = format_ident!("{}", name);
                    match init {
                        QubitInitializer::Single => quote! { sim__.release(#ident); },
                        QubitInitializer::Register(_) => quote! { sim__.release_many(#ident); },

                        _ => unreachable!(),
                    }
                });

                let statements = scope
                    .statements()
                    .iter()
                    .map(|stmt| stmt.to_rust())
                    .collect_vec();

                quote! {
                    {
                        #(#allocate)*
                        #(#statements)*
                        #(#release)*
                    }
                }
            }
            Statement::Conditional(ifs, default) => {
                let mut code = quote! {};

                let (condition, scope) = &ifs[0];
                let condition = condition.to_rust();
                let scope = scope.to_rust();

                code = quote! {
                    #code

                    if #condition #scope
                };

                for (condition, scope) in ifs.iter().skip(1) {
                    let condition = condition.to_rust();
                    let scope = scope.to_rust();

                    code = quote! {
                        #code

                        else if #condition #scope
                    };
                }

                if let Some(scope) = default {
                    let scope = scope.to_rust();

                    code = quote! {
                        #code

                        else #scope
                    };
                }

                code
            }
            Statement::WithinApply(within_scope, apply_scope) => {
                let within_scope_pre = within_scope.to_rust();
                let apply_scope = apply_scope.to_rust();
                let within_scope_post = within_scope.clone().adjoint().to_rust();

                quote! {
                    #within_scope_pre
                    #apply_scope
                    #within_scope_post
                }
            }
            _ => todo!("cannot generate code for statement {:?}", self),
        }
    }
}

impl ToRust for SymbolBinding {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            SymbolBinding::Discard => quote! { _ },
            SymbolBinding::Identifier(name) => {
                let name = format_ident!("{}", name);
                quote! { #name }
            }
            _ => todo!(),
        }
    }
}

impl ToRust for TypeKind {
    fn to_rust(&self) -> proc_macro2::TokenStream {
        match self {
            TypeKind::BigInt => todo!(),
            TypeKind::Bool => quote! { bool },
            TypeKind::Double => quote! { f64 },
            TypeKind::Int => quote! { i64 },
            TypeKind::Pauli => quote! { u8 },
            TypeKind::Qubit => quote! { usize },
            TypeKind::Range => todo!(),
            TypeKind::Result => quote! { bool },
            TypeKind::String => quote! { String },
            TypeKind::Unit => quote! { () },
            TypeKind::Array(elem) => {
                let elem = elem.to_rust();
                quote! { std::rc::Rc<Vec<#elem>> }
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_expression(str: &str) -> Result<Expression> {
        syn::parse_str(str)
    }

    #[test]
    fn test_binary_operations() -> Result<()> {
        assert_eq!(
            parse_expression("1 + 3")?.to_rust().to_string(),
            "(1i64 + 3i64)"
        );
        assert_eq!(parse_expression("a - b")?.to_rust().to_string(), "(a - b)");

        Ok(())
    }
}
