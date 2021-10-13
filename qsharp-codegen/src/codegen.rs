use std::{collections::HashMap, rc::Rc};

use itertools::Itertools;
use proc_macro2::Punct;
use qsharp_ast::{
    analysis::type_from_expression,
    ast::{
        Access, ArrayItemIndex, Callable, CallableBody, Expression, Namespace, NamespaceItem,
        Parameter, Program, QualifiedName, QubitAllocationKind, QubitInitializer, ResultValue,
        Scope, SpecializationGenerator, SpecializationKind, SpecializationParameter, Statement,
        SymbolBinding, TypeKind, UpdateOperator,
    },
};
use quote::{format_ident, quote, TokenStreamExt};

mod parameter;
mod symbol_binding;

use crate::{
    codegen::{parameter::unzip_parameters, symbol_binding::flatten_bindings},
    traits::ToRust,
};

impl ToRust for QualifiedName {
    fn translate(
        &self,
        _symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
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
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        match self {
            ArrayItemIndex::Simple(expr) => {
                let expr = expr.translate(symbol_table);
                quote! { #expr as usize }
            }
            _ => todo!(),
        }
    }
}

impl ToRust for Callable {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        // TODO incomplete!

        // at this point we ignore self.characteristics() and assume that it has
        // been correctly resolved with semantic analysis.  Also we expect that
        // all specializations are given explicitly and we do not have a
        // callable with a simple body.

        if self.type_parameters().is_some() {
            todo!();
        }

        let prefix = self.prefix();
        let return_type = self.return_type().translate(symbol_table);
        let modifier = match prefix.access() {
            Access::Internal => quote! {},
            Access::Default => quote! { pub },
        };
        let parameters = self.parameters().translate(symbol_table);

        if let CallableBody::Multiple(specializations) = self.body() {
            let mut code = quote! {};

            for specialization in specializations {
                if let SpecializationGenerator::Provided(args, scope) = specialization.generator() {
                    match specialization.kind() {
                        SpecializationKind::Body => {
                            let name = format_ident!("{}", self.name());
                            let body = scope.translate(symbol_table);

                            code = quote! {
                                #code
                                #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #parameters) -> #return_type #body
                            }
                        }
                        SpecializationKind::Adjoint => {
                            let name = format_ident!("{}_adj", self.name());
                            let body = scope.translate(symbol_table);

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
                            let body = scope.translate(symbol_table);
                            let qubit_array_type =
                                TypeKind::array(TypeKind::qubit()).translate(symbol_table);
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
                            let body = scope.translate(symbol_table);
                            let qubit_array_type =
                                TypeKind::array(TypeKind::qubit()).translate(symbol_table);
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
            let body = self.body().translate(symbol_table);

            quote! {
                #modifier fn #name(#parameters) -> #return_type #body
            }
        }
    }
}

impl ToRust for CallableBody {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        match self {
            CallableBody::Simple(scope) => scope.translate(symbol_table),
            _ => todo!(),
        }
    }
}

fn expression_to_rust(
    expr: &Expression,
    symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
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
                let types = type_parameters.iter().map(|t| t.translate(symbol_table));
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
            let exprs = exprs.iter().map(|expr| expr.translate(symbol_table));
            quote! { std::rc::Rc::new(vec![#(#exprs),*]) }
        }
        Expression::TupleLiteral(exprs) => {
            let exprs = exprs.iter().map(|expr| expr.translate(symbol_table));
            quote! { (#(#exprs),*) }
        }
        Expression::PosPrefix(expr) => {
            let expr = expr.translate(symbol_table);
            quote! { +(#expr) }
        }
        Expression::NegPrefix(expr) => {
            let expr = expr.translate(symbol_table);
            quote! { -(#expr) }
        }
        Expression::LogicalNot(expr) => {
            let expr = expr.translate(symbol_table);
            quote! { !(#expr) }
        }
        Expression::BitwiseNot(expr) => {
            let expr = expr.translate(symbol_table);
            quote! { ~(#expr) }
        }
        Expression::Adjoint(expr) => {
            expression_to_rust(expr, symbol_table, !is_adjoint, controlled)
        }
        Expression::Controlled(expr) => {
            expression_to_rust(expr, symbol_table, is_adjoint, controlled + 1)
        }
        Expression::Range(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs..=#rhs) }
        }
        Expression::Equality(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs == #rhs) }
        }
        Expression::Inequality(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs != #rhs) }
        }
        Expression::LessThan(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs < #rhs) }
        }
        Expression::LessThanOrEqual(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs <= #rhs) }
        }
        Expression::GreaterThan(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs > #rhs) }
        }
        Expression::GreaterThanOrEqual(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs >= #rhs) }
        }
        Expression::RightShift(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs >> #rhs) }
        }
        Expression::LeftShift(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs << #rhs) }
        }
        Expression::Addition(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs + #rhs) }
        }
        Expression::Subtraction(lhs, rhs) => {
            let lhs = lhs.translate(symbol_table);
            let rhs = rhs.translate(symbol_table);
            quote! { (#lhs - #rhs) }
        }
        Expression::Call(caller, args) => {
            let caller = expression_to_rust(caller, symbol_table, is_adjoint, controlled);

            let mut arg_inits = vec![];
            let mut arg_names = vec![];

            for (idx, arg) in args.iter().enumerate() {
                let arg_name = format_ident!("__arg__{}", idx);
                let arg = arg.translate(symbol_table);
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
            let expr = expr.translate(symbol_table);
            let index = index.translate(symbol_table);

            quote! { #expr[#index] }
        }
        _ => todo!("{:?}", expr),
    }
}

impl ToRust for Expression {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        expression_to_rust(self, symbol_table, false, 0)
    }
}

impl ToRust for Namespace {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        let name = self.name().translate(symbol_table);
        let items = self.items().iter().map(|item| item.translate(symbol_table));

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
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        match self {
            NamespaceItem::OpenDirective(name, alias) => {
                // TODO
                assert!(alias.is_none());

                let name = name.translate(symbol_table);

                quote! { use #name::*; }
            }
            NamespaceItem::Callable(callable) => callable.translate(symbol_table),
            _ => todo!(),
        }
    }
}

fn generate_parameters_directly(parameter: &Rc<Parameter>) -> proc_macro2::TokenStream {
    let (names, kind) = unzip_parameters(parameter);
    quote! { #names: #kind }
}

impl ToRust for Parameter {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        match self {
            Parameter::Item(name, kind) => {
                let name = format_ident!("{}", name);
                let kind = kind.translate(symbol_table);
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
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        let namespaces = self
            .namespaces()
            .iter()
            .map(|ns| ns.translate(symbol_table));

        quote! {
            #(#namespaces)*
        }
    }
}

impl ToRust for Scope {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        let statements = self
            .statements()
            .iter()
            .map(|stmt| stmt.translate(symbol_table))
            .collect_vec();

        quote! {
            {
                #(#statements)*
            }
        }
    }
}

impl ToRust for Statement {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        match self {
            Statement::Let(binding, expr) => {
                let binding = binding.translate(symbol_table);
                let expr = expr.translate(symbol_table);
                quote! { let #binding = #expr; }
            }
            Statement::Return(expr) => {
                let expr = expr.translate(symbol_table);
                quote! { return #expr; }
            }
            Statement::Expression(expr) => {
                let expr = expr.translate(symbol_table);
                quote! { #expr; }
            }
            Statement::Mutable(binding, expr) => {
                let binding = binding.translate(symbol_table);
                let expr = expr.translate(symbol_table);
                quote! { let mut #binding = #expr; }
            }
            Statement::Update(name, UpdateOperator::Addition, expr) => {
                let name = format_ident!("{}", name);
                let expr = expr.translate(symbol_table);
                quote! { #name += #expr; }
            }
            Statement::QubitAllocation(
                QubitAllocationKind::Use,
                binding,
                initializer,
                Some(scope),
            ) => {
                let flattened = flatten_bindings(binding, initializer);

                let mut allocate = vec![];
                let mut release = vec![];
                for (name, init) in flattened {
                    let ident = format_ident!("{}", name);
                    match init {
                        QubitInitializer::Single => {
                            allocate.push(quote! { let #ident = sim__.allocate(); });
                            release.push(quote! { sim__.release(#ident); });
                        }
                        QubitInitializer::Register(expr) => {
                            let expr = expr.translate(symbol_table);
                            allocate
                                .push(quote! { let #ident = sim__.allocate_many(#expr as usize); });
                            release.push(quote! { sim__.release_many(#ident); });
                        }
                        _ => unreachable!(),
                    }
                }

                let mut statements = vec![];
                for stmt in scope.statements() {
                    statements.push(stmt.translate(symbol_table));
                }

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
                let condition = condition.translate(symbol_table);
                let scope = scope.translate(symbol_table);

                code = quote! {
                    #code

                    if #condition #scope
                };

                for (condition, scope) in ifs.iter().skip(1) {
                    let condition = condition.translate(symbol_table);
                    let scope = scope.translate(symbol_table);

                    code = quote! {
                        #code

                        else if #condition #scope
                    };
                }

                if let Some(scope) = default {
                    let scope = scope.translate(symbol_table);

                    code = quote! {
                        #code

                        else #scope
                    };
                }

                code
            }
            Statement::For(binding, expr, scope) => {
                let expr_type = type_from_expression(expr, symbol_table)
                    .expect("cannot determine type of expr in for-loop");

                // TODO for now we assume that expr is array
                let binding = binding.translate(symbol_table);
                let expr = expr.translate(symbol_table);
                let scope = scope.translate(symbol_table);

                match expr_type.as_ref() {
                    TypeKind::Array(_) => quote! { for #binding in #expr.iter() #scope },
                    TypeKind::Range => quote! { for #binding in #expr #scope },
                    _ => panic!("unsupported type of expr in for-loop"),
                }
            }
            Statement::WithinApply(within_scope, apply_scope) => {
                let within_scope_pre = within_scope.translate(symbol_table);
                let apply_scope = apply_scope.translate(symbol_table);
                let within_scope_post = within_scope.clone().adjoint().translate(symbol_table);

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
    fn translate(
        &self,
        _symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
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
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream {
        match self {
            TypeKind::BigInt => todo!(),
            TypeKind::Bool => quote! { bool },
            TypeKind::Double => quote! { f64 },
            TypeKind::Int => quote! { i64 },
            TypeKind::Pauli => quote! { u8 },
            TypeKind::Qubit => quote! { usize },
            TypeKind::Range => quote! { std::ops::Range<i64> },
            TypeKind::Result => quote! { bool },
            TypeKind::String => quote! { String },
            TypeKind::Unit => quote! { () },
            TypeKind::Array(elem) => {
                let elem = elem.translate(symbol_table);
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
        let mut map = HashMap::new();

        assert_eq!(
            parse_expression("1 + 3")?.translate(&mut map).to_string(),
            "(1i64 + 3i64)"
        );
        assert_eq!(
            parse_expression("a - b")?.translate(&mut map).to_string(),
            "(a - b)"
        );

        Ok(())
    }
}
