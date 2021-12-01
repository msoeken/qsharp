use std::{collections::HashMap, rc::Rc};

use proc_macro2::Punct;
use qsharp_ast::{
    analysis::type_from_expression,
    ast::{
        ArrayItemIndex, Callable, Expression, Parameter, QualifiedName, QubitInitializer, Scope,
        SymbolBinding, TypeKind, UpdateOperator,
    },
    utilities::Mapper,
};
use quote::{format_ident, quote, TokenStreamExt};

use crate::codegen::callable::{tokenize_tree, unzip_parameters};

mod callable;
mod expression;
mod symbol_binding;

pub struct Codegen {
    symbol_table: HashMap<QualifiedName, Rc<TypeKind>>,
    scope_variables: Vec<Vec<QualifiedName>>,
    sim_trait: String,
}

impl Default for Codegen {
    fn default() -> Self {
        Self {
            symbol_table: Default::default(),
            scope_variables: Default::default(),
            sim_trait: String::from("QSharpIntrinsics"),
        }
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_sim_trait(&mut self, sim_trait: &str) {
        self.sim_trait = sim_trait.to_string();
    }

    fn add_parameter_to_symbol_table(&mut self, parameter: &Parameter) {
        match parameter {
            Parameter::Item(name, kind) => {
                self.add_scope_variable(QualifiedName::simple(name), kind.clone());
            }
            Parameter::Tuple(params) => {
                for param in params {
                    self.add_parameter_to_symbol_table(param);
                }
            }
        }
    }

    fn add_scope_variable(&mut self, name: QualifiedName, kind: Rc<TypeKind>) {
        // SAFETY: scope_variables cannot be empty at this point
        assert!(!self.scope_variables.is_empty());
        self.scope_variables.last_mut().unwrap().push(name.clone());
        self.symbol_table.insert(name, kind);
    }

    fn push_scope(&mut self) {
        self.scope_variables.push(vec![]);
    }

    fn pop_scope(&mut self) {
        if let Some(names) = self.scope_variables.pop() {
            for name in names {
                self.symbol_table.remove(&name);
            }
        }
    }
}

impl Mapper for Codegen {
    type Output = proc_macro2::TokenStream;

    fn map_program(&mut self, namespaces: &[Self::Output]) -> Self::Output {
        quote! { #(#namespaces)* }
    }

    fn map_namespace(&mut self, name: Self::Output, items: &[Self::Output]) -> Self::Output {
        quote! {
            #[allow(non_snake_case, unused_variables, dead_code, clippy::clone_on_copy, clippy::redundant_clone, clippy::unused_unit)]
            mod #name {
                use crate::*;
                use super::*;
                use qsharp_runtime::*;
                use Microsoft::Quantum::Core::*;

                #(#items)*
            }
        }
    }

    fn map_open_declaration(
        &mut self,
        name: Self::Output,
        namespaces: Option<Self::Output>,
    ) -> Self::Output {
        assert!(
            namespaces.is_none(),
            "aliases in open declarations not supported yet"
        );
        quote! { use #name::*; }
    }

    fn visit_qualified_name(&mut self, name: &QualifiedName) -> Self::Output {
        match name.namespace() {
            Some(namespace) => {
                let mut tokenstream = proc_macro2::TokenStream::new();

                for item in namespace {
                    tokenstream.append(format_ident!("{}", item));
                    tokenstream.append(Punct::new(':', proc_macro2::Spacing::Joint));
                    tokenstream.append(Punct::new(':', proc_macro2::Spacing::Joint));
                }
                tokenstream.append(format_ident!("{}", name.name()));

                tokenstream
            }
            None => {
                let name = format_ident!("{}", name.name());
                quote! { #name }
            }
        }
    }

    fn visit_callable(&mut self, callable: &Callable) -> Self::Output {
        callable::visit_callable(self, callable)
    }

    fn map_callable(
        &mut self,
        _prefix: Self::Output,
        _kind: Self::Output,
        _name: Self::Output,
        _type_parameters: Option<Vec<Self::Output>>,
        _parameters: Self::Output,
        _return_type: Self::Output,
        _characteristics: Option<Self::Output>,
        _body: Self::Output,
    ) -> Self::Output {
        unreachable!("visit_callable has been implemented directly")
    }

    fn map_simple_callable_body(&mut self, _scope: Self::Output) -> Self::Output {
        todo!()
    }

    fn map_multiple_callable_body(&mut self, _specializations: &[Self::Output]) -> Self::Output {
        todo!()
    }

    fn pre_scope(&mut self, _scope: &Scope) {
        self.push_scope();
    }
    fn post_scope(&mut self, _scope: &Scope) {
        self.pop_scope();
    }
    fn map_scope(&mut self, statements: &[Self::Output]) -> Self::Output {
        quote! {
            {
                #(#statements)*
            }
        }
    }

    fn map_expression_statement(&mut self, expression: Self::Output) -> Self::Output {
        quote! { #expression; }
    }

    fn map_return_statement(&mut self, expression: Self::Output) -> Self::Output {
        quote! { return #expression; }
    }

    fn visit_fail_statement(&mut self, _expression: &qsharp_ast::ast::Expression) -> Self::Output {
        todo!()
    }

    fn post_let_statement(&mut self, binding: &SymbolBinding, expression: &Expression) {
        if let SymbolBinding::Identifier(name) = binding {
            if let Some(kind) = type_from_expression(expression, &self.symbol_table) {
                self.add_scope_variable(QualifiedName::simple(name), kind);
            }
        } else {
            // TODO
        }
    }
    fn map_let_statement(
        &mut self,
        binding: Self::Output,
        expression: Self::Output,
    ) -> Self::Output {
        quote! { let #binding = #expression; }
    }

    fn map_set_statement(
        &mut self,
        binding: Self::Output,
        expression: Self::Output,
    ) -> Self::Output {
        quote! { #binding = #expression; }
    }

    fn visit_update_statement(
        &mut self,
        identifier: &str,
        operator: &qsharp_ast::ast::UpdateOperator,
        expression: &qsharp_ast::ast::Expression,
    ) -> Self::Output {
        match operator {
            UpdateOperator::Addition => {
                let name = format_ident!("{}", identifier);
                let expression = self.visit_expression(expression);
                quote! { #name += #expression; }
            }
            _ => todo!("unsupported update operator {:?}", operator),
        }
    }

    fn map_update_statement(
        &mut self,
        _identifier: Self::Output,
        _operator: Self::Output,
        _expression: Self::Output,
    ) -> Self::Output {
        unreachable!("visit_update_statement has been implemented directly")
    }

    fn post_mutable_statement(&mut self, binding: &SymbolBinding, expression: &Expression) {
        if let SymbolBinding::Identifier(name) = binding {
            // TODO: raise a warning if type cannot be determined
            if let Some(kind) = type_from_expression(expression, &self.symbol_table) {
                self.add_scope_variable(QualifiedName::simple(name), kind);
            }
        } else {
            // TODO
        }
    }
    fn map_mutable_statement(
        &mut self,
        binding: Self::Output,
        expression: Self::Output,
    ) -> Self::Output {
        quote! { let mut #binding = #expression; }
    }

    fn visit_qubit_allocation(
        &mut self,
        _kind: &qsharp_ast::ast::QubitAllocationKind,
        binding: &qsharp_ast::ast::SymbolBinding,
        initializer: &qsharp_ast::ast::QubitInitializer,
        scope: &Option<qsharp_ast::ast::Scope>,
    ) -> Self::Output {
        let flattened = symbol_binding::flatten_bindings(binding, initializer);

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
                    let expr = self.visit_expression(expr);
                    allocate.push(quote! { let #ident = sim__.allocate_many(#expr as usize); });
                    release.push(quote! { sim__.release_many(#ident); });
                }
                _ => unreachable!(),
            }
        }

        let mut statements = vec![];
        for stmt in scope
            .as_ref()
            .expect("scope must have been initialized after normalization")
            .statements()
        {
            statements.push(self.visit_statement(stmt));
        }

        quote! {
            {
                #(#allocate)*
                #(#statements)*
                #(#release)*
            }
        }
    }

    fn visit_conditional(
        &mut self,
        conditions: &[(
            std::rc::Rc<qsharp_ast::ast::Expression>,
            qsharp_ast::ast::Scope,
        )],
        default: &Option<qsharp_ast::ast::Scope>,
    ) -> Self::Output {
        let mut code = quote! {};

        let (condition, scope) = &conditions[0];
        let condition = self.visit_expression(condition);
        let scope = self.visit_scope(scope);

        code = quote! {
            #code

            if #condition #scope
        };

        for (condition, scope) in conditions.iter().skip(1) {
            let condition = self.visit_expression(condition);
            let scope = self.visit_scope(scope);

            code = quote! {
                #code

                else if #condition #scope
            };
        }

        if let Some(scope) = default {
            let scope = self.visit_scope(scope);

            code = quote! {
                #code

                else #scope
            };
        }

        code
    }

    fn visit_for(
        &mut self,
        binding: &qsharp_ast::ast::SymbolBinding,
        expression: &qsharp_ast::ast::Expression,
        scope: &qsharp_ast::ast::Scope,
    ) -> Self::Output {
        let expr_type = type_from_expression(expression, &self.symbol_table)
            .expect("cannot determine type of expr in for-loop");

        // TODO for now we assume that expr is array
        let binding = self.visit_symbol_binding(binding);
        let expr = self.visit_expression(expression);
        let scope = self.visit_scope(scope);

        match expr_type.as_ref() {
            TypeKind::Array(_) => quote! { for #binding in #expr.iter() #scope },
            TypeKind::Range => quote! { for #binding in #expr #scope },
            _ => panic!("unsupported type of expr in for-loop"),
        }
    }

    fn visit_while(
        &mut self,
        _expression: &qsharp_ast::ast::Expression,
        _scope: &qsharp_ast::ast::Scope,
    ) -> Self::Output {
        todo!()
    }

    fn visit_repeat_until(
        &mut self,
        _scope: &qsharp_ast::ast::Scope,
        _expression: &qsharp_ast::ast::Expression,
        _fixup_scope: &Option<qsharp_ast::ast::Scope>,
    ) -> Self::Output {
        todo!()
    }

    fn visit_within_apply(
        &mut self,
        within_scope: &qsharp_ast::ast::Scope,
        apply_scope: &qsharp_ast::ast::Scope,
    ) -> Self::Output {
        let within_scope_pre = self.visit_scope(within_scope);
        let apply_scope = self.visit_scope(apply_scope);
        let within_scope_post = self.visit_scope(&within_scope.clone().adjoint());

        quote! {
            #within_scope_pre
            #apply_scope
            #within_scope_post
        }
    }

    fn visit_expression(&mut self, expression: &Expression) -> Self::Output {
        expression::visit_expression(self, expression)
    }

    fn visit_array_item_index(&mut self, index: &ArrayItemIndex) -> Self::Output {
        match index {
            ArrayItemIndex::Simple(expr) => {
                let expr = self.visit_expression(expr);
                quote! { #expr as usize }
            }
            _ => todo!(),
        }
    }

    fn visit_type_parameter(
        &mut self,
        _type_parameter: &qsharp_ast::ast::TypeParameter,
    ) -> Self::Output {
        todo!()
    }

    fn visit_parameter(&mut self, parameter: &qsharp_ast::ast::Parameter) -> Self::Output {
        match parameter {
            Parameter::Item(name, kind) => {
                let name = format_ident!("{}", name);
                let kind = self.visit_type_kind(kind);
                quote! { #name: #kind }
            }
            Parameter::Tuple(items) => {
                // TODO use generate_parameters_directly here
                let items = items.iter().map(|item| {
                    let (names, kind) = unzip_parameters(item);
                    let names = tokenize_tree(&names, self);
                    let kind = tokenize_tree(&kind, self);
                    quote! { #names: #kind }
                });

                quote! { #(#items),* }
            }
        }
    }

    fn visit_characteristics(
        &mut self,
        _characteristics: &qsharp_ast::ast::Characteristics,
    ) -> Self::Output {
        todo!()
    }

    fn visit_symbol_binding(&mut self, binding: &SymbolBinding) -> Self::Output {
        match binding {
            SymbolBinding::Discard => quote! { _ },
            SymbolBinding::Identifier(name) => {
                let name = format_ident!("{}", name);
                quote! { #name }
            }
            _ => todo!(),
        }
    }

    fn visit_identifier(&mut self, _identifier: &str) -> Self::Output {
        todo!()
    }

    fn visit_update_operator(
        &mut self,
        _update_operator: &qsharp_ast::ast::UpdateOperator,
    ) -> Self::Output {
        todo!()
    }

    fn visit_type_kind(&mut self, type_kind: &qsharp_ast::ast::TypeKind) -> Self::Output {
        match type_kind {
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
                let elem = self.visit_type_kind(elem);
                quote! { std::rc::Rc<Vec<#elem>> }
            }
            _ => todo!(),
        }
    }

    fn visit_declaration_prefix(
        &mut self,
        _declaration_prefix: &qsharp_ast::ast::DeclarationPrefix,
    ) -> Self::Output {
        todo!()
    }

    fn visit_callable_kind(&mut self, _kind: &qsharp_ast::ast::CallableKind) -> Self::Output {
        todo!()
    }

    fn visit_ident(&mut self, _ident: &qsharp_ast::ast::Ident) -> Self::Output {
        todo!()
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
        let mut mapper = Codegen::new();

        assert_eq!(
            mapper
                .visit_expression(&parse_expression("1 + 3")?)
                .to_string(),
            "(1i64 + 3i64)"
        );
        assert_eq!(
            mapper
                .visit_expression(&parse_expression("a - b")?)
                .to_string(),
            "(a - b)"
        );

        Ok(())
    }
}
