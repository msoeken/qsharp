use std::rc::Rc;

use itertools::Itertools;

use crate::ast::{
    ArrayItemIndex, Callable, CallableBody, CallableKind, Characteristics, DeclarationPrefix,
    Expression, Ident, Namespace, NamespaceItem, Parameter, Program, QualifiedName,
    QubitAllocationKind, QubitInitializer, Scope, Specialization, Statement, SymbolBinding,
    TypeDeclaration, TypeKind, TypeParameter, UpdateOperator,
};

pub trait Mapper {
    type Output;

    fn visit_program(&mut self, program: &Program) -> Self::Output {
        let namespaces = program
            .namespaces
            .iter()
            .map(|namespace| self.visit_namespace(namespace))
            .collect_vec();
        self.map_program(&namespaces)
    }

    fn map_program(&mut self, namespaces: &[Self::Output]) -> Self::Output;

    fn visit_namespace(&mut self, namespace: &Namespace) -> Self::Output {
        let name = self.visit_qualified_name(namespace.name());
        let items = namespace
            .items
            .iter()
            .map(|item| self.visit_namespace_item(item))
            .collect_vec();
        self.map_namespace(name, &items)
    }

    fn map_namespace(&mut self, name: Self::Output, items: &[Self::Output]) -> Self::Output;

    fn visit_namespace_item(&mut self, item: &NamespaceItem) -> Self::Output {
        match item {
            NamespaceItem::Callable(callable) => self.visit_callable(callable),
            NamespaceItem::OpenDirective(name, alias) => {
                let name = self.visit_qualified_name(name);
                let alias = alias.as_ref().map(|name| self.visit_qualified_name(name));
                self.map_open_declaration(name, alias)
            }
            NamespaceItem::TypeDeclaration(type_declaration) => {
                self.visit_type_declaration(type_declaration)
            }
        }
    }

    fn map_open_declaration(
        &mut self,
        name: Self::Output,
        namespaces: Option<Self::Output>,
    ) -> Self::Output;
    //fn map_callable(&mut self, callable: Self::Output) -> Self::Output;fn map_type_declaration(&mut self, type_declaration: Self::Output) -> Self::Output;

    fn visit_qualified_name(&mut self, name: &QualifiedName) -> Self::Output;

    fn visit_callable(&mut self, callable: &Callable) -> Self::Output {
        let prefix = self.visit_declaration_prefix(&callable.prefix);
        let kind = self.visit_callable_kind(&callable.kind);
        let name = self.visit_ident(&callable.name);
        let type_parameters = callable.type_parameters.as_ref().map(|ps| {
            ps.iter()
                .map(|p| self.visit_type_parameter(p))
                .collect_vec()
        });
        let parameters = self.visit_parameter(&callable.parameters);
        let return_type = self.visit_type_kind(&callable.return_type);
        let characteristics = callable
            .characteristics
            .as_ref()
            .map(|c| self.visit_characteristics(c));
        let body = self.visit_callable_body(&callable.body);

        self.map_callable(
            prefix,
            kind,
            name,
            type_parameters,
            parameters,
            return_type,
            characteristics,
            body,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn map_callable(
        &mut self,
        prefix: Self::Output,
        kind: Self::Output,
        name: Self::Output,
        type_parameters: Option<Vec<Self::Output>>,
        parameters: Self::Output,
        return_type: Self::Output,
        characteristics: Option<Self::Output>,
        body: Self::Output,
    ) -> Self::Output;

    fn visit_callable_body(&mut self, callable_body: &CallableBody) -> Self::Output {
        match callable_body {
            CallableBody::Simple(scope) => {
                let scope = self.visit_scope(scope);
                self.map_simple_callable_body(scope)
            }
            CallableBody::Multiple(specializations) => {
                let specializations = specializations
                    .iter()
                    .map(|s| self.visit_specialization(s))
                    .collect_vec();
                self.map_multiple_callable_body(&specializations)
            }
        }
    }
    fn map_simple_callable_body(&mut self, scope: Self::Output) -> Self::Output;
    fn map_multiple_callable_body(&mut self, specializations: &[Self::Output]) -> Self::Output;
    fn visit_specialization(&mut self, _specialization: &Specialization) -> Self::Output {
        todo!();
    }

    fn pre_scope(&mut self, _scope: &Scope) {}
    fn post_scope(&mut self, _scope: &Scope) {}
    fn visit_scope(&mut self, scope: &Scope) -> Self::Output {
        self.pre_scope(scope);
        let statements = scope
            .statements
            .iter()
            .map(|stmt| self.visit_statement(stmt))
            .collect_vec();
        self.post_scope(scope);
        self.map_scope(&statements)
    }
    fn map_scope(&mut self, statements: &[Self::Output]) -> Self::Output;

    fn visit_statement(&mut self, statement: &Statement) -> Self::Output {
        match statement {
            Statement::Expression(expression) => self.visit_expression_statement(expression),
            Statement::Return(expression) => self.visit_return_statement(expression),
            Statement::Fail(expression) => self.visit_fail_statement(expression),
            Statement::Let(binding, expression) => self.visit_let_statement(binding, expression),
            Statement::Set(binding, expression) => self.visit_set_statement(binding, expression),
            Statement::Update(identifier, operator, expression) => {
                self.visit_update_statement(identifier, operator, expression)
            }
            Statement::Mutable(binding, expression) => {
                self.visit_mutable_statement(binding, expression)
            }
            Statement::QubitAllocation(kind, binding, initializer, scope) => {
                self.visit_qubit_allocation(kind, binding, initializer, scope)
            }
            Statement::Conditional(conditions, default) => {
                self.visit_conditional(conditions, default)
            }
            Statement::For(binding, expression, scope) => {
                self.visit_for(binding, expression, scope)
            }
            Statement::While(expression, scope) => self.visit_while(expression, scope),
            Statement::RepeatUntil(scope, expression, fixup_scope) => {
                self.visit_repeat_until(scope, expression, fixup_scope)
            }
            Statement::WithinApply(within_scope, apply_scope) => {
                self.visit_within_apply(within_scope, apply_scope)
            }
        }
    }

    // Statements

    fn visit_expression_statement(&mut self, expression: &Expression) -> Self::Output {
        let expression = self.visit_expression(expression);
        self.map_expression_statement(expression)
    }
    fn map_expression_statement(&mut self, expression: Self::Output) -> Self::Output;
    fn visit_return_statement(&mut self, expression: &Expression) -> Self::Output {
        let expression = self.visit_expression(expression);
        self.map_return_statement(expression)
    }
    fn map_return_statement(&mut self, expression: Self::Output) -> Self::Output;
    fn visit_fail_statement(&mut self, expression: &Expression) -> Self::Output;

    fn pre_let_statement(&mut self, _binding: &SymbolBinding, _expression: &Expression) {}
    fn post_let_statement(&mut self, _binding: &SymbolBinding, _expression: &Expression) {}
    fn visit_let_statement(
        &mut self,
        binding: &SymbolBinding,
        expression: &Expression,
    ) -> Self::Output {
        self.pre_let_statement(binding, expression);
        let binding_ = self.visit_symbol_binding(binding);
        let expression_ = self.visit_expression(expression);
        self.post_let_statement(binding, expression);
        self.map_let_statement(binding_, expression_)
    }
    fn map_let_statement(
        &mut self,
        binding: Self::Output,
        expression: Self::Output,
    ) -> Self::Output;

    fn visit_set_statement(
        &mut self,
        binding: &SymbolBinding,
        expression: &Expression,
    ) -> Self::Output {
        let binding = self.visit_symbol_binding(binding);
        let expression = self.visit_expression(expression);
        self.map_set_statement(binding, expression)
    }
    fn map_set_statement(
        &mut self,
        binding: Self::Output,
        expression: Self::Output,
    ) -> Self::Output;
    fn visit_update_statement(
        &mut self,
        identifier: &str,
        operator: &UpdateOperator,
        expression: &Expression,
    ) -> Self::Output {
        let identifier = self.visit_identifier(identifier);
        let operator = self.visit_update_operator(operator);
        let expression = self.visit_expression(expression);
        self.map_update_statement(identifier, operator, expression)
    }
    fn map_update_statement(
        &mut self,
        identifier: Self::Output,
        operator: Self::Output,
        expression: Self::Output,
    ) -> Self::Output;

    fn pre_mutable_statement(&mut self, _binding: &SymbolBinding, _expression: &Expression) {}
    fn post_mutable_statement(&mut self, _binding: &SymbolBinding, _expression: &Expression) {}
    fn visit_mutable_statement(
        &mut self,
        binding: &SymbolBinding,
        expression: &Expression,
    ) -> Self::Output {
        self.pre_mutable_statement(binding, expression);
        let binding_ = self.visit_symbol_binding(binding);
        let expression_ = self.visit_expression(expression);
        self.post_mutable_statement(binding, expression);
        self.map_mutable_statement(binding_, expression_)
    }
    fn map_mutable_statement(
        &mut self,
        binding: Self::Output,
        expression: Self::Output,
    ) -> Self::Output;

    fn visit_qubit_allocation(
        &mut self,
        kind: &QubitAllocationKind,
        binding: &SymbolBinding,
        initializer: &QubitInitializer,
        scope: &Option<Scope>,
    ) -> Self::Output;
    fn visit_conditional(
        &mut self,
        conditions: &[(Rc<Expression>, Scope)],
        default: &Option<Scope>,
    ) -> Self::Output;
    fn visit_for(
        &mut self,
        binding: &SymbolBinding,
        expression: &Expression,
        scope: &Scope,
    ) -> Self::Output;
    fn visit_while(&mut self, expression: &Expression, scope: &Scope) -> Self::Output;
    fn visit_repeat_until(
        &mut self,
        scope: &Scope,
        expression: &Expression,
        fixup_scope: &Option<Scope>,
    ) -> Self::Output;
    fn visit_within_apply(&mut self, within_scope: &Scope, apply_scope: &Scope) -> Self::Output;

    // Expressions

    fn visit_expression(&mut self, expression: &Expression) -> Self::Output;
    fn visit_array_item_index(&mut self, index: &ArrayItemIndex) -> Self::Output;

    fn visit_type_declaration(&mut self, _type_declaration: &TypeDeclaration) -> Self::Output {
        todo!();
    }
    fn visit_type_parameter(&mut self, type_parameter: &TypeParameter) -> Self::Output;
    fn visit_parameter(&mut self, parameter: &Parameter) -> Self::Output;
    fn visit_characteristics(&mut self, characteristics: &Characteristics) -> Self::Output;
    fn visit_symbol_binding(&mut self, binding: &SymbolBinding) -> Self::Output;
    fn visit_identifier(&mut self, identifier: &str) -> Self::Output;
    fn visit_update_operator(&mut self, update_operator: &UpdateOperator) -> Self::Output;

    fn visit_type_kind(&mut self, type_kind: &TypeKind) -> Self::Output;

    fn visit_declaration_prefix(&mut self, declaration_prefix: &DeclarationPrefix) -> Self::Output;
    fn visit_callable_kind(&mut self, kind: &CallableKind) -> Self::Output;
    fn visit_ident(&mut self, ident: &Ident) -> Self::Output;
}
