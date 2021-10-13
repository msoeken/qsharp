use std::rc::Rc;

use crate::ast::{
    Callable, CallableBody, Expression, Namespace, NamespaceItem, Program, QualifiedName,
    QubitAllocationKind, QubitInitializer, Scope, Specialization, Statement, SymbolBinding,
    TypeDeclaration, UpdateOperator,
};

pub trait Transformer {
    fn visit_callable(&mut self, callable: Callable) -> Callable {
        let body = self.visit_callable_body(callable.body);

        Callable { body, ..callable }
    }

    fn visit_callable_body(&mut self, callable_body: CallableBody) -> CallableBody {
        match callable_body {
            CallableBody::Simple(scope) => CallableBody::Simple(self.visit_scope(scope)),
            CallableBody::Multiple(specializations) => CallableBody::Multiple(
                specializations
                    .into_iter()
                    .map(|specialization| self.visit_specialization(specialization))
                    .collect(),
            ),
        }
    }

    fn visit_expression(&mut self, expression: Rc<Expression>) -> Rc<Expression> {
        expression
    }

    fn visit_namespace(&mut self, namespace: Namespace) -> Namespace {
        let name = self.visit_qualified_name(namespace.name);
        let items = namespace
            .items
            .into_iter()
            .map(|namespace_item| self.visit_namespace_item(namespace_item))
            .collect();

        Namespace { name, items }
    }

    fn visit_namespace_item(&mut self, namespace_item: NamespaceItem) -> NamespaceItem {
        match namespace_item {
            NamespaceItem::Callable(callable) => {
                NamespaceItem::Callable(self.visit_callable(callable))
            }
            NamespaceItem::OpenDirective(name, alias) => NamespaceItem::OpenDirective(
                self.visit_qualified_name(name),
                alias.map(|qualified_name| self.visit_qualified_name(qualified_name)),
            ),
            NamespaceItem::TypeDeclaration(type_declaration) => {
                NamespaceItem::TypeDeclaration(self.visit_type_declaration(type_declaration))
            }
        }
    }

    fn visit_program(&mut self, prog: Program) -> Program {
        let namespaces = prog
            .namespaces
            .into_iter()
            .map(|namespace| self.visit_namespace(namespace))
            .collect();

        Program { namespaces }
    }

    fn visit_qualified_name(&mut self, qualified_name: QualifiedName) -> QualifiedName {
        qualified_name
    }

    fn visit_scope(&mut self, scope: Scope) -> Scope {
        let statements = scope
            .statements
            .into_iter()
            .map(|statement| self.visit_statement(statement))
            .collect();

        Scope { statements }
    }

    fn visit_specialization(&mut self, specialization: Specialization) -> Specialization {
        specialization
    }

    fn visit_expression_statement(&mut self, expression: Rc<Expression>) -> Rc<Expression> {
        self.visit_expression(expression)
    }
    fn visit_return_statement(&mut self, expression: Rc<Expression>) -> Rc<Expression> {
        self.visit_expression(expression)
    }
    fn visit_fail_statement(&mut self, expression: Rc<Expression>) -> Rc<Expression> {
        self.visit_expression(expression)
    }
    fn visit_let_statement(
        &mut self,
        binding: Rc<SymbolBinding>,
        expression: Rc<Expression>,
    ) -> (Rc<SymbolBinding>, Rc<Expression>) {
        (binding, self.visit_expression(expression))
    }
    fn visit_set_statement(
        &mut self,
        binding: Rc<SymbolBinding>,
        expression: Rc<Expression>,
    ) -> (Rc<SymbolBinding>, Rc<Expression>) {
        (binding, self.visit_expression(expression))
    }
    fn visit_update_statement(
        &mut self,
        identifier: String,
        operator: UpdateOperator,
        expression: Rc<Expression>,
    ) -> (String, UpdateOperator, Rc<Expression>) {
        (identifier, operator, self.visit_expression(expression))
    }
    fn visit_mutable_statement(
        &mut self,
        binding: Rc<SymbolBinding>,
        expression: Rc<Expression>,
    ) -> (Rc<SymbolBinding>, Rc<Expression>) {
        (binding, self.visit_expression(expression))
    }
    fn visit_qubit_allocation_statement(
        &mut self,
        kind: QubitAllocationKind,
        binding: Rc<SymbolBinding>,
        initializer: Rc<QubitInitializer>,
        scope: Option<Scope>,
    ) -> (
        QubitAllocationKind,
        Rc<SymbolBinding>,
        Rc<QubitInitializer>,
        Option<Scope>,
    ) {
        (
            kind,
            binding,
            initializer,
            scope.map(|scope| self.visit_scope(scope)),
        )
    }
    fn visit_conditional_statement(
        &mut self,
        conditions: Vec<(Rc<Expression>, Scope)>,
        default: Option<Scope>,
    ) -> (Vec<(Rc<Expression>, Scope)>, Option<Scope>) {
        (
            conditions
                .into_iter()
                .map(|(expression, scope)| {
                    (self.visit_expression(expression), self.visit_scope(scope))
                })
                .collect(),
            default.map(|scope| self.visit_scope(scope)),
        )
    }
    fn visit_for_statement(
        &mut self,
        binding: Rc<SymbolBinding>,
        expression: Rc<Expression>,
        scope: Scope,
    ) -> (Rc<SymbolBinding>, Rc<Expression>, Scope) {
        (
            binding,
            self.visit_expression(expression),
            self.visit_scope(scope),
        )
    }
    fn visit_while_statement(
        &mut self,
        expression: Rc<Expression>,
        scope: Scope,
    ) -> (Rc<Expression>, Scope) {
        (self.visit_expression(expression), self.visit_scope(scope))
    }
    fn visit_repeat_until_statement(
        &mut self,
        scope: Scope,
        expression: Rc<Expression>,
        fixup: Option<Scope>,
    ) -> (Scope, Rc<Expression>, Option<Scope>) {
        (
            self.visit_scope(scope),
            self.visit_expression(expression),
            fixup.map(|scope| self.visit_scope(scope)),
        )
    }
    fn visit_within_apply_statement(
        &mut self,
        within_scope: Scope,
        apply_scope: Scope,
    ) -> (Scope, Scope) {
        (
            self.visit_scope(within_scope),
            self.visit_scope(apply_scope),
        )
    }

    fn visit_statement(&mut self, statement: Statement) -> Statement {
        match statement {
            Statement::Expression(expression) => {
                Statement::Expression(self.visit_expression_statement(expression))
            }
            Statement::Return(expression) => {
                Statement::Return(self.visit_return_statement(expression))
            }
            Statement::Fail(expression) => Statement::Fail(self.visit_fail_statement(expression)),
            Statement::Let(binding, expression) => {
                let (binding, expression) = self.visit_let_statement(binding, expression);
                Statement::Let(binding, expression)
            }
            Statement::Set(binding, expression) => {
                let (binding, expression) = self.visit_set_statement(binding, expression);
                Statement::Set(binding, expression)
            }
            Statement::Update(identifier, operator, expression) => {
                let (identifier, operator, expression) =
                    self.visit_update_statement(identifier, operator, expression);
                Statement::Update(identifier, operator, expression)
            }
            Statement::Mutable(binding, expression) => {
                let (binding, expression) = self.visit_mutable_statement(binding, expression);
                Statement::Mutable(binding, expression)
            }
            Statement::QubitAllocation(kind, binding, initializer, scope) => {
                let (kind, binding, initializer, scope) =
                    self.visit_qubit_allocation_statement(kind, binding, initializer, scope);
                Statement::QubitAllocation(kind, binding, initializer, scope)
            }
            Statement::Conditional(conditions, default) => {
                let (conditions, default) = self.visit_conditional_statement(conditions, default);
                Statement::Conditional(conditions, default)
            }
            Statement::For(binding, expression, scope) => {
                let (binding, expression, scope) =
                    self.visit_for_statement(binding, expression, scope);
                Statement::For(binding, expression, scope)
            }
            Statement::While(expression, scope) => {
                let (expression, scope) = self.visit_while_statement(expression, scope);
                Statement::While(expression, scope)
            }
            Statement::RepeatUntil(scope, expression, fixup_scope) => {
                let (scope, expression, fixup_scope) =
                    self.visit_repeat_until_statement(scope, expression, fixup_scope);
                Statement::RepeatUntil(scope, expression, fixup_scope)
            }
            Statement::WithinApply(within_scope, apply_scope) => {
                let (within_scope, apply_scope) =
                    self.visit_within_apply_statement(within_scope, apply_scope);
                Statement::WithinApply(within_scope, apply_scope)
            }
        }
    }

    fn visit_type_declaration(&mut self, type_declaration: TypeDeclaration) -> TypeDeclaration {
        type_declaration
    }
}
