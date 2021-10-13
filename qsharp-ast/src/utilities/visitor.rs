use std::rc::Rc;

use crate::ast::{
    Callable, CallableBody, Expression, Namespace, NamespaceItem, Program, QualifiedName, Scope,
    Specialization, SpecializationGenerator, Statement, TypeDeclaration,
};

pub trait VisitorMut {
    fn pre_callable(&mut self, _callable: &mut Callable) {}
    fn post_callable(&mut self, _callable: &mut Callable) {}
    fn visit_callable(&mut self, callable: &mut Callable) {
        self.pre_callable(callable);
        self.visit_callable_body(&mut callable.body);
        self.post_callable(callable);
    }

    fn pre_callable_body(&mut self, _callable_body: &mut CallableBody) {}
    fn post_callable_body(&mut self, _callable_body: &mut CallableBody) {}
    fn visit_callable_body(&mut self, callable_body: &mut CallableBody) {
        self.pre_callable_body(callable_body);
        match callable_body {
            CallableBody::Simple(scope) => self.visit_scope(scope),
            CallableBody::Multiple(specializations) => {
                for specialization in specializations {
                    self.visit_specialization(specialization);
                }
            }
        }
        self.post_callable_body(callable_body);
    }

    fn visit_expression(&mut self, _expression: &mut Rc<Expression>) {}

    fn pre_namespace(&mut self, _namespace: &mut Namespace) {}
    fn post_namespace(&mut self, _namespace: &mut Namespace) {}
    fn visit_namespace(&mut self, namespace: &mut Namespace) {
        self.pre_namespace(namespace);
        self.visit_qualified_name(&mut namespace.name);
        for namespace_item in &mut namespace.items {
            self.visit_namespace_item(namespace_item)
        }
        self.post_namespace(namespace);
    }

    fn pre_namespace_item(&mut self, _namespace_item: &mut NamespaceItem) {}
    fn post_namespace_item(&mut self, _namespace_item: &mut NamespaceItem) {}
    fn visit_namespace_item(&mut self, namespace_item: &mut NamespaceItem) {
        self.pre_namespace_item(namespace_item);
        match namespace_item {
            NamespaceItem::Callable(callable) => self.visit_callable(callable),
            NamespaceItem::OpenDirective(name, alias) => {
                self.visit_qualified_name(name);
                if let Some(alias) = alias {
                    self.visit_qualified_name(alias);
                }
            }
            NamespaceItem::TypeDeclaration(type_declaration) => {
                self.visit_type_declaration(type_declaration)
            }
        }
        self.post_namespace_item(namespace_item);
    }

    fn pre_program(&mut self, _program: &mut Program) {}
    fn post_program(&mut self, _program: &mut Program) {}
    fn visit_program(&mut self, program: &mut Program) {
        self.pre_program(program);
        for namespace in &mut program.namespaces {
            self.visit_namespace(namespace)
        }
        self.post_program(program);
    }

    fn visit_qualified_name(&mut self, _qualified_name: &mut QualifiedName) {}

    fn pre_scope(&mut self, _scope: &mut Scope) {}
    fn post_scope(&mut self, _scope: &mut Scope) {}
    fn visit_scope(&mut self, scope: &mut Scope) {
        self.pre_scope(scope);
        for statement in &mut scope.statements {
            self.visit_statement(statement);
        }
        self.post_scope(scope);
    }

    fn visit_specialization(&mut self, specialization: &mut Specialization) {
        if let SpecializationGenerator::Provided(_, scope) = specialization.generator_mut() {
            self.visit_scope(scope);
        }
    }

    fn pre_statement(&mut self, _statement: &mut Statement) {}
    fn post_statement(&mut self, _statement: &mut Statement) {}
    fn visit_statement(&mut self, statement: &mut Statement) {
        self.pre_statement(statement);
        match statement {
            Statement::Expression(expression)
            | Statement::Return(expression)
            | Statement::Fail(expression) => self.visit_expression(expression),
            Statement::Let(_, expression)
            | Statement::Set(_, expression)
            | Statement::Mutable(_, expression) => {
                // TODO binding
                self.visit_expression(expression);
            }
            Statement::Update(_, _, expression) => {
                self.visit_expression(expression);
            }
            Statement::QubitAllocation(_, _, _, scope) => {
                if let Some(scope) = scope {
                    self.visit_scope(scope);
                }
            }
            Statement::Conditional(conditions, default) => {
                for (expression, scope) in conditions {
                    self.visit_expression(expression);
                    self.visit_scope(scope);
                }
                if let Some(scope) = default {
                    self.visit_scope(scope);
                }
            }
            Statement::For(_, expression, scope) => {
                self.visit_expression(expression);
                self.visit_scope(scope);
            }
            Statement::While(expression, scope) => {
                self.visit_expression(expression);
                self.visit_scope(scope);
            }
            Statement::RepeatUntil(scope, expression, fixup_scope) => {
                self.visit_scope(scope);
                self.visit_expression(expression);
                if let Some(scope) = fixup_scope {
                    self.visit_scope(scope);
                }
            }
            Statement::WithinApply(within_scope, apply_scope) => {
                self.visit_scope(within_scope);
                self.visit_scope(apply_scope);
            }
        }
        self.post_statement(statement);
    }

    fn visit_type_declaration(&mut self, _type_declaration: &mut TypeDeclaration) {}
}
