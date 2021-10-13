use crate::{
    ast::{
        Callable, CallableBody, Characteristics, Scope, Specialization, SpecializationGenerator,
        SpecializationParameter, Statement,
    },
    utilities::VisitorMut,
};

pub(crate) struct Normalizer {}

impl VisitorMut for Normalizer {
    /// In `pre_callable` we first make sure that `CallableBody` is implemented
    /// using `Multiple` implementations; also `... auto` specializations are
    /// added if they are part of the callable's characteristics.
    fn pre_callable(&mut self, callable: &mut Callable) {
        let flags: u8 = if let Some(characteristics) = callable.characteristics() {
            characteristics.as_ref().into()
        } else {
            0b00
        };

        // find single body scopes and move them into a specialization vector.
        if let CallableBody::Simple(scope) = callable.body_mut() {
            let specialization = Specialization::body(scope.clone());

            callable.set_body(CallableBody::Multiple(vec![specialization]));
        }

        // adjoint auto?
        if (flags & 0b01) == 0b01 && !callable.body().has_adjoint() {
            callable
                .body_mut()
                .add_specialization(Specialization::adjoint_with(SpecializationGenerator::Auto));
        }
        // controlled auto?
        if (flags & 0b10) == 0b10 && !callable.body().has_controlled() {
            callable
                .body_mut()
                .add_specialization(Specialization::controlled_with(
                    SpecializationGenerator::Auto,
                ));
        }
        // controlled auto?
        if flags == 0b11 && !callable.body().has_controlled_adjoint() {
            callable
                .body_mut()
                .add_specialization(Specialization::controlled_adjoint_with(
                    SpecializationGenerator::Auto,
                ));
        }
    }

    /// In `pre_callable_body` we generate all specializations such that all of
    /// them are `Provided`, unless they are `... intrinsic`.
    fn pre_callable_body(&mut self, callable_body: &mut CallableBody) {
        if let CallableBody::Simple(scope) = callable_body {
            let specialization = Specialization::body(scope.clone());

            *callable_body = CallableBody::Multiple(vec![specialization]);
        }

        normalize_adjoint(callable_body);
        let parameters = normalize_controlled(callable_body);
        normalize_controlled_adjoint(callable_body, parameters);
    }

    fn visit_scope(&mut self, scope: &mut Scope) {
        let mut found = None;

        // either normalize all, or find the first qubit allocation without a scope
        for (idx, statement) in scope.statements_mut().iter_mut().enumerate() {
            if let Statement::QubitAllocation(_, _, _, None) = statement {
                found = Some(idx);
                break;
            } else {
                self.visit_statement(statement);
            }
        }

        if let Some(idx) = found {
            let mut iter = scope.statements_mut().drain(idx..);

            // the first statement of iter is use statement
            if let Some(Statement::QubitAllocation(kind, binding, init, None)) = iter.next() {
                let inner_scope = Scope::with_statements(iter.collect());
                let mut statement =
                    Statement::QubitAllocation(kind, binding, init, Some(inner_scope));
                self.visit_statement(&mut statement);
                scope.statements_mut().push(statement);
            }
        }
    }
}

fn normalize_adjoint(body: &mut CallableBody) {
    // first make sure that specialization has a provided generator
    match body.adjoint_mut().map(|spec| spec.generator()) {
        Some(SpecializationGenerator::Provided(..)) => {}
        Some(SpecializationGenerator::Self_) => {
            let scope = body.body_scope().unwrap().clone();
            body.adjoint_mut()
                .unwrap()
                .set_generator(SpecializationGenerator::Provided(None, scope))
        }
        Some(SpecializationGenerator::Auto) => {
            let scope = body.body_scope().unwrap().clone().adjoint();
            body.adjoint_mut()
                .unwrap()
                .set_generator(SpecializationGenerator::Provided(None, scope))
        }
        Some(_) => {
            todo!();
        }
        None => {}
    }
}

fn normalize_controlled(body: &mut CallableBody) -> Vec<SpecializationParameter> {
    // first make sure that specialization has a provided generator
    match body.controlled_mut().map(|spec| spec.generator()) {
        Some(SpecializationGenerator::Provided(None, _)) => {
            panic!("controlled specialization has no specialization parameters")
        }
        Some(SpecializationGenerator::Provided(Some(parameters), _)) => parameters.clone(),
        Some(SpecializationGenerator::Auto) => {
            let scope = body.body_scope().unwrap().clone().controlled();
            let parameters = vec![SpecializationParameter::Identifier("__ctls".into())];
            body.controlled_mut()
                .unwrap()
                .set_generator(SpecializationGenerator::Provided(
                    Some(parameters.clone()),
                    scope,
                ));
            parameters
        }
        Some(_) => {
            todo!();
        }
        // if there's no controlled, there'll also be no controlled adjoint
        None => vec![],
    }
}

fn normalize_controlled_adjoint(body: &mut CallableBody, parameters: Vec<SpecializationParameter>) {
    // first make sure that specialization has a provided generator
    match body.controlled_adjoint_mut().map(|spec| spec.generator()) {
        Some(SpecializationGenerator::Provided(..)) => {}
        Some(SpecializationGenerator::Self_) => {
            let scope = body.controlled_scope().unwrap().clone();
            //specialization.set_generator(SpecializationGenerator::Provided(None, scope));
            body.controlled_adjoint_mut()
                .unwrap()
                .set_generator(SpecializationGenerator::Provided(Some(parameters), scope))
        }
        Some(SpecializationGenerator::Auto) => {
            let scope = body.controlled_scope().unwrap().clone().adjoint();
            body.controlled_adjoint_mut()
                .unwrap()
                .set_generator(SpecializationGenerator::Provided(Some(parameters), scope))
        }
        Some(_) => {
            todo!();
        }
        None => {}
    }
}

/// Convert characteristics into bit flags
impl From<&Characteristics> for u8 {
    fn from(characteristics: &Characteristics) -> Self {
        match characteristics {
            Characteristics::Adj => 0b01,
            Characteristics::Ctl => 0b10,
            Characteristics::Union(lhs, rhs) => u8::from(lhs.as_ref()) | u8::from(rhs.as_ref()),
            Characteristics::Intersection(lhs, rhs) => {
                u8::from(lhs.as_ref()) & u8::from(rhs.as_ref())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, QubitAllocationKind, QubitInitializer, SymbolBinding};
    use syn::{parse_str, Result};

    use super::*;

    #[test]
    fn normalize_allocation() {
        let mut scope = Scope::with_statements(vec![
            Statement::Expression(Expression::call(Expression::simple_identifier("A"), vec![])),
            Statement::Expression(Expression::call(Expression::simple_identifier("B"), vec![])),
            Statement::QubitAllocation(
                QubitAllocationKind::Use,
                SymbolBinding::identifier("q"),
                QubitInitializer::single(),
                None,
            ),
            Statement::Expression(Expression::call(Expression::simple_identifier("C"), vec![])),
            Statement::Expression(Expression::call(Expression::simple_identifier("D"), vec![])),
        ]);

        Normalizer {}.visit_scope(&mut scope);

        assert_eq!(
            scope.statements(),
            vec![
                Statement::Expression(Expression::call(Expression::simple_identifier("A"), vec![])),
                Statement::Expression(Expression::call(Expression::simple_identifier("B"), vec![])),
                Statement::QubitAllocation(
                    QubitAllocationKind::Use,
                    SymbolBinding::identifier("q"),
                    QubitInitializer::single(),
                    Some(Scope::with_statements(vec![
                        Statement::Expression(Expression::call(
                            Expression::simple_identifier("C"),
                            vec![]
                        )),
                        Statement::Expression(Expression::call(
                            Expression::simple_identifier("D"),
                            vec![]
                        ))
                    ])),
                ),
            ]
        );
    }

    fn parse_callable(str: &str) -> Result<Callable> {
        parse_str(str)
    }

    #[test]
    fn make_multiple() -> Result<()> {
        let mut callable = parse_callable("operation Op() : Unit {}")?;
        assert!(callable.body().is_simple());
        Normalizer {}.visit_callable(&mut callable);

        assert!(callable.body().is_multiple());
        assert!(callable.body().has_body());
        assert!(!callable.body().has_adjoint());
        assert!(!callable.body().has_controlled());
        assert!(!callable.body().has_controlled_adjoint());

        Ok(())
    }

    #[test]
    fn auto_adjoint() -> Result<()> {
        let mut callable = parse_callable("operation Op() : Unit is Adj {}")?;
        assert!(callable.body().is_simple());
        Normalizer {}.visit_callable(&mut callable);

        assert!(callable.body().is_multiple());
        assert!(callable.body().has_body());
        assert!(callable.body().has_adjoint());
        assert!(!callable.body().has_controlled());
        assert!(!callable.body().has_controlled_adjoint());

        Ok(())
    }

    // TODO: uncomment when automatically controlled is implemented
    #[test]
    fn auto_controlled() -> Result<()> {
        let mut callable = parse_callable("operation Op() : Unit is Ctl {}")?;
        assert!(callable.body().is_simple());
        Normalizer {}.visit_callable(&mut callable);

        assert!(callable.body().is_multiple());
        assert!(callable.body().has_body());
        assert!(!callable.body().has_adjoint());
        assert!(callable.body().has_controlled());
        assert!(!callable.body().has_controlled_adjoint());

        Ok(())
    }

    #[test]
    fn auto_adjoint_controlled() -> Result<()> {
        let mut callable = parse_callable("operation Op() : Unit is Adj+Ctl {}")?;
        assert!(callable.body().is_simple());
        Normalizer {}.visit_callable(&mut callable);

        assert!(callable.body().is_multiple());
        assert!(callable.body().has_body());
        assert!(callable.body().has_adjoint());
        assert!(callable.body().has_controlled());
        assert!(callable.body().has_controlled_adjoint());

        Ok(())
    }
}
