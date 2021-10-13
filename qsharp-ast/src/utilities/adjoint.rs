use std::rc::Rc;

use itertools::{Either, Itertools};

use crate::{
    ast::{Expression, Scope, Statement},
    utilities::Transformer,
};

pub(crate) struct AdjointTransformer {}

impl Transformer for AdjointTransformer {
    fn visit_scope(&mut self, scope: Scope) -> Scope {
        let (mut statements, mut adjoints): (Vec<Statement>, Vec<Statement>) = scope
            .statements
            .into_iter()
            .partition_map(|statement| match statement {
                Statement::Let(..) | Statement::QubitAllocation(..) => {
                    Either::Left(self.visit_statement(statement))
                }
                _ => Either::Right(self.visit_statement(statement)),
            });

        adjoints.reverse();
        statements.append(&mut adjoints);

        Scope { statements }
    }

    fn visit_within_apply_statement(
        &mut self,
        within_scope: Scope,
        apply_scope: Scope,
    ) -> (Scope, Scope) {
        (within_scope, self.visit_scope(apply_scope))
    }

    fn visit_expression(&mut self, expression: Rc<Expression>) -> Rc<Expression> {
        if let Expression::Call(caller, args) = expression.as_ref() {
            Expression::call(Expression::adjoint(caller.clone()), args.clone())
        } else {
            expression
        }
    }
}
