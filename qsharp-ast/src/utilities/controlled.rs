use std::rc::Rc;

use crate::{
    ast::{Expression, Scope},
    utilities::Transformer,
};

pub(crate) struct ControlledTransformer {}

impl Transformer for ControlledTransformer {
    fn visit_within_apply_statement(
        &mut self,
        within_scope: Scope,
        apply_scope: Scope,
    ) -> (Scope, Scope) {
        (within_scope, self.visit_scope(apply_scope))
    }

    fn visit_expression(&mut self, expression: Rc<Expression>) -> Rc<Expression> {
        if let Expression::Call(caller, args) = expression.as_ref() {
            let args_ctl = vec![
                Expression::simple_identifier("__ctls"),
                Expression::tuple_literal(args.clone()),
            ];

            Expression::call(Expression::controlled(caller.clone()), args_ctl)
        } else {
            expression
        }
    }
}
