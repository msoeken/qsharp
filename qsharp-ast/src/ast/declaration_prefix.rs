use std::rc::Rc;

use syn::{
    parse::{Parse, ParseStream},
    Attribute, Result, Token,
};

use crate::ast::{expression::parse_expression, kw, utilities::peek_and_consume, Expression};

/// Access modifier in declaration prefix (e.g., `internal`)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Access {
    Default,
    Internal,
}

/// Contains Q# attributes and modifiers, (e.g., `@Test(ToffoliSimulator) internal`)
#[derive(Debug, PartialEq)]
pub struct DeclarationPrefix {
    attributes: Vec<Rc<Expression>>,
    access: Access,
}

impl DeclarationPrefix {
    pub fn attributes(&self) -> &[Rc<Expression>] {
        &self.attributes
    }

    pub fn access(&self) -> Access {
        self.access
    }
}

impl Parse for DeclarationPrefix {
    fn parse(input: ParseStream) -> Result<Self> {
        // TODO Don't ignore doc comments
        let _ = input.call(Attribute::parse_outer)?;

        // attributes
        let mut attributes = vec![];
        while peek_and_consume(input, Token![@])? {
            attributes.push(parse_expression(input, false)?);
        }

        let access = if peek_and_consume(input, kw::internal)? {
            Access::Internal
        } else {
            Access::Default
        };

        Ok(Self { attributes, access })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expression;

    use syn::Result;

    fn parse_declaration_prefix(str: &str) -> Result<DeclarationPrefix> {
        syn::parse_str(str)
    }

    #[test]
    fn test_empty() -> Result<()> {
        assert_eq!(
            parse_declaration_prefix("")?,
            DeclarationPrefix {
                attributes: vec![],
                access: Access::Default
            }
        );

        Ok(())
    }

    #[test]
    fn test_one_attribute() -> Result<()> {
        assert_eq!(
            parse_declaration_prefix("@Attribute()")?,
            DeclarationPrefix {
                attributes: vec![Expression::call(
                    Expression::simple_identifier("Attribute"),
                    vec![]
                )],
                access: Access::Default
            }
        );

        Ok(())
    }

    #[test]
    fn test_multiple_attribute() -> Result<()> {
        assert_eq!(
            parse_declaration_prefix("@Attribute() @RunOn(1.2, 5, true) internal")?,
            DeclarationPrefix {
                attributes: vec![
                    Expression::call(Expression::simple_identifier("Attribute"), vec![]),
                    Expression::call(
                        Expression::simple_identifier("RunOn"),
                        vec![
                            Expression::double_literal(1.2),
                            Expression::int_literal(5),
                            Expression::bool_literal(true)
                        ]
                    )
                ],
                access: Access::Internal
            }
        );

        Ok(())
    }
}
