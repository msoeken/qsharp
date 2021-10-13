use syn::{
    braced,
    parse::{Parse, ParseStream},
    Result,
};

use crate::{
    ast::Statement,
    utilities::{AdjointTransformer, ControlledTransformer, Transformer},
};

use super::utilities::parse_many;

/// A Q# scope is a sequence of Q# statements inside `{ ... }`
#[derive(Debug, Default, PartialEq, Clone)]
pub struct Scope {
    pub(crate) statements: Vec<Statement>,
}

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_statements(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    pub fn statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn adjoint(self) -> Self {
        AdjointTransformer {}.visit_scope(self)
    }

    pub fn controlled(self) -> Self {
        ControlledTransformer {}.visit_scope(self)
    }
}

/// Parses scope without parsing the braces
pub(crate) fn parse_scope_statements(input: ParseStream) -> Result<Scope> {
    Ok(Scope {
        statements: parse_many(input)?,
    })
}

impl Parse for Scope {
    fn parse(input: ParseStream) -> Result<Self> {
        let inner;
        braced!(inner in input);

        Ok(Self {
            statements: parse_many(&inner)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_scope(s: &str) -> Result<Scope> {
        syn::parse_str(s)
    }

    #[test]
    fn test_empty_scope() -> Result<()> {
        let scope = parse_scope("{}")?;

        assert!(scope.statements.is_empty());

        Ok(())
    }
}
