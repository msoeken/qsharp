use itertools::{chain, Itertools};
use syn::{
    parse::{Parse, ParseStream},
    Result, Token,
};

use crate::ast::Ident;

/// A Q# qualified name (e.g., `Microsoft.Quantum.Intrinsic.X` or simply `CNOT`)
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct QualifiedName {
    namespace: Option<Vec<String>>,
    name: String,
}

impl QualifiedName {
    pub fn new(namespace: &[&str], name: &str) -> Self {
        Self {
            namespace: Some(namespace.iter().cloned().map(String::from).collect()),
            name: String::from(name),
        }
    }

    pub fn simple(name: &str) -> Self {
        Self {
            namespace: None,
            name: String::from(name),
        }
    }

    pub fn is_simple(&self) -> bool {
        self.namespace.is_none()
    }

    pub fn is_simple_name(&self, name: &str) -> bool {
        self.namespace.is_none() && self.name == name
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn namespace(&self) -> &Option<Vec<String>> {
        &self.namespace
    }

    /// Joins namespace parts and name with custom separator
    pub fn join(&self, sep: &str) -> String {
        if let Some(ref vec) = self.namespace {
            chain![vec, std::iter::once(&self.name)].join(sep)
        } else {
            self.name.clone()
        }
    }
}

/// Parses qualified name, but already provides first identifier
pub(crate) fn parse_qualified_name_without_head(
    input: ParseStream,
    first_ident: Ident,
) -> Result<QualifiedName> {
    // we cannot use Punctuated here, because it might conflict with `a..b`
    // as an expression.

    // always starts with an identifier
    let mut items = vec![first_ident.to_string()];

    while !input.is_empty() && input.peek(Token![.]) && input.peek2(syn::Ident) {
        input.parse::<Token![.]>()?;
        items.push(input.parse::<Ident>()?.to_string());
    }

    // SAFETY items cannot be empty because it was constructed with a single
    // element
    let name = items.pop().unwrap();

    Ok(QualifiedName {
        namespace: if items.is_empty() { None } else { Some(items) },
        name,
    })
}

impl Parse for QualifiedName {
    fn parse(input: ParseStream) -> Result<Self> {
        let first_ident = input.parse()?;
        parse_qualified_name_without_head(input, first_ident)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_qualified_name(s: &str) -> Result<QualifiedName> {
        syn::parse_str(s)
    }

    #[test]
    fn test_names() -> Result<()> {
        assert_eq!(parse_qualified_name("H")?, QualifiedName::simple("H"));
        assert_eq!(
            parse_qualified_name("Microsoft.Quantum.Intrinsic.H")?,
            QualifiedName::new(&["Microsoft", "Quantum", "Intrinsic"], "H")
        );

        Ok(())
    }
}
