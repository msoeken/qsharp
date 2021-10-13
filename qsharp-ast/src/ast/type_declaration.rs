use std::rc::Rc;

use proc_macro2::Ident;
use syn::{
    parse::{Parse, ParseStream},
    Result, Token,
};

use crate::ast::{kw, type_kind::parse_type, TypeKind};

use super::declaration_prefix::DeclarationPrefix;

/// A type declaration (e.g., `newtype LittleEndian = Qubit[]`)
#[derive(Debug, PartialEq)]
pub struct TypeDeclaration {
    prefix: DeclarationPrefix,
    name: String,
    underlying_type: Rc<TypeKind>,
}

impl TypeDeclaration {
    pub fn prefix(&self) -> &DeclarationPrefix {
        &self.prefix
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn underlying_type(&self) -> &Rc<TypeKind> {
        &self.underlying_type
    }
}

/// Parses the type declaration without prefix, but provides the prefix as an extra argument.
pub(crate) fn parse_type_declaration_without_prefix(
    input: ParseStream,
    prefix: DeclarationPrefix,
) -> Result<TypeDeclaration> {
    input.parse::<kw::newtype>()?;
    let name: Ident = input.parse()?;
    input.parse::<Token![=]>()?;
    let underlying_type = parse_type(input)?;
    input.parse::<Token![;]>()?;

    Ok(TypeDeclaration {
        prefix,
        name: name.to_string(),
        underlying_type,
    })
}

impl Parse for TypeDeclaration {
    fn parse(input: ParseStream) -> Result<Self> {
        let prefix = input.parse()?;
        parse_type_declaration_without_prefix(input, prefix)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{declaration_prefix::Access, Expression};

    use super::*;

    use syn::Result;

    fn parse_type_declaration(str: &str) -> Result<TypeDeclaration> {
        syn::parse_str(str)
    }

    #[test]
    fn test_type_declarations() -> Result<()> {
        let td = parse_type_declaration("@Attribute() internal newtype Name = String;")?;

        assert_eq!(
            td.prefix().attributes(),
            vec![Expression::call(
                Expression::simple_identifier("Attribute"),
                vec![]
            )]
        );
        assert_eq!(td.prefix().access(), Access::Internal);
        assert_eq!(td.name(), "Name");
        assert_eq!(*td.underlying_type(), TypeKind::string());

        Ok(())
    }
}
