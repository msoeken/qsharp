use std::rc::Rc;

use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Paren,
    Result, Token,
};

use super::type_kind::parse_type;
use crate::ast::{Ident, TypeKind};

/// Parameter in an `operation` or `function` signature
#[derive(Debug, PartialEq)]
pub enum Parameter {
    Item(String, Rc<TypeKind>),
    Tuple(Vec<Rc<Self>>),
}

impl Parameter {
    pub fn item(name: &str, type_kind: Rc<TypeKind>) -> Rc<Self> {
        Rc::new(Self::Item(String::from(name), type_kind))
    }

    pub fn tuple(entries: Vec<Rc<Parameter>>) -> Rc<Self> {
        Rc::new(Self::Tuple(entries))
    }
}

pub(crate) fn parse_parameter(input: ParseStream) -> Result<Rc<Parameter>> {
    if input.peek(Paren) {
        let inner_buffer;
        parenthesized!(inner_buffer in input);
        let entries = if inner_buffer.is_empty() {
            vec![]
        } else {
            let params: Punctuated<Rc<Parameter>, Token![,]> =
                Punctuated::parse_separated_nonempty_with(&inner_buffer, parse_parameter)?;
            params.into_iter().collect()
        };
        Ok(Parameter::tuple(entries))
    } else {
        let ident: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let type_kind = parse_type(input)?;
        Ok(Parameter::item(ident.to_string().as_str(), type_kind))
    }
}

impl Parse for Parameter {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = parse_parameter(input)?;
        Rc::try_unwrap(expr).map_err(|_| input.error("cannot extract parameter"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_parameter(s: &str) -> Result<Parameter> {
        syn::parse_str(s)
    }

    #[test]
    fn test_parameters() -> Result<()> {
        assert_eq!(
            parse_parameter("a : Qubit")?,
            *Parameter::item("a", TypeKind::qubit())
        );

        assert_eq!(parse_parameter("()")?, *Parameter::tuple(vec![]));

        assert_eq!(
            parse_parameter("(a : Qubit, b : Qubit, c : Qubit)")?,
            *Parameter::tuple(vec![
                Parameter::item("a", TypeKind::qubit()),
                Parameter::item("b", TypeKind::qubit()),
                Parameter::item("c", TypeKind::qubit())
            ])
        );

        assert_eq!(
            parse_parameter("((a : Qubit, b : Qubit), c : Qubit)")?,
            *Parameter::tuple(vec![
                Parameter::tuple(vec![
                    Parameter::item("a", TypeKind::qubit()),
                    Parameter::item("b", TypeKind::qubit())
                ]),
                Parameter::item("c", TypeKind::qubit())
            ])
        );

        Ok(())
    }
}
