use std::rc::Rc;

use crate::ast::{type_kind::parse_type, TypeKind};

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Result, Token,
};

use super::utilities::peek_and_consume;

/// Type parameters are an optional list of types (e.g., `<Qubit, Int -> Bool>`)
#[derive(Debug, PartialEq)]
pub struct TypeParameters(pub Option<Vec<Rc<TypeKind>>>);

impl Parse for TypeParameters {
    fn parse(input: ParseStream) -> Result<Self> {
        if peek_and_consume(input, Token![<])? {
            let arguments: Punctuated<Rc<TypeKind>, Token![,]> =
                Punctuated::parse_separated_nonempty_with(input, parse_type)?;
            input.parse::<Token![>]>()?;
            Ok(TypeParameters(Some(arguments.into_iter().collect())))
        } else {
            Ok(TypeParameters(None))
        }
    }
}
