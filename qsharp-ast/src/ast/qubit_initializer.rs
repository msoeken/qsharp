use std::rc::Rc;

use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Paren},
    Result, Token,
};

use crate::ast::expression::{parse_expression, Expression};

use super::utilities::expect_ident;

/// Qubit initializers in `use` and `borrow` statements
#[derive(Debug, PartialEq)]
pub enum QubitInitializer {
    Single,
    Register(Rc<Expression>),
    Tuple(Vec<Rc<Self>>),
}

impl QubitInitializer {
    pub fn single() -> Rc<Self> {
        Rc::new(Self::Single)
    }

    pub fn register(expr: Rc<Expression>) -> Rc<Self> {
        Rc::new(Self::Register(expr))
    }

    pub fn tuple(items: Vec<Rc<Self>>) -> Rc<Self> {
        Rc::new(Self::Tuple(items))
    }
}

pub(crate) fn parse_qubit_initializer(input: ParseStream) -> Result<Rc<QubitInitializer>> {
    if input.peek(Paren) {
        let inner_buffer;
        parenthesized!(inner_buffer in input);
        let entries = if inner_buffer.is_empty() {
            vec![]
        } else {
            let items: Punctuated<Rc<QubitInitializer>, Token![,]> =
                Punctuated::parse_separated_nonempty_with(&inner_buffer, parse_qubit_initializer)?;
            items.into_iter().collect()
        };
        Ok(QubitInitializer::tuple(entries))
    } else {
        expect_ident(input.parse()?, "Qubit")?;

        if input.peek(Paren) {
            let buffer;
            parenthesized!(buffer in input);
            if !buffer.is_empty() {
                return Err(buffer.error("parentheses for single qubit allocation must be empty"));
            }
            Ok(QubitInitializer::single())
        } else if input.peek(Bracket) {
            let buffer;
            bracketed!(buffer in input);
            let expr = parse_expression(&buffer, false)?;
            Ok(QubitInitializer::register(expr))
        } else {
            Err(input.error("cannot parse qubit initialization"))
        }
    }
}

impl Parse for QubitInitializer {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = parse_qubit_initializer(input)?;
        Rc::try_unwrap(expr).map_err(|_| input.error("cannot extract qubit initializer"))
    }
}
