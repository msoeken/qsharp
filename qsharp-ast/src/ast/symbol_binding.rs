use std::rc::Rc;

use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Paren,
    Result, Token,
};

use crate::ast::Ident;

/// Symbol bindings in `let`, `mutable`, and `set` expressions
#[derive(Debug, PartialEq)]
pub enum SymbolBinding {
    Discard,
    Identifier(String),
    Tuple(Vec<Rc<Self>>),
}

impl SymbolBinding {
    pub fn discard() -> Rc<Self> {
        Rc::new(Self::Discard)
    }

    pub fn identifier(name: &str) -> Rc<Self> {
        Rc::new(Self::Identifier(String::from(name)))
    }

    pub fn tuple(items: Vec<Rc<Self>>) -> Rc<Self> {
        Rc::new(Self::Tuple(items))
    }
}

pub(crate) fn parse_symbol_binding(input: ParseStream) -> Result<Rc<SymbolBinding>> {
    if input.peek(Token![_]) {
        input.parse::<Token![_]>()?;
        Ok(SymbolBinding::discard())
    } else if input.peek(Paren) {
        let inner_buffer;
        parenthesized!(inner_buffer in input);
        let entries = if inner_buffer.is_empty() {
            vec![]
        } else {
            let params: Punctuated<Rc<SymbolBinding>, Token![,]> =
                Punctuated::parse_separated_nonempty_with(&inner_buffer, parse_symbol_binding)?;
            params.into_iter().collect()
        };
        Ok(SymbolBinding::tuple(entries))
    } else {
        let ident: Ident = input.parse()?;
        Ok(SymbolBinding::identifier(ident.to_string().as_str()))
    }
}

impl Parse for SymbolBinding {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = parse_symbol_binding(input)?;
        Rc::try_unwrap(expr).map_err(|_| input.error("cannot extract symbol binding"))
    }
}
