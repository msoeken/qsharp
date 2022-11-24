use std::iter::repeat;

use proc_macro2::{Ident, TokenTree};
use syn::{
    parse::{Parse, ParseStream, Peek},
    Error, Result, Token,
};

pub fn expect_ident(ident: Ident, name: &str) -> Result<()> {
    if ident != name {
        Err(Error::new(ident.span(), format!("expected `{}`", name)))
    } else {
        Ok(())
    }
}

pub fn parse_many<T: Parse>(input: ParseStream) -> Result<Vec<T>> {
    repeat(())
        .take_while(|_| !input.is_empty())
        .map(|_| input.parse())
        .collect()
}

pub fn peek_and_consume<T: Peek>(input: ParseStream, token: T) -> Result<bool>
where
    T::Token: Parse,
{
    Ok(if input.peek(token) {
        input.parse::<T::Token>()?;
        true
    } else {
        false
    })
}

pub fn peek_and_consume_single_quote(input: ParseStream) -> Result<bool> {
    if let Some((TokenTree::Punct(punct), _)) = input.cursor().token_tree() {
        if punct.as_char() == '\'' {
            input.parse::<TokenTree>()?;
            return Ok(true);
        }
    }

    Ok(false)
}

pub fn peek_and_consume_ellipsis(input: ParseStream) -> Result<bool> {
    if input.peek(Token![.]) && input.peek2(Token![.]) && input.peek3(Token![.]) {
        input.parse::<Token![.]>()?;
        input.parse::<Token![.]>()?;
        input.parse::<Token![.]>()?;
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn has_extra_parens<PredicateFn>(input: ParseStream, predicate: PredicateFn) -> bool
where
    PredicateFn: Fn(TokenTree) -> bool,
{
    if let Some((into, _, _)) = input.cursor().group(proc_macro2::Delimiter::Parenthesis) {
        into.token_stream().into_iter().any(predicate)
    } else {
        false
    }
}
