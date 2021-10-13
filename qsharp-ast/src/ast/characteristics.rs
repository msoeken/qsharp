use std::rc::Rc;

use proc_macro2::Ident;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Paren,
    Result, Token,
};

/// Characteristics such as `Adj` and `Ctl`, as well as their union and intersection
#[derive(Debug, PartialEq)]
pub enum Characteristics {
    Adj,
    Ctl,
    Union(Rc<Self>, Rc<Self>),
    Intersection(Rc<Self>, Rc<Self>),
}

impl Characteristics {
    pub fn adj() -> Rc<Self> {
        Rc::new(Self::Adj)
    }

    pub fn ctl() -> Rc<Self> {
        Rc::new(Self::Ctl)
    }

    pub fn union(lhs: Rc<Self>, rhs: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Union(lhs, rhs))
    }

    pub fn intersection(lhs: Rc<Self>, rhs: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Intersection(lhs, rhs))
    }
}

fn parse_characteristics_terminal(input: ParseStream) -> Result<Rc<Characteristics>> {
    if input.peek(Paren) {
        let buffer;
        parenthesized!(buffer in input);
        parse_characteristics_union(&buffer)
    } else if input.peek(syn::Ident) {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "Adj" => Ok(Characteristics::adj()),
            "Ctl" => Ok(Characteristics::ctl()),
            other => Err(input.error(format!("invalid characteristics {}", other))),
        }
    } else {
        Err(input.error("unexpected token in parsing characteristics"))
    }
}

fn parse_characteristics_union(input: ParseStream) -> Result<Rc<Characteristics>> {
    let terms: Punctuated<Rc<Characteristics>, Token![+]> =
        Punctuated::parse_separated_nonempty_with(input, parse_characteristics_intersection)?;

    // we know iter is non empty
    Ok(terms.into_iter().reduce(Characteristics::union).unwrap())
}

fn parse_characteristics_intersection(input: ParseStream) -> Result<Rc<Characteristics>> {
    let terms: Punctuated<Rc<Characteristics>, Token![*]> =
        Punctuated::parse_separated_nonempty_with(input, parse_characteristics_terminal)?;

    // we know iter is non empty
    Ok(terms
        .into_iter()
        .reduce(Characteristics::intersection)
        .unwrap())
}

pub(crate) fn parse_characteristics(input: ParseStream) -> Result<Rc<Characteristics>> {
    // We use the precedence climbing parsing method (see, e.g., https://en.wikipedia.org/wiki/Operator-precedence_parser)
    parse_characteristics_union(input)
}

impl Parse for Characteristics {
    fn parse(input: ParseStream) -> Result<Self> {
        let characteristics = parse_characteristics(input)?;
        Rc::try_unwrap(characteristics).map_err(|_| input.error("cannot extract characteristics"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_characteristics(s: &str) -> Result<Characteristics> {
        syn::parse_str(s)
    }

    #[test]
    fn test_simple() -> Result<()> {
        assert_eq!(parse_characteristics("Adj")?, *Characteristics::adj());
        assert_eq!(parse_characteristics("Ctl")?, *Characteristics::ctl());
        assert_eq!(parse_characteristics("(Adj)")?, *Characteristics::adj());
        assert_eq!(parse_characteristics("(Ctl)")?, *Characteristics::ctl());

        assert_eq!(
            parse_characteristics("Adj + Ctl")?,
            *Characteristics::union(Characteristics::adj(), Characteristics::ctl())
        );
        assert_eq!(
            parse_characteristics("Adj * Ctl")?,
            *Characteristics::intersection(Characteristics::adj(), Characteristics::ctl())
        );
        assert_eq!(
            parse_characteristics("Ctl + Adj")?,
            *Characteristics::union(Characteristics::ctl(), Characteristics::adj())
        );
        assert_eq!(
            parse_characteristics("Ctl * Adj")?,
            *Characteristics::intersection(Characteristics::ctl(), Characteristics::adj())
        );

        Ok(())
    }

    #[test]
    fn test_precedence() -> Result<()> {
        // (Adj + Ctl) + Adj
        assert_eq!(
            parse_characteristics("Adj + Ctl + Adj")?,
            *Characteristics::union(
                Characteristics::union(Characteristics::adj(), Characteristics::ctl()),
                Characteristics::adj()
            )
        );

        // (Adj * Ctl) * Adj
        assert_eq!(
            parse_characteristics("Adj * Ctl * Adj")?,
            *Characteristics::intersection(
                Characteristics::intersection(Characteristics::adj(), Characteristics::ctl()),
                Characteristics::adj()
            )
        );

        // Adj + (Ctl * Adj)
        assert_eq!(
            parse_characteristics("Adj + Ctl * Adj")?,
            *Characteristics::union(
                Characteristics::adj(),
                Characteristics::intersection(Characteristics::ctl(), Characteristics::adj()),
            )
        );

        // Adj + (Ctl * Adj)
        assert_eq!(
            parse_characteristics("Adj * Ctl + Adj")?,
            *Characteristics::union(
                Characteristics::intersection(Characteristics::adj(), Characteristics::ctl()),
                Characteristics::adj(),
            )
        );

        Ok(())
    }
}
