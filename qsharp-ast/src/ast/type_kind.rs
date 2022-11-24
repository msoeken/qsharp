use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::{Delimiter, Ident};
use syn::{
    bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Result, Token,
};

use crate::ast::{
    characteristics::parse_characteristics, kw, qualified_name::parse_qualified_name_without_head,
    utilities::peek_and_consume, Characteristics, QualifiedName,
};

use super::utilities::peek_and_consume_single_quote;

/// A type parameter that can be provided to generic expression (e.g., `'A`)
#[derive(Debug, PartialEq, Eq)]
pub struct TypeParameter {
    name: String,
}

impl TypeParameter {
    pub fn new(name: &str) -> Self {
        Self { name: name.into() }
    }
}

impl Parse for TypeParameter {
    fn parse(input: ParseStream) -> Result<Self> {
        if !peek_and_consume_single_quote(input)? {
            Err(input.error("cannot parse type parameter"))
        } else {
            Ok(Self {
                name: input.parse::<Ident>()?.to_string(),
            })
        }
    }
}

/// An item of a tuple type (which can have a name when used in type declarations)
#[derive(Debug, PartialEq)]
pub enum TypeTupleItem {
    Simple(Rc<TypeKind>),
    Named(String, Rc<TypeKind>),
}

impl TypeTupleItem {
    pub fn simple(kind: Rc<TypeKind>) -> Self {
        Self::Simple(kind)
    }

    pub fn named(name: &str, kind: Rc<TypeKind>) -> Self {
        Self::Named(name.into(), kind)
    }
}

impl From<Rc<TypeKind>> for TypeTupleItem {
    fn from(kind: Rc<TypeKind>) -> Self {
        Self::Simple(kind)
    }
}

impl Parse for TypeTupleItem {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(syn::Ident::peek_any) && input.peek2(Token![:]) {
            let name: crate::ast::Ident = input.parse()?;
            input.parse::<Token![:]>()?;
            let kind = parse_type(input)?;
            Ok(Self::Named(name.to_string(), kind))
        } else {
            let kind = parse_type(input)?;
            Ok(Self::Simple(kind))
        }
    }
}

/// Any Q# type (e.g., `Bool`, `Int`, `Qubit[]`, `Int => Unit`, `Qubit -> Result`)
#[derive(Debug, PartialEq)]
pub enum TypeKind {
    BigInt,
    Bool,
    Double,
    Int,
    Pauli,
    Qubit,
    Range,
    Result,
    String,
    Unit,
    Missing,
    TypeParameter(TypeParameter),
    UDT(QualifiedName),
    Array(Rc<Self>),
    Tuple(Vec<TypeTupleItem>),
    Function(Rc<Self>, Rc<Self>, Option<Rc<Characteristics>>),
    Operation(Rc<Self>, Rc<Self>, Option<Rc<Characteristics>>),
}

impl TypeKind {
    pub fn big_int() -> Rc<Self> {
        Rc::new(Self::BigInt)
    }

    pub fn bool() -> Rc<Self> {
        Rc::new(Self::Bool)
    }

    pub fn double() -> Rc<Self> {
        Rc::new(Self::Double)
    }

    pub fn int() -> Rc<Self> {
        Rc::new(Self::Int)
    }

    pub fn pauli() -> Rc<Self> {
        Rc::new(Self::Pauli)
    }

    pub fn qubit() -> Rc<Self> {
        Rc::new(Self::Qubit)
    }

    pub fn range() -> Rc<Self> {
        Rc::new(Self::Range)
    }

    pub fn result() -> Rc<Self> {
        Rc::new(Self::Result)
    }

    pub fn string() -> Rc<Self> {
        Rc::new(Self::String)
    }

    pub fn unit() -> Rc<Self> {
        Rc::new(Self::Unit)
    }

    pub fn missing() -> Rc<Self> {
        Rc::new(Self::Missing)
    }

    pub fn type_parameter(parameter: TypeParameter) -> Rc<Self> {
        Rc::new(Self::TypeParameter(parameter))
    }

    pub fn udt(name: QualifiedName) -> Rc<Self> {
        Rc::new(Self::UDT(name))
    }

    pub fn array(inner: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Array(inner))
    }

    pub fn tuple(inner: Vec<TypeTupleItem>) -> Rc<Self> {
        Rc::new(Self::Tuple(inner))
    }

    pub fn function(from: Rc<Self>, to: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Function(from, to, None))
    }

    pub fn function_with_characteristics(
        from: Rc<Self>,
        to: Rc<Self>,
        characteristics: Rc<Characteristics>,
    ) -> Rc<Self> {
        Rc::new(Self::Function(from, to, Some(characteristics)))
    }

    pub fn operation(from: Rc<Self>, to: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Operation(from, to, None))
    }

    pub fn operation_with_characteristics(
        from: Rc<Self>,
        to: Rc<Self>,
        characteristics: Rc<Characteristics>,
    ) -> Rc<Self> {
        Rc::new(Self::Operation(from, to, Some(characteristics)))
    }
}

pub(crate) fn parse_type(input: ParseStream) -> Result<Rc<TypeKind>> {
    // TODO type parameter types
    if peek_and_consume(input, Token![_])? {
        return Ok(TypeKind::missing());
    }

    // We first derive a type prefix.  This might be followed by some token
    // that indicates a special type, e.g., array, tuple.
    let mut prefix = if input.peek(syn::token::Paren) {
        let buffer;
        parenthesized!(buffer in input);

        let items: Punctuated<TypeTupleItem, Token![,]> =
            Punctuated::parse_separated_nonempty(&buffer)?;
        let items = items.into_iter().collect_vec();

        if items.len() == 1 {
            if let TypeTupleItem::Simple(ref kind) = items[0] {
                kind.clone()
            } else {
                TypeKind::tuple(items)
            }
        } else {
            TypeKind::tuple(items)
        }
    } else if peek_and_consume_single_quote(input)? {
        TypeKind::type_parameter(TypeParameter::new(
            input.parse::<Ident>()?.to_string().as_str(),
        ))
    } else {
        // TODO more efficient parsing?
        let ident: crate::ast::Ident = input.parse()?;
        match ident.to_string().as_str() {
            "BigInt" => TypeKind::big_int(),
            "Bool" => TypeKind::bool(),
            "Double" => TypeKind::double(),
            "Int" => TypeKind::int(),
            "Pauli" => TypeKind::pauli(),
            "Qubit" => TypeKind::qubit(),
            "Range" => TypeKind::range(),
            "Result" => TypeKind::result(),
            "String" => TypeKind::string(),
            "Unit" => TypeKind::unit(),
            _ => TypeKind::udt(parse_qualified_name_without_head(input, ident)?),
        }
    };

    // Lookahead to check for array types
    loop {
        // we use the lower level cursor API to distinguish from `new Type[expr]` expression
        if let Some((inner, _, _)) = input.cursor().group(Delimiter::Bracket) {
            if inner.eof() {
                let _in_brackets;
                bracketed![_in_brackets in input];
                prefix = TypeKind::array(prefix);
                continue;
            }
        }
        break;
    }

    // Repeatedly check whether we can extend the prefix as tuple type, function
    // type, or operation type, exit the loop once none of the cases applies
    loop {
        // Lookahead for function type
        if input.peek(Token![-]) && input.peek2(Token![>]) {
            input.parse::<Token![-]>()?;
            input.parse::<Token![>]>()?;

            let next_type = parse_type(input)?;
            prefix = if peek_and_consume(input, kw::is)? {
                TypeKind::function_with_characteristics(
                    prefix,
                    next_type,
                    parse_characteristics(input)?,
                )
            } else {
                TypeKind::function(prefix, next_type)
            };
            continue;
        }

        // Lookahead for operation type
        if input.peek(Token![=]) && input.peek2(Token![>]) {
            input.parse::<Token![=]>()?;
            input.parse::<Token![>]>()?;

            let next_type = parse_type(input)?;
            prefix = if peek_and_consume(input, kw::is)? {
                TypeKind::operation_with_characteristics(
                    prefix,
                    next_type,
                    parse_characteristics(input)?,
                )
            } else {
                TypeKind::operation(prefix, next_type)
            };
            continue;
        }

        break;
    }

    Ok(prefix)
}

impl Parse for TypeKind {
    fn parse(input: ParseStream) -> Result<Self> {
        let t = parse_type(input)?;
        Rc::try_unwrap(t).map_err(|_| input.error("cannot extract type"))
    }
}

#[cfg(test)]
mod tests {
    use syn::Result;

    use super::*;

    fn parse_type(s: &str) -> Result<TypeKind> {
        syn::parse_str(s)
    }

    #[test]
    fn test_base_types() -> Result<()> {
        assert!(matches!(parse_type("BigInt")?, TypeKind::BigInt));
        assert!(matches!(parse_type("Bool")?, TypeKind::Bool));
        assert!(matches!(parse_type("Double")?, TypeKind::Double));
        assert!(matches!(parse_type("Int")?, TypeKind::Int));
        assert!(matches!(parse_type("Pauli")?, TypeKind::Pauli));
        assert!(matches!(parse_type("Qubit")?, TypeKind::Qubit));
        assert!(matches!(parse_type("Range")?, TypeKind::Range));
        assert!(matches!(parse_type("Result")?, TypeKind::Result));
        assert!(matches!(parse_type("String")?, TypeKind::String));
        assert!(matches!(parse_type("Unit")?, TypeKind::Unit));

        Ok(())
    }

    #[test]
    fn test_array_types() -> Result<()> {
        assert_eq!(parse_type("Int[]")?, *TypeKind::array(TypeKind::int()));
        assert_eq!(
            parse_type("Pauli[][]")?,
            *TypeKind::array(TypeKind::array(TypeKind::pauli()))
        );
        assert_eq!(
            parse_type("'Op[]")?,
            *TypeKind::array(TypeKind::type_parameter(TypeParameter::new("Op")))
        );

        Ok(())
    }

    #[test]
    fn test_tuple_type() -> Result<()> {
        assert_eq!(
            parse_type("(Int, Bool)")?,
            *TypeKind::tuple(vec![TypeKind::int().into(), TypeKind::bool().into()])
        );
        assert_eq!(
            parse_type("(Int[], (Int, Int)[])")?,
            *TypeKind::tuple(vec![
                TypeKind::array(TypeKind::int()).into(),
                TypeKind::array(TypeKind::tuple(vec![
                    TypeKind::int().into(),
                    TypeKind::int().into()
                ]))
                .into()
            ])
        );
        assert_eq!(
            parse_type("(Int, Bool, String)")?,
            *TypeKind::tuple(vec![
                TypeKind::int().into(),
                TypeKind::bool().into(),
                TypeKind::string().into()
            ])
        );
        assert_eq!(
            parse_type("(Int[], Bool, String[])")?,
            *TypeKind::tuple(vec![
                TypeKind::array(TypeKind::int()).into(),
                TypeKind::bool().into(),
                TypeKind::array(TypeKind::string()).into()
            ])
        );
        assert_eq!(
            parse_type("(Int[], Bool, String[])[]")?,
            *TypeKind::array(TypeKind::tuple(vec![
                TypeKind::array(TypeKind::int()).into(),
                TypeKind::bool().into(),
                TypeKind::array(TypeKind::string()).into()
            ]))
        );
        assert_eq!(
            parse_type("((Int[], Qubit), (Pauli, String)[])[][]")?,
            *TypeKind::array(TypeKind::array(TypeKind::tuple(vec![
                TypeKind::tuple(vec![
                    TypeKind::array(TypeKind::int()).into(),
                    TypeKind::qubit().into()
                ])
                .into(),
                TypeKind::array(TypeKind::tuple(vec![
                    TypeKind::pauli().into(),
                    TypeKind::string().into()
                ]))
                .into()
            ])))
        );

        Ok(())
    }

    #[test]
    fn test_function_type() -> Result<()> {
        assert_eq!(
            parse_type("(Int -> Int)")?,
            *TypeKind::function(TypeKind::int(), TypeKind::int())
        );
        assert_eq!(
            parse_type("(Int -> (Int -> Int))")?,
            *TypeKind::function(
                TypeKind::int(),
                TypeKind::function(TypeKind::int(), TypeKind::int())
            )
        );
        assert_eq!(
            parse_type("((Int -> Int) -> Int)")?,
            *TypeKind::function(
                TypeKind::function(TypeKind::int(), TypeKind::int()),
                TypeKind::int()
            )
        );
        assert_eq!(
            parse_type("Int -> Int")?,
            *TypeKind::function(TypeKind::int(), TypeKind::int())
        );
        assert_eq!(
            parse_type("Int -> Int -> Int")?,
            *TypeKind::function(
                TypeKind::int(),
                TypeKind::function(TypeKind::int(), TypeKind::int())
            )
        );
        assert_eq!(
            parse_type("Int -> (Int -> Int)")?,
            *TypeKind::function(
                TypeKind::int(),
                TypeKind::function(TypeKind::int(), TypeKind::int())
            )
        );
        assert_eq!(
            parse_type("(Int -> Int) -> Int")?,
            *TypeKind::function(
                TypeKind::function(TypeKind::int(), TypeKind::int()),
                TypeKind::int()
            )
        );
        assert_eq!(
            parse_type("(Int -> Int, Int)")?,
            *TypeKind::tuple(vec![
                TypeKind::function(TypeKind::int(), TypeKind::int()).into(),
                TypeKind::int().into()
            ])
        );
        assert_eq!(
            parse_type("(Int, Int -> Int)")?,
            *TypeKind::tuple(vec![
                TypeKind::int().into(),
                TypeKind::function(TypeKind::int(), TypeKind::int()).into()
            ])
        );
        assert_eq!(
            parse_type("(Int, Int[] -> Int)[]")?,
            *TypeKind::array(TypeKind::tuple(vec![
                TypeKind::int().into(),
                TypeKind::function(TypeKind::array(TypeKind::int()), TypeKind::int()).into()
            ]))
        );

        assert_eq!(
            parse_type("(Int, Int[] => Int is Adj + Ctl)[]")?,
            *TypeKind::array(TypeKind::tuple(vec![
                TypeKind::int().into(),
                TypeKind::operation_with_characteristics(
                    TypeKind::array(TypeKind::int()),
                    TypeKind::int(),
                    Characteristics::union(Characteristics::adj(), Characteristics::ctl())
                )
                .into()
            ]))
        );

        Ok(())
    }

    #[test]
    fn test_type_parameter() -> Result<()> {
        syn::parse_str::<TypeParameter>("'A")?;
        syn::parse_str::<TypeParameter>("'B")?;

        Ok(())
    }

    #[test]
    fn test_named_and_simple() -> Result<()> {
        assert!(parse_type("a : Bool").is_err());

        assert_eq!(
            parse_type("(a : Bool)")?,
            *TypeKind::tuple(vec![TypeTupleItem::named("a", TypeKind::bool()),])
        );

        assert_eq!(
            parse_type("(a : Bool, b : Int, String)")?,
            *TypeKind::tuple(vec![
                TypeTupleItem::named("a", TypeKind::bool()),
                TypeTupleItem::named("b", TypeKind::int()),
                TypeKind::string().into(),
            ])
        );
        Ok(())
    }
}
