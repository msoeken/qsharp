use itertools::Itertools;
use proc_macro2::Ident;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Paren,
    Result, Token,
};

use crate::ast::{kw, utilities::peek_and_consume, Scope};

/// Specialization parameter inside provided specialization generator
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SpecializationParameter {
    Identifier(String),
    Dots,
}

/// Generator for specialization (e.g., `auto`, `self`, or provided via explicit scope)
#[derive(Debug, PartialEq)]
pub enum SpecializationGenerator {
    Auto,
    Self_,
    Invert,
    Distribute,
    Intrinsic,
    Provided(Option<Vec<SpecializationParameter>>, Scope),
}

impl SpecializationGenerator {
    /// Returns the scope of a generator, if it provides one, otherwise `None`
    pub fn scope(&self) -> Option<&Scope> {
        match self {
            Self::Provided(_, scope) => Some(scope),
            _ => None,
        }
    }
}

/// One of the four specialization kinds (`body`, `adjoint`, `controlled`, `controlled adjoint`)
#[derive(Debug, PartialEq, Eq, PartialOrd, Clone, Copy)]
pub enum SpecializationKind {
    Body,
    Adjoint,
    Controlled,
    ControlledAdjoint,
}

/// A specialization inside a Q# operation (e.g., `body { ... }` or `adjoint auto`)
#[derive(Debug, PartialEq)]
pub struct Specialization {
    kind: SpecializationKind,
    generator: SpecializationGenerator,
}

impl PartialOrd for Specialization {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.kind.partial_cmp(&other.kind)
    }
}

impl Specialization {
    /// Generates `body` specialization with provided scope
    pub fn body(scope: Scope) -> Self {
        Self {
            kind: SpecializationKind::Body,
            generator: SpecializationGenerator::Provided(None, scope),
        }
    }

    /// Generates `adjoint` specialization with provided scope
    pub fn adjoint(scope: Scope) -> Self {
        Self {
            kind: SpecializationKind::Adjoint,
            generator: SpecializationGenerator::Provided(None, scope),
        }
    }

    /// Generates `controlled` specialization with provided scope and `ctls` argument
    pub fn controlled(scope: Scope) -> Self {
        Self {
            kind: SpecializationKind::Controlled,
            generator: SpecializationGenerator::Provided(
                Some(vec![SpecializationParameter::Identifier("ctls".into())]),
                scope,
            ),
        }
    }

    /// Generates `controlled adjoint` specialization with provided scope and `ctls` argument
    pub fn controlled_adjoint(scope: Scope) -> Self {
        Self {
            kind: SpecializationKind::ControlledAdjoint,
            generator: SpecializationGenerator::Provided(
                Some(vec![SpecializationParameter::Identifier("ctls".into())]),
                scope,
            ),
        }
    }

    /// Generates `body` specialization with generator
    pub fn body_with(generator: SpecializationGenerator) -> Self {
        Self {
            kind: SpecializationKind::Body,
            generator,
        }
    }

    /// Generates `adjoint` specialization with generator
    pub fn adjoint_with(generator: SpecializationGenerator) -> Self {
        Self {
            kind: SpecializationKind::Adjoint,
            generator,
        }
    }

    /// Generates `controlled` specialization with generator
    pub fn controlled_with(generator: SpecializationGenerator) -> Self {
        Self {
            kind: SpecializationKind::Controlled,
            generator,
        }
    }

    /// Generates `controlled_adjoint` specialization with generator
    pub fn controlled_adjoint_with(generator: SpecializationGenerator) -> Self {
        Self {
            kind: SpecializationKind::ControlledAdjoint,
            generator,
        }
    }

    pub fn kind(&self) -> SpecializationKind {
        self.kind
    }

    pub fn generator(&self) -> &SpecializationGenerator {
        &self.generator
    }

    pub fn generator_mut(&mut self) -> &mut SpecializationGenerator {
        &mut self.generator
    }

    pub fn set_generator(&mut self, generator: SpecializationGenerator) {
        self.generator = generator;
    }
}

impl Parse for SpecializationParameter {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![.]) {
            input.parse::<Token![.]>()?;
            input.parse::<Token![.]>()?;
            input.parse::<Token![.]>()?;
            Ok(SpecializationParameter::Dots)
        } else {
            let ident: Ident = input.parse()?;
            Ok(SpecializationParameter::Identifier(ident.to_string()))
        }
    }
}

impl Parse for SpecializationGenerator {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::auto) {
            input.parse::<kw::auto>()?;
            input.parse::<Token![;]>()?;
            Ok(SpecializationGenerator::Auto)
        } else if input.peek(Token![self]) {
            input.parse::<Token![self]>()?;
            input.parse::<Token![;]>()?;
            Ok(SpecializationGenerator::Self_)
        } else if input.peek(kw::invert) {
            input.parse::<kw::invert>()?;
            input.parse::<Token![;]>()?;
            Ok(SpecializationGenerator::Invert)
        } else if input.peek(kw::distribute) {
            input.parse::<kw::distribute>()?;
            input.parse::<Token![;]>()?;
            Ok(SpecializationGenerator::Distribute)
        } else if input.peek(kw::intrinsic) {
            input.parse::<kw::intrinsic>()?;
            input.parse::<Token![;]>()?;
            Ok(SpecializationGenerator::Intrinsic)
        } else {
            let tuple = if input.peek(Paren) {
                let buffer;
                parenthesized!(buffer in input);
                let items: Punctuated<SpecializationParameter, Token![,]> =
                    Punctuated::parse_separated_nonempty(&buffer)?;
                Some(items.into_iter().collect_vec())
            } else {
                None
            };
            let scope = input.parse()?;
            Ok(SpecializationGenerator::Provided(tuple, scope))
        }
    }
}

impl Parse for Specialization {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut is_body = 0;
        let mut is_adjoint = 0;
        let mut is_controlled = 0;

        loop {
            if peek_and_consume(input, kw::body)? {
                is_body += 1;
            } else if peek_and_consume(input, kw::adjoint)? {
                is_adjoint += 1;
            } else if peek_and_consume(input, kw::controlled)? {
                is_controlled += 1;
            } else {
                break;
            }
        }

        let kind = match (is_body, is_adjoint, is_controlled) {
            (1, 0, 0) => SpecializationKind::Body,
            (0, 1, 0) => SpecializationKind::Adjoint,
            (0, 0, 1) => SpecializationKind::Controlled,
            (0, 1, 1) => SpecializationKind::ControlledAdjoint,
            _ => {
                return Err(input.error("invalid specialization keyword"));
            }
        };

        let generator: SpecializationGenerator = input.parse()?;

        Ok(Specialization { kind, generator })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_specialization(s: &str) -> Result<Specialization> {
        syn::parse_str(s)
    }

    #[test]
    fn test_specializations() -> Result<()> {
        parse_specialization("body intrinsic;")?;
        parse_specialization("adjoint controlled distribute;")?;

        Ok(())
    }
}
