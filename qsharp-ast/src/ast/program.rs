use crate::{
    analysis::normalize::Normalizer,
    ast::{utilities::parse_many, Namespace},
    utilities::VisitorMut,
};

use syn::{
    parse::{Parse, ParseStream},
    Result,
};

/// A Q# program is a collection of Q# namespaces
pub struct Program {
    pub(crate) namespaces: Vec<Namespace>,
}

impl Program {
    pub fn namespaces(&self) -> &[Namespace] {
        &self.namespaces
    }

    pub fn namespaces_mut(&mut self) -> &mut [Namespace] {
        &mut self.namespaces
    }

    pub fn normalize(&mut self) {
        Normalizer {}.visit_program(self);
    }
}

impl Parse for Program {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            namespaces: parse_many(input)?,
        })
    }
}
