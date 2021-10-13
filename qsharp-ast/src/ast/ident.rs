use syn::{parse::Parse, Result};

// TODO ignore Q# keywords
/// Q# identifier (incl. Q# keywords and excl. Rust keywords)
#[derive(Debug, PartialEq)]
pub struct Ident {
    value: String,
}

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.value
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self { value }
    }
}

impl From<Ident> for String {
    fn from(ident: Ident) -> Self {
        ident.value
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.value.clone()
    }
}

impl Parse for Ident {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                return Ok((Self::from(ident.to_string()), rest));
            }
            Err(cursor.error("expected identifier"))
        })
    }
}
