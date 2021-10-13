use proc_macro2::Span;
use syn::{
    Result,
    __private::IntoSpans,
    buffer::Cursor,
    custom_keyword,
    parse::{Parse, ParseStream},
};

custom_keyword!(operation);
custom_keyword!(function);
custom_keyword!(newtype);
custom_keyword!(namespace);
custom_keyword!(open);
custom_keyword!(internal);

custom_keyword!(is);
custom_keyword!(not);
custom_keyword!(and);
custom_keyword!(or);
custom_keyword!(size);
custom_keyword!(new);

custom_keyword!(body);
custom_keyword!(adjoint);
custom_keyword!(controlled);
custom_keyword!(Adjoint);
custom_keyword!(Controlled);

custom_keyword!(auto);
custom_keyword!(invert);
custom_keyword!(distribute);
custom_keyword!(intrinsic);

custom_keyword!(Zero);
custom_keyword!(One);
custom_keyword!(PauliI);
custom_keyword!(PauliX);
custom_keyword!(PauliY);
custom_keyword!(PauliZ);

custom_keyword!(mutable);
custom_keyword!(using);
custom_keyword!(borrow);
custom_keyword!(borrowing);
custom_keyword!(repeat);
custom_keyword!(until);
custom_keyword!(fixup);
custom_keyword!(fail);
custom_keyword!(set);
custom_keyword!(elif);
custom_keyword!(within);
custom_keyword!(apply);

pub struct CopyAndUpdate {
    pub span: Span,
}

#[doc(hidden)]
#[allow(dead_code, non_snake_case)]
pub fn CopyAndUpdate<__S: IntoSpans<[Span; 1]>>(span: __S) -> CopyAndUpdate {
    CopyAndUpdate {
        span: IntoSpans::into_spans(span)[0],
    }
}

impl Default for CopyAndUpdate {
    fn default() -> Self {
        CopyAndUpdate {
            span: Span::call_site(),
        }
    }
}

impl syn::token::CustomToken for CopyAndUpdate {
    fn peek(cursor: Cursor) -> bool {
        if let Some((ident, rest)) = cursor.ident() {
            if ident != "w" {
                false
            } else if let Some((punct, _)) = rest.punct() {
                punct.as_char() == '/'
            } else {
                false
            }
        } else {
            false
        }
    }

    fn display() -> &'static str {
        "`/w`"
    }
}

impl Parse for CopyAndUpdate {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if ident == "w" {
                    if let Some((punct, rest2)) = rest.punct() {
                        if punct.as_char() == '/' {
                            return Ok((Self { span: ident.span() }, rest2));
                        }
                    }
                }
            }

            Err(cursor.error("expected `w/`"))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    #[test]
    fn parse_copy_update() -> Result<()> {
        syn::parse_str::<CopyAndUpdate>("w/")?;

        Ok(())
    }
}
