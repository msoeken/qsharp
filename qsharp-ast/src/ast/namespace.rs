use syn::{
    braced,
    parse::{Parse, ParseStream},
    Attribute, Result, Token,
};

use crate::ast::{kw, utilities::parse_many, Callable, QualifiedName, TypeDeclaration};

use super::{
    callable::parse_callable_without_prefix,
    type_declaration::parse_type_declaration_without_prefix,
};

/// Namespace items (`operation`, `function`, `open` directives, and `newtype` declarations)
#[derive(Debug, PartialEq)]
pub enum NamespaceItem {
    OpenDirective(QualifiedName, Option<QualifiedName>),
    Callable(Callable),
    TypeDeclaration(TypeDeclaration),
}

/// Q# namespace `namespace Name { ... }`
#[derive(Debug, PartialEq)]
pub struct Namespace {
    pub(crate) name: QualifiedName,
    pub(crate) items: Vec<NamespaceItem>,
}

impl Namespace {
    pub fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub fn items(&self) -> &[NamespaceItem] {
        &self.items
    }

    pub fn items_mut(&mut self) -> &mut [NamespaceItem] {
        &mut self.items
    }
}

impl Parse for NamespaceItem {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(kw::open) {
            input.parse::<kw::open>()?;
            let name = input.parse()?;

            let alias = if input.peek(Token![as]) {
                input.parse::<Token![as]>()?;
                Some(input.parse()?)
            } else {
                None
            };

            input.parse::<Token![;]>()?;

            Ok(NamespaceItem::OpenDirective(name, alias))
        } else {
            let prefix = input.parse()?;
            if input.peek(kw::newtype) {
                Ok(NamespaceItem::TypeDeclaration(
                    parse_type_declaration_without_prefix(input, prefix)?,
                ))
            } else {
                Ok(NamespaceItem::Callable(parse_callable_without_prefix(
                    input, prefix,
                )?))
            }
        }
    }
}

impl Parse for Namespace {
    fn parse(input: ParseStream) -> Result<Self> {
        // TODO Don't ignore doc comments
        let _ = input.call(Attribute::parse_outer)?;

        input.parse::<kw::namespace>()?;

        let name = input.parse()?;

        let buffer;
        braced!(buffer in input);

        Ok(Self {
            name,
            items: parse_many(&buffer)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    fn parse_namespace(s: &str) -> Result<Namespace> {
        syn::parse_str(s)
    }

    #[test]
    fn test_multiple_operations() -> Result<()> {
        let ns =
            parse_namespace("namespace Ns { operation Foo(): Unit {} operation Bar(): Unit {} }")?;

        assert_eq!(ns.items.len(), 2);

        Ok(())
    }

    #[test]
    fn test_empty_with_doc_comments() -> Result<()> {
        let ns = parse_namespace("/// Doc comment\nnamespace Ns {  }")?;

        assert!(ns.items.is_empty());

        Ok(())
    }

    #[test]
    fn test_namespace1() -> Result<()> {
        parse_namespace(
            "namespace SubOps {\n\
                             operation SubOp1() : Unit { }\n\
                             operation SubOp2() : Unit { }\n\
                             operation SubOp3() : Unit { }\n\
                             operation SubOpCA1() : Unit is Ctl + Adj { }\n\
                             operation SubOpCA2() : Unit is Ctl + Adj { }\n\
                             operation SubOpCA3() : Unit is Ctl + Adj { }\n\
                         }",
        )?;

        Ok(())
    }

    #[test]
    fn test_namespace2() -> Result<()> {
        parse_namespace(
            "namespace Microsoft.Quantum.Testing.General {
                 operation Unitary (q : Qubit) : Unit {
                     body intrinsic;
                     adjoint auto;
                     controlled auto;
                     controlled adjoint auto;
                 }

                 operation M (q : Qubit) : Result {
                     body intrinsic;
                 }
             }",
        )?;

        Ok(())
    }

    #[test]
    fn test_namespace3() -> Result<()> {
        parse_namespace(
            "namespace Microsoft.Quantum.Testing.ClassicalControl {
                open SubOps;
            
                operation Foo() : Unit {
                    let r = Zero;
            
                    if (r == Zero) {
                        SubOp1();
                        SubOp2();
                        SubOp3();
                        let temp = 4;
                        using (q = Qubit()) {
                            let temp2 = q;
                        }
                    }
                }
            }",
        )?;

        Ok(())
    }

    #[test]
    fn test_namespace4() -> Result<()> {
        parse_namespace(
            "namespace Microsoft.Quantum.Testing.ClassicalControl {

                operation Foo() : Unit {
                    let r = Zero;
            
                    if (r == Zero) {
                        for (index in 0 .. 3) {
                            let temp = index;
                        }
            
                        repeat {
                            let success = true;
                        } until (success)
                        fixup {
                            let temp2 = 0;
                        }
                    }
                }
            }",
        )?;

        Ok(())
    }

    #[test]
    fn test_namespace5() -> Result<()> {
        parse_namespace(
            "namespace Microsoft.Quantum.Testing.ClassicalControl {
                open SubOps;
            
                operation Foo() : Unit {
                    let r = Zero;
                    if (r == Zero) {
                        SubOp1();
                    }
                }
            }",
        )?;

        Ok(())
    }

    #[test]
    fn test_namespace6() -> Result<()> {
        parse_namespace(
            "namespace Microsoft.Quantum.Testing.ClassicalControl {
                open SubOps;
            
                operation Foo() : Unit {
                    let r = Zero;
            
                    if (r == Zero) {
                        let temp = 2;
                    }
                }
            }",
        )?;

        Ok(())
    }

    #[test]
    fn test_namespace7() -> Result<()> {
        parse_namespace(
            "namespace Microsoft.Quantum.Testing.AccessModifiers {
                open Microsoft.Quantum.Testing.AccessModifiers.A;
                open Microsoft.Quantum.Testing.AccessModifiers.B as B;
                open Microsoft.Quantum.Testing.AccessModifiers.C;
            }",
        )?;

        Ok(())
    }
}
