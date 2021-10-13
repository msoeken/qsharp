use std::rc::Rc;

use proc_macro2::{Group, Punct, TokenStream};
use qsharp_ast::ast::{Parameter, TypeKind};
use quote::{format_ident, ToTokens, TokenStreamExt};

use crate::ToRust;

#[derive(Debug, PartialEq)]
pub enum Tree<T> {
    Leaf(T),
    Branch(Vec<Tree<T>>),
}

impl<T> Tree<T> {
    pub fn leaf(element: impl Into<T>) -> Self {
        Self::Leaf(element.into())
    }

    pub fn branch(elements: Vec<Tree<T>>) -> Self {
        Self::Branch(elements)
    }
}

pub trait ToTokensOne {
    fn to_tokens_one(&self, tokens: &mut proc_macro2::TokenStream);
}

impl ToTokensOne for String {
    fn to_tokens_one(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = format_ident!("{}", self);
        ident.to_tokens(tokens);
    }
}

impl<T: ToRust> ToTokensOne for Rc<T> {
    fn to_tokens_one(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(self.to_rust());
    }
}

impl<T: ToTokensOne> ToTokens for Tree<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Tree::Leaf(item) => {
                item.to_tokens_one(tokens);
            }
            Tree::Branch(items) => {
                let mut stream = TokenStream::new();

                for (idx, item) in items.iter().enumerate() {
                    if idx > 0 {
                        stream.append(Punct::new(',', proc_macro2::Spacing::Alone));
                    }
                    item.to_tokens(&mut stream);
                }

                tokens.append(Group::new(proc_macro2::Delimiter::Parenthesis, stream));
            }
        }
    }
}

pub fn unzip_parameters(param: &Rc<Parameter>) -> (Tree<String>, Tree<Rc<TypeKind>>) {
    match param.as_ref() {
        Parameter::Item(name, kind) => (Tree::leaf(name), Tree::leaf(kind.clone())),
        Parameter::Tuple(items) => {
            let mut names = vec![];
            let mut kinds = vec![];

            for (name, kind) in items.iter().map(unzip_parameters) {
                names.push(name);
                kinds.push(kind);
            }

            (Tree::branch(names), Tree::branch(kinds))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use qsharp_ast::ast::{Parameter, TypeKind};
    use quote::quote;

    #[test]
    fn single_parameter() {
        let param = Parameter::item("a", TypeKind::int());
        let expected = (Tree::leaf("a"), Tree::leaf(TypeKind::int()));

        assert_eq!(unzip_parameters(&param), expected);
    }

    #[test]
    fn non_nested_tuple() {
        let param = Parameter::tuple(vec![
            Parameter::item("a", TypeKind::int()),
            Parameter::item("b", TypeKind::bool()),
        ]);
        let expected = (
            Tree::branch(vec![Tree::leaf("a"), Tree::leaf("b")]),
            Tree::branch(vec![
                Tree::leaf(TypeKind::int()),
                Tree::leaf(TypeKind::bool()),
            ]),
        );

        assert_eq!(unzip_parameters(&param), expected);
    }

    #[test]
    fn nested_tuple() {
        let param = Parameter::tuple(vec![
            Parameter::tuple(vec![
                Parameter::item("a", TypeKind::int()),
                Parameter::item("b", TypeKind::bool()),
            ]),
            Parameter::tuple(vec![
                Parameter::item("c", TypeKind::qubit()),
                Parameter::item("d", TypeKind::result()),
            ]),
        ]);
        let expected = (
            Tree::branch(vec![
                Tree::branch(vec![Tree::leaf("a"), Tree::leaf("b")]),
                Tree::branch(vec![Tree::leaf("c"), Tree::leaf("d")]),
            ]),
            Tree::branch(vec![
                Tree::branch(vec![
                    Tree::leaf(TypeKind::int()),
                    Tree::leaf(TypeKind::bool()),
                ]),
                Tree::branch(vec![
                    Tree::leaf(TypeKind::qubit()),
                    Tree::leaf(TypeKind::result()),
                ]),
            ]),
        );

        assert_eq!(unzip_parameters(&param), expected);
    }

    #[test]
    fn leaf_code_gen() {
        let leaf = Tree::<String>::leaf("a");
        let code = quote! { #leaf };

        assert_eq!("a", code.to_string());

        let leaf = Tree::<Rc<TypeKind>>::leaf(TypeKind::bool());
        let code = quote! { #leaf };

        assert_eq!("bool", code.to_string());
    }

    #[test]
    fn non_nested_tuple_code_gen() {
        let tuple = Tree::<String>::branch(vec![Tree::leaf("a"), Tree::leaf("b")]);
        let code = quote! { #tuple };

        assert_eq!("(a , b)", code.to_string());

        let tuple = Tree::<Rc<TypeKind>>::branch(vec![
            Tree::leaf(TypeKind::int()),
            Tree::leaf(TypeKind::bool()),
        ]);
        let code = quote! { #tuple };

        assert_eq!("(i64 , bool)", code.to_string());
    }

    #[test]
    fn nested_tuple_code_gen() {
        let tuple = Tree::<String>::branch(vec![
            Tree::branch(vec![Tree::leaf("a"), Tree::leaf("b")]),
            Tree::branch(vec![Tree::leaf("c"), Tree::leaf("d")]),
        ]);

        let code = quote! { #tuple };

        assert_eq!("((a , b) , (c , d))", code.to_string());

        let tuple = Tree::<Rc<TypeKind>>::branch(vec![
            Tree::branch(vec![
                Tree::leaf(TypeKind::int()),
                Tree::leaf(TypeKind::bool()),
            ]),
            Tree::branch(vec![
                Tree::leaf(TypeKind::qubit()),
                Tree::leaf(TypeKind::result()),
            ]),
        ]);
        let code = quote! { #tuple };

        assert_eq!("((i64 , bool) , (usize , bool))", code.to_string());
    }
}
