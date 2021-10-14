use std::rc::Rc;

use proc_macro2::{Group, Punct, TokenStream};
use qsharp_ast::{
    ast::{
        Access, Callable, CallableBody, Parameter, SpecializationGenerator, SpecializationKind,
        SpecializationParameter, TypeKind,
    },
    utilities::Mapper,
};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};

pub(crate) fn visit_callable(
    mapper: &mut super::Codegen,
    callable: &Callable,
) -> proc_macro2::TokenStream {
    // TODO incomplete!

    // at this point we ignore callable.characteristics() and assume that it has
    // been correctly resolved with semantic analysis.  Also we expect that
    // all specializations are given explicitly and we do not have a
    // callable with a simple body.

    if callable.type_parameters().is_some() {
        todo!();
    }

    let prefix = callable.prefix();
    let return_type = mapper.visit_type_kind(callable.return_type());
    let modifier = match prefix.access() {
        Access::Internal => quote! {},
        Access::Default => quote! { pub },
    };
    let parameters = mapper.visit_parameter(callable.parameters());

    // add new variable scope for parameters
    mapper.push_scope();
    mapper.add_parameter_to_symbol_table(callable.parameters());

    let code = if let CallableBody::Multiple(specializations) = callable.body() {
        let mut code = quote! {};

        for specialization in specializations {
            if let SpecializationGenerator::Provided(args, scope) = specialization.generator() {
                match specialization.kind() {
                    SpecializationKind::Body => {
                        let name = format_ident!("{}", callable.name());
                        let body = mapper.visit_scope(scope);

                        code = quote! {
                            #code
                            #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #parameters) -> #return_type #body
                        }
                    }
                    SpecializationKind::Adjoint => {
                        let name = format_ident!("{}_adj", callable.name());
                        let body = mapper.visit_scope(scope);

                        code = quote! {
                            #code
                            #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #parameters) -> #return_type #body
                        }
                    }
                    SpecializationKind::Controlled => {
                        // get parameter name for controls from args (normalization guarantees that it exists)
                        let ctls_name = match args {
                            Some(vec) => match &vec[0] {
                                SpecializationParameter::Identifier(name) => {
                                    format_ident!("{}", name)
                                }
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };

                        let name = format_ident!("{}_ctl", callable.name());
                        let body = mapper.visit_scope(scope);
                        let qubit_array_type =
                            mapper.visit_type_kind(TypeKind::array(TypeKind::qubit()).as_ref());
                        let parameters =
                            generate_parameters_directly(callable.parameters(), mapper);

                        code = quote! {
                            #code
                            #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #ctls_name: #qubit_array_type, #parameters) -> #return_type #body
                        }
                    }
                    SpecializationKind::ControlledAdjoint => {
                        // get parameter name for controls from args (normalization guarantees that it exists)
                        let ctls_name = match args {
                            Some(vec) => match &vec[0] {
                                SpecializationParameter::Identifier(name) => {
                                    format_ident!("{}", name)
                                }
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };

                        let name = format_ident!("{}_ctl_adj", callable.name());
                        let body = mapper.visit_scope(scope);
                        let qubit_array_type =
                            mapper.visit_type_kind(TypeKind::array(TypeKind::qubit()).as_ref());
                        let parameters =
                            generate_parameters_directly(callable.parameters(), mapper);

                        code = quote! {
                            #code
                            #modifier fn #name<Sim: QSharpIntrinsics>(sim__: &mut Sim, #ctls_name: #qubit_array_type, #parameters) -> #return_type #body
                        }
                    }
                }
            } else {
                // all specializations should be resolved at this point
                unreachable!();
            }
        }
        code
    } else {
        // TODO this should be unreachable after semantic analysis is complete
        let name = format_ident!("{}", callable.name());
        let body = mapper.visit_callable_body(callable.body());

        quote! {
            #modifier fn #name(#parameters) -> #return_type #body
        }
    };

    // pop parameter from scope
    mapper.pop_scope();

    code
}

fn generate_parameters_directly(
    parameter: &Rc<Parameter>,
    mapper: &mut impl Mapper<Output = proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    let (names, kind) = unzip_parameters(parameter);

    let names = tokenize_tree(&names, mapper);
    let kind = tokenize_tree(&kind, mapper);

    quote! { #names: #kind }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Tree<T> {
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

pub(crate) trait ToTokensOne {
    fn to_tokens_one(
        &self,
        mapper: &mut impl Mapper<Output = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream;
}

impl ToTokensOne for String {
    fn to_tokens_one(
        &self,
        _mapper: &mut impl Mapper<Output = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        let mut stream = proc_macro2::TokenStream::new();
        let ident = format_ident!("{}", self);
        ident.to_tokens(&mut stream);
        stream
    }
}

impl ToTokensOne for Rc<TypeKind> {
    fn to_tokens_one(
        &self,
        mapper: &mut impl Mapper<Output = proc_macro2::TokenStream>,
    ) -> proc_macro2::TokenStream {
        mapper.visit_type_kind(self)
    }
}

pub(crate) fn tokenize_tree<T: ToTokensOne>(
    tree: &Tree<T>,
    mapper: &mut impl Mapper<Output = proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream {
    match tree {
        Tree::Leaf(item) => item.to_tokens_one(mapper),
        Tree::Branch(items) => {
            let mut stream = TokenStream::new();

            for (idx, item) in items.iter().enumerate() {
                if idx > 0 {
                    stream.append(Punct::new(',', proc_macro2::Spacing::Alone));
                }
                stream.extend(tokenize_tree(item, mapper));
            }

            let mut output = TokenStream::new();
            output.append(Group::new(proc_macro2::Delimiter::Parenthesis, stream));

            output
        }
    }
}

pub(crate) fn unzip_parameters(param: &Rc<Parameter>) -> (Tree<String>, Tree<Rc<TypeKind>>) {
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
    use crate::codegen::Codegen;

    use super::*;

    use qsharp_ast::ast::{Parameter, TypeKind};

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
        let mut mapper = Codegen::new();

        let leaf = Tree::<String>::leaf("a");
        let code = tokenize_tree(&leaf, &mut mapper);

        assert_eq!("a", code.to_string());

        let leaf = Tree::<Rc<TypeKind>>::leaf(TypeKind::bool());
        let code = tokenize_tree(&leaf, &mut mapper);

        assert_eq!("bool", code.to_string());
    }

    #[test]
    fn non_nested_tuple_code_gen() {
        let mut mapper = Codegen::new();

        let tuple = Tree::<String>::branch(vec![Tree::leaf("a"), Tree::leaf("b")]);
        let code = tokenize_tree(&tuple, &mut mapper);

        assert_eq!("(a , b)", code.to_string());

        let tuple = Tree::<Rc<TypeKind>>::branch(vec![
            Tree::leaf(TypeKind::int()),
            Tree::leaf(TypeKind::bool()),
        ]);
        let code = tokenize_tree(&tuple, &mut mapper);

        assert_eq!("(i64 , bool)", code.to_string());
    }

    #[test]
    fn nested_tuple_code_gen() {
        let mut mapper = Codegen::new();

        let tuple = Tree::<String>::branch(vec![
            Tree::branch(vec![Tree::leaf("a"), Tree::leaf("b")]),
            Tree::branch(vec![Tree::leaf("c"), Tree::leaf("d")]),
        ]);

        let code = tokenize_tree(&tuple, &mut mapper);

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
        let code = tokenize_tree(&tuple, &mut mapper);

        assert_eq!("((i64 , bool) , (usize , bool))", code.to_string());
    }
}
