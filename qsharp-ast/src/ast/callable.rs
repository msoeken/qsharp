use std::rc::Rc;

use syn::{
    braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Result, Token,
};

use super::{declaration_prefix::DeclarationPrefix, kw, utilities::peek_and_consume};
use crate::ast::{
    characteristics::parse_characteristics, parameter::parse_parameter,
    scope::parse_scope_statements, type_kind::parse_type, Characteristics, Ident, Parameter, Scope,
    Specialization, SpecializationGenerator, SpecializationKind, TypeKind, TypeParameter,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CallableKind {
    Operation,
    Function,
}

impl Parse for CallableKind {
    fn parse(input: ParseStream) -> Result<Self> {
        if peek_and_consume(input, kw::operation)? {
            Ok(CallableKind::Operation)
        } else if peek_and_consume(input, kw::function)? {
            Ok(CallableKind::Function)
        } else {
            Err(input.error("unknown callable kind"))
        }
    }
}

/// Callable of a body (either simple scope, or list of [specializations](Specialization))
#[derive(Debug, PartialEq)]
pub enum CallableBody {
    Simple(Scope),
    Multiple(Vec<Specialization>),
}

impl CallableBody {
    /// Returns all specializations, if body is multiple
    pub fn specializations(&self) -> Option<&[Specialization]> {
        match self {
            CallableBody::Multiple(specializations) => Some(specializations),
            _ => None,
        }
    }

    /// Returns all specializations, if body is multiple (mutable)
    pub fn specializations_mut(&mut self) -> Option<&mut [Specialization]> {
        match self {
            CallableBody::Multiple(specializations) => Some(specializations),
            _ => None,
        }
    }

    fn specialization_of_kind(&self, kind: SpecializationKind) -> Option<&Specialization> {
        match self {
            CallableBody::Simple(_) => None,
            CallableBody::Multiple(specializations) => {
                specializations.iter().find(|s| s.kind() == kind)
            }
        }
    }

    fn specialization_of_kind_mut(
        &mut self,
        kind: SpecializationKind,
    ) -> Option<&mut Specialization> {
        match self {
            CallableBody::Simple(_) => None,
            CallableBody::Multiple(specializations) => {
                specializations.iter_mut().find(|s| s.kind() == kind)
            }
        }
    }

    fn scope_of_kind(&self, kind: SpecializationKind) -> Option<&Scope> {
        match self {
            CallableBody::Simple(scope) if kind == SpecializationKind::Body => Some(scope),
            CallableBody::Simple(_) => None,
            CallableBody::Multiple(specializations) => {
                let specialization = specializations.iter().find(|&s| s.kind() == kind)?;
                if let SpecializationGenerator::Provided(_, scope) = specialization.generator() {
                    Some(scope)
                } else {
                    None
                }
            }
        }
    }

    fn scope_of_kind_mut(&mut self, kind: SpecializationKind) -> Option<&mut Scope> {
        match self {
            CallableBody::Simple(scope) if kind == SpecializationKind::Body => Some(scope),
            CallableBody::Simple(_) => None,
            CallableBody::Multiple(specializations) => {
                let specialization = specializations.iter_mut().find(|s| s.kind() == kind)?;
                if let SpecializationGenerator::Provided(_, scope) = specialization.generator_mut()
                {
                    Some(scope)
                } else {
                    None
                }
            }
        }
    }

    /// Returns true, if `CallableBody` is `Simple`
    pub fn is_simple(&self) -> bool {
        match self {
            CallableBody::Simple(_) => true,
            CallableBody::Multiple(_) => false,
        }
    }

    /// Returns true, if `CallableBody` is `Multiple`
    pub fn is_multiple(&self) -> bool {
        !self.is_simple()
    }

    /// Returns the specialization for body (returns None if `CallableBody` is
    /// `Simple`)
    pub fn body(&self) -> Option<&Specialization> {
        self.specialization_of_kind(SpecializationKind::Body)
    }

    /// Returns the specialization for adjoint
    pub fn adjoint(&self) -> Option<&Specialization> {
        self.specialization_of_kind(SpecializationKind::Adjoint)
    }

    /// Returns the specialization for controlled
    pub fn controlled(&self) -> Option<&Specialization> {
        self.specialization_of_kind(SpecializationKind::Controlled)
    }

    /// Returns the specialization for controlled adjoint
    pub fn controlled_adjoint(&self) -> Option<&Specialization> {
        self.specialization_of_kind(SpecializationKind::ControlledAdjoint)
    }

    /// Returns true if body specialization is contained (returns false if
    /// `CallableBody` is `Simple`)
    pub fn has_body(&self) -> bool {
        self.body().is_some()
    }

    /// Returns true if adjoint specialization is contained
    pub fn has_adjoint(&self) -> bool {
        self.adjoint().is_some()
    }

    /// Returns true if controlled specialization is contained
    pub fn has_controlled(&self) -> bool {
        self.controlled().is_some()
    }

    /// Returns true if controlled adjoint specialization is contained
    pub fn has_controlled_adjoint(&self) -> bool {
        self.controlled_adjoint().is_some()
    }

    /// Returns the specialization for body (mutable reference, returns None if
    /// `CallableBody` is`Simple`)
    pub fn body_mut(&mut self) -> Option<&mut Specialization> {
        self.specialization_of_kind_mut(SpecializationKind::Body)
    }

    /// Returns the specialization for adjoint (mutable reference)
    pub fn adjoint_mut(&mut self) -> Option<&mut Specialization> {
        self.specialization_of_kind_mut(SpecializationKind::Adjoint)
    }

    /// Returns the specialization for controlled (mutable reference)
    pub fn controlled_mut(&mut self) -> Option<&mut Specialization> {
        self.specialization_of_kind_mut(SpecializationKind::Controlled)
    }

    /// Returns the specialization for adjoint controlled (mutable reference)
    pub fn controlled_adjoint_mut(&mut self) -> Option<&mut Specialization> {
        self.specialization_of_kind_mut(SpecializationKind::ControlledAdjoint)
    }

    /// Returns the scope for the body implementation
    pub fn body_scope(&self) -> Option<&Scope> {
        self.scope_of_kind(SpecializationKind::Body)
    }

    /// Returns the scope for the adjoint implementation
    pub fn adjoint_scope(&self) -> Option<&Scope> {
        self.scope_of_kind(SpecializationKind::Adjoint)
    }

    /// Returns the scope for the controlled implementation
    pub fn controlled_scope(&self) -> Option<&Scope> {
        self.scope_of_kind(SpecializationKind::Controlled)
    }

    /// Returns the scope for the controlled adjoint implementation
    pub fn controlled_adjoint_scope(&self) -> Option<&Scope> {
        self.scope_of_kind(SpecializationKind::ControlledAdjoint)
    }

    /// Returns the scope for the body implementation (mutable reference)
    pub fn body_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scope_of_kind_mut(SpecializationKind::Body)
    }

    /// Returns the scope for the adjoint implementation (mutable reference)
    pub fn adjoint_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scope_of_kind_mut(SpecializationKind::Adjoint)
    }

    /// Returns the scope for the controlled implementation (mutable reference)
    pub fn controlled_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scope_of_kind_mut(SpecializationKind::Controlled)
    }

    /// Returns the scope for the controlled adjoint implementation (mutable reference)
    pub fn controlled_adjoint_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scope_of_kind_mut(SpecializationKind::ControlledAdjoint)
    }

    /// Add specialization (user must check whether specialization already
    /// exists, switches from `Simple` to `Multiple`, when `Simple`)
    pub fn add_specialization(&mut self, specialization: Specialization) {
        match self {
            Self::Simple(_) => *self = Self::Multiple(vec![specialization]),
            Self::Multiple(specializations) => specializations.push(specialization),
        }
    }
}

/// Q# callable (e.g., `operation Op ...` or `function Func ...`)
#[derive(Debug, PartialEq)]
pub struct Callable {
    pub(crate) prefix: DeclarationPrefix,
    pub(crate) kind: CallableKind,
    pub(crate) name: Ident,
    pub(crate) type_parameters: Option<Vec<TypeParameter>>,
    pub(crate) parameters: Rc<Parameter>,
    pub(crate) return_type: Rc<TypeKind>,
    pub(crate) characteristics: Option<Rc<Characteristics>>,
    pub(crate) body: CallableBody,
}

impl Callable {
    pub fn prefix(&self) -> &DeclarationPrefix {
        &self.prefix
    }

    pub fn kind(&self) -> CallableKind {
        self.kind
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn type_parameters(&self) -> &Option<Vec<TypeParameter>> {
        &self.type_parameters
    }

    pub fn parameters(&self) -> &Rc<Parameter> {
        &self.parameters
    }

    pub fn return_type(&self) -> &Rc<TypeKind> {
        &self.return_type
    }

    pub fn characteristics(&self) -> &Option<Rc<Characteristics>> {
        &self.characteristics
    }

    pub fn body(&self) -> &CallableBody {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut CallableBody {
        &mut self.body
    }

    pub fn set_body(&mut self, body: CallableBody) {
        self.body = body;
    }
}

impl Parse for CallableBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let buffer;
        braced!(buffer in input);

        if buffer.peek(kw::body) || buffer.peek(kw::adjoint) || buffer.peek(kw::controlled) {
            let mut specializations = vec![];
            while !buffer.is_empty() {
                specializations.push(buffer.parse()?);
            }

            Ok(CallableBody::Multiple(specializations))
        } else {
            Ok(CallableBody::Simple(parse_scope_statements(&buffer)?))
        }
    }
}

/// Parses the callable without prefix, but provides the prefix as an extra argument.
pub(crate) fn parse_callable_without_prefix(
    input: ParseStream,
    prefix: DeclarationPrefix,
) -> Result<Callable> {
    let kind = input.parse()?;

    let ident: Ident = input.parse()?;

    // type parameters
    let type_parameters = if peek_and_consume(input, Token![<])? {
        let params: Punctuated<TypeParameter, Token![,]> =
            Punctuated::parse_separated_nonempty(input)?;
        input.parse::<Token![>]>()?;
        Some(params.into_iter().collect())
    } else {
        None
    };

    let parameters = parse_parameter(input)?;
    input.parse::<Token![:]>()?;

    let return_type = parse_type(input)?;

    // characteristics?
    let characteristics = if input.peek(syn::Ident) {
        input.parse::<kw::is>()?;
        let characteristics = parse_characteristics(input)?;
        Some(characteristics)
    } else {
        None
    };

    let body = input.parse()?;

    Ok(Callable {
        prefix,
        kind,
        name: ident,
        type_parameters,
        parameters,
        return_type,
        characteristics,
        body,
    })
}

impl Parse for Callable {
    fn parse(input: ParseStream) -> Result<Self> {
        let prefix = input.parse()?;
        parse_callable_without_prefix(input, prefix)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use syn::Result;

    #[test]
    fn test_operation_unit_unit() -> Result<()> {
        let operation: Callable = syn::parse_str("operation Op() : Unit {}")?;

        assert_eq!(operation.kind, CallableKind::Operation);
        assert_eq!(operation.name.as_str(), "Op");
        assert_eq!(operation.parameters, Parameter::tuple(vec![]));
        assert_eq!(operation.return_type, TypeKind::unit());
        match &operation.body {
            CallableBody::Simple(scope) => assert!(scope.statements().is_empty()),
            _ => panic!(),
        }

        Ok(())
    }

    #[test]
    fn test_function_unit_unit() -> Result<()> {
        let operation: Callable = syn::parse_str("function Op() : Unit {}")?;

        assert_eq!(operation.kind, CallableKind::Function);
        assert_eq!(operation.name.as_str(), "Op");
        assert_eq!(operation.parameters, Parameter::tuple(vec![]));
        assert_eq!(operation.return_type, TypeKind::unit());
        match &operation.body {
            CallableBody::Simple(scope) => assert!(scope.statements().is_empty()),
            _ => panic!(),
        }

        Ok(())
    }

    #[test]
    fn test_operation_with_specialization() -> Result<()> {
        let _operation: Callable = syn::parse_str(
            "operation Op() : Unit { body intrinsic; adjoint controlled distribute; }",
        )?;

        Ok(())
    }

    #[test]
    fn test_operation_with_attributes_and_access_modifier() -> Result<()> {
        let _operation: Callable = syn::parse_str(
            "@Attribute() internal operation Op() : Unit { body intrinsic; adjoint controlled distribute; }",
        )?;

        Ok(())
    }

    #[test]
    fn test_basic_lift() -> Result<()> {
        let prog = "operation Foo() : Unit {\n\
                             let r = Zero;\n\
                             \n\
                             if r == Zero {\n\
                                 SubOp1();\n\
                                 SubOp2();\n\
                                 SubOp3();\n\
                                 let tmp = 4;\n\
                                 use q = Qubit() {\n\
                                     let temp2 = q;\n\
                                 }\n\
                             }\n\
                         }\n";
        let _: Callable = syn::parse_str(prog)?;

        Ok(())
    }

    #[test]
    fn test_rust_identifier() -> Result<()> {
        let op: Callable = syn::parse_str("operation Self() : Unit {}")?;
        assert_eq!(op.name.as_str(), "Self");
        Ok(())
    }
}
