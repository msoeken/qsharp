//! This is a library for parsing, analyzing, and normalizing Q# code.  The Q#
//! code is parsed based on a [`proc_macro2::TokenStream`] using the [`syn`]
//! crate.
//!
//! Some highlights of this library include
//! * Architecture allows to include Q# into Rust code using procedural macros
//! * Hand-written parser with AST-element specific performance optimizations
//! * The parser is benchmarked against all Q# files in Microsoft's
//!   [Quantum](https://github.com/microsoft/Quantum) and
//!   [QuantumLibraries](https://github.com/microsoft/QuantumLibraries)
//!   repositories.
//!
//! # Example
//!
//! This is an example on how to parse a Q# program as an AST and normalize it
//! afterwards.
//!
//! ```
//! use rflat_ast::ast::Program;
//!
//! let mut program: Program = syn::parse_str("
//!   namespace Sample {
//!     open Microsoft.Quantum.Intrinsic;
//!
//!     operation Majority(a : Qubit, b : Qubit, c : Qubit, target: Qubit) : Unit is Adj {
//!       within {
//!         CNOT(b, a);
//!         CNOT(b, c);
//!       } apply {
//!         CCNOT(a, c, target);
//!         CNOT(b, target);
//!       }
//!     }
//!   }
//! ")?;
//! program.normalize();
//! # Ok::<(), syn::parse::Error>(())
//! ```

pub(crate) mod analysis {
    pub mod expression;
    pub mod normalize;
}

/// Contains all AST data structures, which implement the [`syn::parse::Parse`] trait.
pub mod ast {
    mod callable;
    mod characteristics;
    mod declaration_prefix;
    mod expression;
    mod ident;
    mod kw;
    mod namespace;
    mod parameter;
    mod program;
    mod qualified_name;
    mod qubit_initializer;
    mod scope;
    mod specialization;
    mod statement;
    mod symbol_binding;
    mod type_declaration;
    mod type_kind;
    mod type_parameters;
    mod utilities;

    pub use callable::{Callable, CallableBody};
    pub use characteristics::Characteristics;
    pub use declaration_prefix::{Access, DeclarationPrefix};
    pub use expression::{ArrayItemIndex, Expression, Pauli, ResultValue};
    pub use ident::Ident;
    pub use namespace::{Namespace, NamespaceItem};
    pub use parameter::Parameter;
    pub use program::Program;
    pub use qualified_name::QualifiedName;
    pub use qubit_initializer::QubitInitializer;
    pub use scope::Scope;
    pub use specialization::{
        Specialization, SpecializationGenerator, SpecializationKind, SpecializationParameter,
    };
    pub use statement::{QubitAllocationKind, Statement, UpdateOperator};
    pub use symbol_binding::SymbolBinding;
    pub use type_declaration::TypeDeclaration;
    pub use type_kind::{TypeKind, TypeParameter, TypeTupleItem};
    pub use type_parameters::TypeParameters;
}

pub(crate) mod utilities;
