mod codegen;

use qsharp_ast::{ast::Program, utilities::Mapper};

pub fn qsharp_to_rust(program: &Program) -> proc_macro2::TokenStream {
    let mut codegen = codegen::Codegen::new();
    codegen.visit_program(program)
}
