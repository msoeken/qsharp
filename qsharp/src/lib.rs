use proc_macro::TokenStream;
use qsharp_ast::ast::Program;
use qsharp_codegen::qsharp_to_rust;
use syn::parse_macro_input;

#[proc_macro]
pub fn qsharp(item: TokenStream) -> TokenStream {
    let mut program = parse_macro_input!(item as Program);
    program.normalize();

    qsharp_to_rust(&program).into()
}
