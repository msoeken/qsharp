use proc_macro::TokenStream;
use proc_macro2::Ident;
use qsharp_ast::{ast::Program, utilities::Mapper};
use qsharp_codegen::Codegen;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Result, Token,
};

struct ProgramWithOptions {
    program: Program,
    sim_trait: Option<String>,
}

impl Parse for ProgramWithOptions {
    fn parse(input: ParseStream) -> Result<Self> {
        let sim_trait = if input.peek(Token![impl]) {
            input.parse::<Token![impl]>()?;
            let ident: Ident = input.parse()?;
            Some(ident.to_string())
        } else {
            None
        };

        let program = input.parse()?;

        Ok(Self { program, sim_trait })
    }
}

#[proc_macro]
pub fn qsharp(item: TokenStream) -> TokenStream {
    let mut result = parse_macro_input!(item as ProgramWithOptions);
    result.program.normalize();

    let mut codegen = Codegen::new();
    if let Some(sim_trait) = result.sim_trait {
        codegen.set_sim_trait(&sim_trait);
    }

    codegen.visit_program(&result.program).into()
}
