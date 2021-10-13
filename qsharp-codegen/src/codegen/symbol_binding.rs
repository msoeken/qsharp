use qsharp_ast::ast::{QubitInitializer, SymbolBinding};

pub fn flatten_bindings<'a, 'b>(
    binding: &'a SymbolBinding,
    initializer: &'b QubitInitializer,
) -> Vec<(&'a str, &'b QubitInitializer)> {
    match (binding, initializer) {
        (SymbolBinding::Identifier(name), QubitInitializer::Single)
        | (SymbolBinding::Identifier(name), QubitInitializer::Register(_)) => {
            vec![(name.as_str(), initializer)]
        }
        (SymbolBinding::Tuple(_), QubitInitializer::Tuple(_)) => todo!(),
        _ => unreachable!(),
    }
}
