use std::{collections::HashMap, rc::Rc};

use qsharp_ast::ast::TypeKind;

pub trait ToRust {
    fn translate(
        &self,
        symbol_table: &mut HashMap<String, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream;

    fn to_rust(&self) -> proc_macro2::TokenStream {
        let mut map = HashMap::new();
        self.translate(&mut map)
    }
}
