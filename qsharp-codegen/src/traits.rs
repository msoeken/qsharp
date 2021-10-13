use std::{collections::HashMap, rc::Rc};

use qsharp_ast::ast::{QualifiedName, TypeKind};

pub trait ToRust {
    fn translate(
        &self,
        symbol_table: &mut HashMap<QualifiedName, Rc<TypeKind>>,
    ) -> proc_macro2::TokenStream;

    fn to_rust(&self) -> proc_macro2::TokenStream {
        let mut map = HashMap::new();
        self.translate(&mut map)
    }
}
