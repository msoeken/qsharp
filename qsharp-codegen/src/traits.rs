pub trait ToRust {
    fn to_rust(&self) -> proc_macro2::TokenStream;
}
