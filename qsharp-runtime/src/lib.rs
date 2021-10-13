#[allow(non_snake_case)]
pub mod Microsoft {
    pub mod Quantum {
        pub mod Arrays;
        pub mod Core;
        pub mod Diagnostics;
        pub mod Intrinsic;
    }
}

mod types;
pub use types::{Array, QSharpIntrinsics};
