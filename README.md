# Q# integration in Rust

This is a personal project to experiment with an integration of Q# in Rust. For
stable Q# compilers, see the following:

* [Microsoft Quantum Development Kit: Q# Compiler and Language Server](https://github.com/microsoft/qsharp-compiler)
* [Microsoft Quantum Development Kit: IQ# Kernel](https://github.com/microsoft/iqsharp)

## Contents

* [./qsharp](qsharp) • Procedural macros to insert Q# code into Rust code
* [./qsharp-ast](qsharp-ast) • Q# parser from Rust token stream
* [./qsharp-codegen](qsharp-codegen) • Rust code generation from Q# AST (*very preliminary*)
* [./qsharp-runtime](qsharp-runtime) • Runtime intrinsics used by Rust code generation (*very preliminary*)

## Build and test

To build the program run `cargo build` from the repository's root folder and run
`cargo test` to run all the tests.
