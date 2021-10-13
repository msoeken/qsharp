use qsharp_ast::ast::Program;

use qsharp_codegen::ToRust;
use syn::Result;

#[test]
fn test_and() -> Result<()> {
    let code = "
    namespace Common {
        open Microsoft.Quantum.Intrinsic;

        operation And(ctl1: Qubit, ctl2: Qubit, tgt: Qubit) : Unit is Adj {
            body (...) {
                H(tgt);
                T(tgt);
                CNOT(ctl1, tgt);
                CNOT(ctl2, tgt);
                CNOT(tgt, ctl1);
                CNOT(tgt, ctl2);
                Adjoint T(ctl1);
                Adjoint T(ctl2);
                T(tgt);
                CNOT(tgt, ctl2);
                CNOT(tgt, ctl1);
                H(tgt);
                S(tgt);
            }

            adjoint (...) {
                H(tgt);
                if M(tgt) == One {
                    H(ctl2);
                    CNOT(ctl1, ctl2);
                    H(ctl2);
                }
            }
        }
    }
    ";

    let mut ast: Program = syn::parse_str(code)?;
    ast.normalize();

    let _rust = ast.to_rust();

    println!("{}", _rust);

    Ok(())
}

#[test]
fn test_swap() -> Result<()> {
    let code = "
    namespace Common {
        open Microsoft.Quantum.Intrinsic;

        operation SWAP(q1: Qubit, q2: Qubit) : Unit is Adj+Ctl {
            body (...) {
                within {
                    CNOT(q2, q1);
                } apply {
                    CNOT(q1, q2);
                }
            }

            adjoint self;
        }
    }
    ";

    let mut ast: Program = syn::parse_str(code)?;
    ast.normalize();

    let _rust = ast.to_rust();

    println!("{}", _rust);

    Ok(())
}

#[test]
fn test_sum() -> Result<()> {
    let code = "
    namespace Common {
        open Microsoft.Quantum.Arrays;
        open Microsoft.Quantum.Diagnostics;
        open Microsoft.Quantum.Intrinsic;
        open Circuits;

        internal operation Sum(carryIn : Qubit, x : Qubit, y : Qubit) : Unit is Adj+Ctl {
            body (...) {
                CNOT(carryIn, y);
                CNOT(x, y);
            }

            adjoint self;

            controlled (ctls, ...) {
                EqualityFactI(Length(ctls), 1, \"Number of control lines must be 1\");

                use q = Qubit();
                within {
                    CNOT(carryIn, x);
                    ApplyAnd(ctls[0], x, q);
                } apply {
                    CNOT(q, y);
                }
            }

            controlled adjoint self;
        }
    }
    ";

    let mut ast: Program = syn::parse_str(code)?;
    ast.normalize();

    let _rust = ast.to_rust();

    println!("{}", _rust);

    Ok(())
}
