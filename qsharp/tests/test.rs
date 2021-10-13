use std::rc::Rc;

use qsharp::qsharp;

use qsharp_runtime::*;

qsharp! {

namespace Circuits {
    open Microsoft.Quantum.Intrinsic;

    operation ApplyAnd(ctl1: Qubit, ctl2: Qubit, tgt: Qubit) : Unit is Adj {
        body (...) {
            H(tgt);
            T(tgt);
            CNOT(ctl1, tgt);
            CNOT(ctl2, tgt);
            within {
                CNOT(tgt, ctl1);
                CNOT(tgt, ctl2);
            } apply {
                Adjoint T(ctl1);
                Adjoint T(ctl2);
                T(tgt);
            }
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

namespace Arithmetic {
    open Microsoft.Quantum.Arrays;
    open Microsoft.Quantum.Diagnostics;
    open Microsoft.Quantum.Intrinsic;
    open Circuits;

    operation Carry(carryIn : Qubit, x : Qubit, y : Qubit, carryOut : Qubit) : Unit is Adj+Ctl {
        body (...) {
            Controlled Carry(EmptyArray<Qubit>(), (carryIn, x, y, carryOut));
        }

        adjoint auto;

        controlled (ctls, ...) {
            Fact(Length(ctls) <= 1, "Number of control lines must be at most 1");
            AssertAllZero([carryOut]);

            CNOT(carryIn, x);
            CNOT(carryIn, y);
            ApplyAnd(x, y, carryOut);
            CNOT(carryIn, carryOut);
        }

        controlled adjoint auto;
    }

    operation HalfCarry(carryIn : Bool, xIsOddConstant : Bool, x : Qubit, y : Qubit, carryOut : Qubit) : Unit is Adj+Ctl {
        body (...) {
            Controlled HalfCarry(EmptyArray<Qubit>(), (carryIn, xIsOddConstant, x, y, carryOut));
        }

        adjoint auto;

        controlled (ctls, ...) {
            Fact(Length(ctls) <= 1, "Number of control lines must be at most 1");
            AssertAllZero([carryOut]);

            if carryIn {
                X(x);
                X(y);
            }
            if xIsOddConstant {
                CNOT(y, carryOut);
            } else {
                ApplyAnd(x, y, carryOut);
            }
            if carryIn {
                X(carryOut);
            }
        }

        controlled adjoint auto;
    }
}

}

//mod Partial {
//    use crate::*;
//    use Microsoft::Quantum::Intrinsic::*;
//
//    fn ApplyToEach<Sim: QSharpIntrinsics, F, T: Clone>(sim: &mut Sim, op: F, values: Array<T>)
//    where
//        F: Fn(&mut Sim, T),
//    {
//        for value in values.as_ref() {
//            op(sim, value.clone());
//        }
//    }
//
//    fn ApplyIfA<'a, Sim: QSharpIntrinsics, F, T: Clone>(
//        sim: &'a mut Sim,
//        op: F,
//        enable: bool,
//        value: T,
//    ) where
//        F: Fn(&'a mut Sim, T),
//    {
//        if enable {
//            op(sim, value.clone())
//        }
//    }
//
//    fn Foo<Sim: QSharpIntrinsics>(sim: &mut Sim) {
//        let qs = std::rc::Rc::new(vec![1, 2]);
//
//        let op = |sim, qs| ApplyToEach(sim, H, qs);
//        ApplyIfA(sim, op, true, qs);
//    }
//}

pub struct PrintSimulator {}

impl QSharpIntrinsics for PrintSimulator {
    fn x(&mut self, q: usize) {
        println!("X {}", q);
    }

    fn y(&mut self, q: usize) {
        println!("Y {}", q);
    }

    fn z(&mut self, q: usize) {
        println!("Z {}", q);
    }

    fn h(&mut self, q: usize) {
        println!("H {}", q);
    }

    fn s(&mut self, q: usize) {
        println!("S {}", q);
    }

    fn t(&mut self, q: usize) {
        println!("T {}", q);
    }

    fn t_adj(&mut self, q: usize) {
        println!("T† {}", q);
    }

    fn cnot(&mut self, ctl: usize, tgt: usize) {
        println!("CNOT {} {}", ctl, tgt);
    }

    fn m(&mut self, q: usize) -> bool {
        println!("M {}", q);
        true
    }

    fn mcx(&mut self, ctls: &[usize], q: usize) {
        println!("MCX {:?} {}", ctls, q);
    }

    fn allocate(&mut self) -> usize {
        println!("allocate");
        0
    }

    fn allocate_many(&mut self, count: usize) -> Array<usize> {
        println!("allocate {}", count);
        Rc::new(vec![])
    }

    fn release(&mut self, _q: usize) {
        println!("release");
    }

    fn release_many(&mut self, qs: Array<usize>) {
        println!("release {}", qs.len());
    }
}

#[test]
fn test_the_answer() {
    let mut sim = PrintSimulator {};

    //Circuits::ApplyAnd(&mut sim, 0, 1, 2);

    Arithmetic::Carry(&mut sim, 0, 1, 2, 3);
}
