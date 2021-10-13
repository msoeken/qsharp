use std::rc::Rc;

use fixedbitset::FixedBitSet;
use qsharp::qsharp;
use qsharp_runtime::QSharpIntrinsics;

#[test]
fn test_arithmetic() {
    qsharp! {
    namespace Arithmetic {
        open Microsoft.Quantum.Arrays;
        open Microsoft.Quantum.Diagnostics;
        open Microsoft.Quantum.Intrinsic;

        operation ApplyAnd(ctl1: Qubit, ctl2: Qubit, tgt: Qubit) : Unit is Adj {
            body (...) {
                Controlled X([ctl1, ctl2], tgt);
            }

            adjoint self;
        }

        operation Add(x : Qubit[], y : Qubit[]) : Unit is Adj+Ctl {
            let n = Length(x);
            Fact(n > 0, "Bitwidth must be at least 1");

            if Length(y) == n + 1 {
                AddWithCarryOut(false, x, y);
            } else {
                AddWithoutCarryOut(false, false, x, y);
            }
        }

        operation AddWithCarryIn(x : Qubit[], y : Qubit[], carryIn : Qubit) : Unit is Adj+Ctl {
            let n = Length(x);
            Fact(n > 0, "Bitwidth must be at least 1");

            if Length(y) == n + 1 {
                AddWithCarryInAndOut(x, y, carryIn);
            } else {
                EqualityFactI(Length(y), n, "Bitwidth of x and y must equal");

                if n == 1 {
                    Sum(carryIn, Head(x), Head(y));
                } else {
                    use carry = Qubit();
                    Carry(carryIn, Head(x), Head(y), carry);
                    AddWithCarryIn(Rest(x), Rest(y), carry);
                    Uncarry(carryIn, Head(x), Head(y), carry);
                }
            }
        }

        internal operation AddWithCarryInAndOut(x : Qubit[], y : Qubit[], carryIn : Qubit) : Unit is Adj+Ctl {
            body (...) {
                let n = Length(x);
                Fact(n > 0, "Bitwidth must be at least 1");
                EqualityFactI(Length(y), n + 1, "Bitwidth of y must be 1 more than x");

                let carryOut = Tail(y);
                AssertAllZero([carryOut]);

                if n == 1 {
                    Carry(carryIn, Head(x), Head(y), carryOut);
                    CNOT(carryIn, Head(x));
                    CNOT(Head(x), Head(y));
                } else {
                    use carry = Qubit();
                    Carry(carryIn, Head(x), Head(y), carry);
                    AddWithCarryInAndOut(Rest(x), Rest(y), carry);
                    Uncarry(carryIn, Head(x), Head(y), carry);
                }
            }

            adjoint auto;

            controlled (ctls, ...) {
                let n = Length(x);
                Fact(n > 0, "Bitwidth must be at least 1");
                EqualityFactI(Length(y), n + 1, "Bitwidth of y must be 1 more than x");
                EqualityFactI(Length(ctls), 1, "Number of control lines must be 1");

                let carryOut = Tail(y);
                AssertAllZero([carryOut]);

                if n == 1 {
                    use carry = Qubit();
                    Carry(carryIn, Head(x), Head(y), carry);
                    ApplyAnd(ctls[0], carry, carryOut);
                    Controlled Uncarry(ctls, (carryIn, Head(x), Head(y), carry));
                } else {
                    use carry = Qubit();
                    Carry(carryIn, Head(x), Head(y), carry);
                    Controlled AddWithCarryInAndOut(ctls, (Rest(x), Rest(y), carry));
                    Controlled Uncarry(ctls, (carryIn, Head(x), Head(y), carry));
                }
            }
        }

        internal operation AddWithoutCarryOut(useInSubtract : Bool, xIsOddConstant : Bool, x : Qubit[], y : Qubit[]) : Unit is Adj+Ctl {
            let n = Length(x);
            Fact(n > 0, "Bitwidth must be at least 1");
            EqualityFactI(Length(y), n, "Bitwidth of x and y must equal, or y is one larger than x");

            if n == 1 {
                HalfSum(useInSubtract, xIsOddConstant, Head(x), Head(y));
            } else {
                use carry = Qubit();
                HalfCarry(useInSubtract, xIsOddConstant, Head(x), Head(y), carry);
                AddWithCarryIn(Rest(x), Rest(y), carry);
                HalfUncarry(useInSubtract, xIsOddConstant, Head(x), Head(y), carry);
            }
        }

        internal operation AddWithCarryOut(useInSubtract : Bool, x : Qubit[], y : Qubit[]) : Unit is Adj+Ctl {
            body (...) {
                let n = Length(x);
                Fact(n > 0, "Bitwidth must be at least 1");
                EqualityFactI(Length(y), n + 1, "Bitwidth of y must be 1 more than x");

                let carryOut = Tail(y);
                AssertAllZero([carryOut]);

                if n == 1 {
                    HalfCarry(useInSubtract, false, Head(x), Head(y), carryOut);
                    if useInSubtract {
                        X(Head(x));
                    }
                    CNOT(Head(x), Head(y));
                } else {
                    use carry = Qubit();
                    HalfCarry(useInSubtract, false, Head(x), Head(y), carry);
                    AddWithCarryInAndOut(Rest(x), Rest(y), carry);
                    HalfUncarry(useInSubtract, false, Head(x), Head(y), carry);
                }
            }

            adjoint auto;

            controlled (ctls, ...) {
                let n = Length(x);
                Fact(n > 0, "Bitwidth must be at least 1");
                EqualityFactI(Length(y), n + 1, "Bitwidth of y must be 1 more than x");
                EqualityFactI(Length(ctls), 1, "Number of control lines must be 1");

                let carryOut = Tail(y);
                AssertAllZero([carryOut]);

                if n == 1 {
                    use carry = Qubit();
                    HalfCarry(useInSubtract, false, Head(x), Head(y), carry);
                    ApplyAnd(ctls[0], carry, carryOut);
                    Controlled HalfUncarry(ctls, (useInSubtract, false, Head(x), Head(y), carry));
                } else {
                    use carry = Qubit();
                    HalfCarry(useInSubtract, false, Head(x), Head(y), carry);
                    Controlled AddWithCarryInAndOut(ctls, (Rest(x), Rest(y), carry));
                    Controlled HalfUncarry(ctls, (useInSubtract, false, Head(x), Head(y), carry));
                }
            }
        }

        internal operation Carry(carryIn : Qubit, x : Qubit, y : Qubit, carryOut : Qubit) : Unit is Adj+Ctl {
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

        internal operation HalfCarry(carryIn : Bool, xIsOddConstant : Bool, x : Qubit, y : Qubit, carryOut : Qubit) : Unit is Adj+Ctl {
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

        internal operation Uncarry(carryIn : Qubit, x : Qubit, y : Qubit, carryOut : Qubit) : Unit is Adj+Ctl {
            body (...) {
                CNOT(carryIn, carryOut);
                Adjoint ApplyAnd(x, y, carryOut);
                CNOT(carryIn, x);
                CNOT(x, y);

                AssertAllZero([carryOut]);
            }

            adjoint auto;

            controlled (ctls, ...) {
                EqualityFactI(Length(ctls), 1, "Number of control lines must be 1");

                CNOT(carryIn, carryOut);
                Adjoint ApplyAnd(x, y, carryOut);
                AssertAllZero([carryOut]);

                within {
                    ApplyAnd(ctls[0], x, carryOut);
                } apply {
                    CNOT(carryOut, y);
                }
                AssertAllZero([carryOut]);
                CNOT(carryIn, x);
                CNOT(carryIn, y);
            }

            controlled adjoint auto;
        }

        internal operation HalfUncarry(carryIn: Bool, xIsOddConstant : Bool, x : Qubit, y : Qubit, carryOut : Qubit) : Unit is Adj {
            body (...) {
                if carryIn {
                    X(carryOut);
                }
                if xIsOddConstant {
                    CNOT(y, carryOut);
                    X(y);
                } else {
                    Adjoint ApplyAnd(x, y, carryOut);
                    CNOT(x, y);
                }
                if carryIn {
                    X(x);
                    X(y);
                }

                AssertAllZero([carryOut]);
            }

            adjoint auto;

            controlled (ctls, ...) {
                EqualityFactI(Length(ctls), 1, "Number of control lines must be 1");

                if carryIn {
                    X(carryOut);
                }
                if xIsOddConstant {
                    CNOT(y, carryOut);
                    CNOT(ctls[0], y);
                } else {
                    Adjoint ApplyAnd(x, y, carryOut);
                    AssertAllZero([carryOut]);

                    within {
                        ApplyAnd(ctls[0], x, carryOut);
                    } apply {
                        CNOT(carryOut, y);
                    }
                }
                if carryIn {
                    X(x);
                    X(y);
                }

                AssertAllZero([carryOut]);
            }

            controlled adjoint auto;
        }

        internal operation Sum(carryIn : Qubit, x : Qubit, y : Qubit) : Unit is Adj+Ctl {
            body (...) {
                CNOT(carryIn, y);
                CNOT(x, y);
            }

            adjoint self;

            controlled (ctls, ...) {
                EqualityFactI(Length(ctls), 1, "Number of control lines must be 1");

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

        internal operation HalfSum(carryIn : Bool, xIsOddConstant : Bool, x : Qubit, y : Qubit) : Unit is Adj+Ctl {
            body (...) {
                if carryIn {
                    X(y);
                }
                if xIsOddConstant {
                    X(y);
                } else {
                    CNOT(x, y);
                }
            }

            adjoint self;

            controlled (ctls, ...) {
                EqualityFactI(Length(ctls), 1, "Number of control lines must be 1");

                if xIsOddConstant {
                    if not carryIn {
                        CNOT(ctls[0], y);
                    }
                } else {
                    use q = Qubit();
                    within {
                        if carryIn {
                            X(x);
                        }
                        ApplyAnd(ctls[0], x, q);
                    } apply {
                        CNOT(q, y);
                    }
                }
            }

            controlled adjoint self;
        }
    }
    }

    let mut sim = ToffoliSimulator::new();
    let bitwidth = 4;

    let a = sim.allocate_many(bitwidth);
    let b = sim.allocate_many(bitwidth);

    fn apply_xor_in_place(sim: &mut impl QSharpIntrinsics, value: usize, register: &[usize]) {
        for (idx, &qubit) in register.iter().enumerate() {
            if ((value >> idx) & 1) == 1 {
                sim.x(qubit);
            }
        }
    }

    fn measure_integer(sim: &mut impl QSharpIntrinsics, register: &[usize]) -> usize {
        let mut sum = 0;

        for (idx, &qubit) in register.iter().enumerate() {
            if sim.m(qubit) {
                sum += 1 << idx;
                sim.x(qubit);
            }
        }

        sum
    }

    for aval in 0..(1 << bitwidth) {
        for bval in 0..(1 << bitwidth) {
            apply_xor_in_place(&mut sim, aval, &a);
            apply_xor_in_place(&mut sim, bval, &b);
            Arithmetic::Add(&mut sim, a.clone(), b.clone());

            assert_eq!(
                measure_integer(&mut sim, &a),
                aval,
                "unexpected value for a (a = {}, b = {})",
                aval,
                bval
            );
            assert_eq!(
                measure_integer(&mut sim, &b),
                (aval + bval) % (1 << bitwidth),
                "unexpected value for a (a = {}, b = {})",
                aval,
                bval
            );
        }
    }
}

#[derive(Default)]
struct ToffoliSimulator {
    state: FixedBitSet,
    num_qubits: usize,
    free_list: Vec<usize>,
}

impl ToffoliSimulator {
    pub fn new() -> Self {
        Self::default()
    }
}

impl QSharpIntrinsics for ToffoliSimulator {
    fn x(&mut self, q: usize) {
        self.state.toggle(q);
    }

    fn y(&mut self, _q: usize) {
        panic!("cannot simulate non-classical quantum instruction");
    }

    fn z(&mut self, _q: usize) {
        panic!("cannot simulate non-classical quantum instruction");
    }

    fn h(&mut self, _q: usize) {
        panic!("cannot simulate non-classical quantum instruction");
    }

    fn s(&mut self, _q: usize) {
        panic!("cannot simulate non-classical quantum instruction");
    }

    fn t(&mut self, _q: usize) {
        panic!("cannot simulate non-classical quantum instruction");
    }

    fn t_adj(&mut self, _q: usize) {
        panic!("cannot simulate non-classical quantum instruction");
    }

    fn cnot(&mut self, ctl: usize, tgt: usize) {
        if self.state[ctl] {
            self.state.toggle(tgt);
        }
    }

    fn m(&mut self, q: usize) -> bool {
        self.state[q]
    }

    fn mcx(&mut self, ctls: &[usize], q: usize) {
        if ctls.iter().all(|&ctl| self.state[ctl]) {
            self.state.toggle(q);
        }
    }

    fn allocate(&mut self) -> usize {
        if let Some(free) = self.free_list.pop() {
            free
        } else {
            let index = self.num_qubits;
            self.num_qubits += 1;
            self.state.grow(self.num_qubits);
            index
        }
    }

    fn allocate_many(&mut self, count: usize) -> qsharp_runtime::Array<usize> {
        Rc::new(
            std::iter::repeat(())
                .take(count)
                .map(|_| self.allocate())
                .collect(),
        )
    }

    fn release(&mut self, q: usize) {
        self.free_list.push(q);
    }

    fn release_many(&mut self, qs: qsharp_runtime::Array<usize>) {
        for &q in qs.iter() {
            self.release(q);
        }
    }
}
