use std::rc::Rc;

use qsharp::qsharp;
use qsharp_runtime::{Array, QSharpIntrinsics};

struct TestSimulator {}

impl QSharpIntrinsics for TestSimulator {
    fn x(&mut self, _q: usize) {}
    fn y(&mut self, _q: usize) {}
    fn z(&mut self, _q: usize) {}
    fn h(&mut self, _q: usize) {}
    fn s(&mut self, _q: usize) {}
    fn t(&mut self, _q: usize) {}
    fn t_adj(&mut self, _q: usize) {}
    fn cnot(&mut self, _ctl: usize, _tgt: usize) {}
    fn m(&mut self, _q: usize) -> bool {
        true
    }

    fn mcx(&mut self, _ctls: &[usize], _q: usize) {}

    fn allocate(&mut self) -> usize {
        todo!()
    }

    fn allocate_many(&mut self, _count: usize) -> Array<usize> {
        todo!()
    }

    fn release(&mut self, _q: usize) {
        todo!()
    }

    fn release_many(&mut self, _qs: Array<usize>) {
        todo!()
    }
}

#[test]
fn test_array() {
    qsharp! {
        namespace Array {
            function Function(index : Int) : Double {
                let numbers = [1.23, 4.56, 7.89];
                return numbers[index];
            }

            function Sum(numbers: Int[]) : Int {
                mutable sum = 0;
                for number in numbers {
                    set sum += number;
                }
                return sum;
            }

            function RangeSum(to: Int) : Int {
                mutable sum = 0;
                for number in 1..to {
                    set sum += number;
                }
                return sum;
            }
        }
    }

    let mut sim = TestSimulator {};
    assert!((Array::Function(&mut sim, 0) - 1.23).abs() < f64::EPSILON);
    assert!((Array::Function(&mut sim, 1) - 4.56).abs() < f64::EPSILON);
    assert!((Array::Function(&mut sim, 2) - 7.89).abs() < f64::EPSILON);
    assert_eq!(Array::Sum(&mut sim, Rc::new(vec![1, 2, 3, 4])), 10);
    assert_eq!(Array::RangeSum(&mut sim, 4), 10);
}
