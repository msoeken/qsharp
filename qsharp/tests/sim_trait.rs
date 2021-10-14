use qsharp::qsharp;

#[derive(Default)]
struct TestSimulator {
    gates: Vec<u8>,
}

pub trait MyIntrinsics {
    fn x(&mut self);
    fn y(&mut self);
    fn z(&mut self);
}

impl MyIntrinsics for TestSimulator {
    fn x(&mut self) {
        self.gates.push(0);
    }

    fn y(&mut self) {
        self.gates.push(1);
    }

    fn z(&mut self) {
        self.gates.push(2);
    }
}

#[allow(non_snake_case)]
fn X(sim: &mut impl MyIntrinsics) {
    sim.x();
}

#[allow(non_snake_case)]
fn Y(sim: &mut impl MyIntrinsics) {
    sim.y();
}

#[allow(non_snake_case)]
fn Z(sim: &mut impl MyIntrinsics) {
    sim.z();
}

qsharp! {
    impl MyIntrinsics

    namespace Operations {
        operation Operation() : Unit {
            X();
            Y();
            Z();
            Y();
            X();
        }
    }
}

#[test]
fn test_sim_trait() {
    let mut sim = TestSimulator::default();

    assert!(sim.gates.is_empty());

    Operations::Operation(&mut sim);

    assert_eq!(sim.gates, vec![0, 1, 2, 1, 0]);
}
