use crate::{Array, QSharpIntrinsics};

pub fn AssertAllZero<Sim: QSharpIntrinsics>(_sim: &mut Sim, _qubits: Array<usize>) {
    // TODO
}

pub fn AssertAllZero_adj<Sim: QSharpIntrinsics>(_sim: &mut Sim, _qubits: Array<usize>) {
    // TODO
}

#[inline]
pub fn Fact<Sim: QSharpIntrinsics>(_sim: &mut Sim, actual: bool, message: String) {
    assert!(actual, "{}", message);
}

#[inline]
pub fn Fact_adj<Sim: QSharpIntrinsics>(_sim: &mut Sim, actual: bool, message: String) {
    assert!(actual, "{}", message);
}

#[inline]
pub fn Fact_ctl<Sim: QSharpIntrinsics>(
    _sim: &mut Sim,
    _ctls: Array<usize>,
    (actual, message): (bool, String),
) {
    assert!(actual, "{}", message);
}

#[inline]
pub fn Fact_ctl_adj<Sim: QSharpIntrinsics>(
    _sim: &mut Sim,
    _ctls: Array<usize>,
    (actual, message): (bool, String),
) {
    assert!(actual, "{}", message);
}

#[inline]
pub fn EqualityFactI<Sim: QSharpIntrinsics>(
    sim: &mut Sim,
    actual: i64,
    expected: i64,
    message: String,
) {
    Fact(sim, actual == expected, message);
}

#[inline]
pub fn EqualityFactI_adj<Sim: QSharpIntrinsics>(
    sim: &mut Sim,
    actual: i64,
    expected: i64,
    message: String,
) {
    Fact_adj(sim, actual == expected, message);
}

#[inline]
pub fn EqualityFactI_ctl<Sim: QSharpIntrinsics>(
    sim: &mut Sim,
    _ctls: Array<usize>,
    (actual, expected, message): (i64, i64, String),
) {
    Fact(sim, actual == expected, message);
}

#[inline]
pub fn EqualityFactI_ctl_adj<Sim: QSharpIntrinsics>(
    sim: &mut Sim,
    _ctls: Array<usize>,
    (actual, expected, message): (i64, i64, String),
) {
    Fact(sim, actual == expected, message);
}
