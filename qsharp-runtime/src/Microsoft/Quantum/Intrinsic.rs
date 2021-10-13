use crate::{Array, QSharpIntrinsics};

pub fn X(sim: &mut impl QSharpIntrinsics, q: usize) {
    sim.x(q);
}

pub fn X_adj(sim: &mut impl QSharpIntrinsics, q: usize) {
    sim.x(q);
}

pub fn X_ctl(sim: &mut impl QSharpIntrinsics, ctls: Array<usize>, q: usize) {
    sim.mcx(&ctls, q);
}

pub fn H(sim: &mut impl QSharpIntrinsics, q: usize) {
    sim.h(q);
}

pub fn S(sim: &mut impl QSharpIntrinsics, q: usize) {
    sim.s(q);
}

pub fn T(sim: &mut impl QSharpIntrinsics, q: usize) {
    sim.t(q);
}

pub fn T_adj(sim: &mut impl QSharpIntrinsics, q: usize) {
    sim.t_adj(q);
}

pub fn CNOT(sim: &mut impl QSharpIntrinsics, ctl: usize, tgt: usize) {
    sim.cnot(ctl, tgt);
}

pub fn CNOT_adj(sim: &mut impl QSharpIntrinsics, ctl: usize, tgt: usize) {
    sim.cnot(ctl, tgt);
}

pub fn M(sim: &mut impl QSharpIntrinsics, q: usize) -> bool {
    sim.m(q)
}
