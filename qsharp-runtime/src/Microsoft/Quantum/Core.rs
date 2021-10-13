use crate::{Array, QSharpIntrinsics};

#[inline]
pub fn Length<Sim: QSharpIntrinsics, T>(_sim: &mut Sim, array: Array<T>) -> i64 {
    array.len() as i64
}

#[inline]
pub fn Length_adj<Sim: QSharpIntrinsics, T>(_sim: &mut Sim, array: Array<T>) -> i64 {
    array.len() as i64
}

#[inline]
pub fn Length_ctl<Sim: QSharpIntrinsics, T>(
    _sim: &mut Sim,
    _ctls: Array<usize>,
    array: Array<T>,
) -> i64 {
    array.len() as i64
}

#[inline]
pub fn Length_ctl_adj<Sim: QSharpIntrinsics, T>(
    _sim: &mut Sim,
    _ctls: Array<usize>,
    array: Array<T>,
) -> i64 {
    array.len() as i64
}
