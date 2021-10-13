use std::rc::Rc;

use crate::{Array, QSharpIntrinsics};

#[inline]
pub fn EmptyArray<Sim: QSharpIntrinsics, T>(_sim: &mut Sim) -> Array<T> {
    Rc::new(Vec::new())
}

#[inline]
pub fn Head<Sim: QSharpIntrinsics, T: Clone>(_sim: &mut Sim, array: Array<T>) -> T {
    array[0].clone()
}

#[inline]
pub fn Head_adj<Sim: QSharpIntrinsics, T: Clone>(_sim: &mut Sim, array: Array<T>) -> T {
    array[0].clone()
}

#[inline]
pub fn Tail<Sim: QSharpIntrinsics, T: Clone>(_sim: &mut Sim, array: Array<T>) -> T {
    array[array.len() - 1].clone()
}

#[inline]
pub fn Tail_adj<Sim: QSharpIntrinsics, T: Clone>(_sim: &mut Sim, array: Array<T>) -> T {
    array[array.len() - 1].clone()
}

#[inline]
pub fn Rest<Sim: QSharpIntrinsics, T: Clone>(_sim: &mut Sim, array: Array<T>) -> Array<T> {
    Rc::new(array[1..].to_vec())
}

#[inline]
pub fn Rest_adj<Sim: QSharpIntrinsics, T: Clone>(_sim: &mut Sim, array: Array<T>) -> Array<T> {
    Rc::new(array[1..].to_vec())
}
