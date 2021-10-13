extern "C" {
    fn __quantum__rt__qubit_allocate() -> u32;
    fn __quantum__rt__qubit_release(q: u32);
    fn __quantum__qis__h__body(q: u32);
}

fn main() {
    let q = unsafe { __quantum__rt__qubit_allocate() };
    unsafe {
        __quantum__qis__h__body(q);
    }
    unsafe {
        __quantum__rt__qubit_release(q);
    }
}
