use std::rc::Rc;

pub trait QSharpIntrinsics {
    fn x(&mut self, q: usize);
    fn y(&mut self, q: usize);
    fn z(&mut self, q: usize);
    fn h(&mut self, q: usize);
    fn s(&mut self, q: usize);
    fn t(&mut self, q: usize);
    fn t_adj(&mut self, q: usize);
    fn cnot(&mut self, ctl: usize, tgt: usize);
    fn m(&mut self, q: usize) -> bool;

    fn mcx(&mut self, ctls: &[usize], q: usize);

    fn allocate(&mut self) -> usize;
    fn allocate_many(&mut self, count: usize) -> Array<usize>;
    fn release(&mut self, q: usize);
    fn release_many(&mut self, qs: Array<usize>);
}

pub type Array<T> = Rc<Vec<T>>;

//impl<T> Iterator for Array<T> {
//    type Item = T;
//
//    fn next(&mut self) -> Option<Self::Item> {
//        todo!()
//    }
//}
