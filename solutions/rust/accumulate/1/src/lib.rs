pub fn map<A, B>(input: Vec<A>, mut f: impl FnMut(A) -> B) -> Vec<B> {
    let mut bs = vec![];
    for a in input {
        bs.push(f(a));
    }
    bs
}
