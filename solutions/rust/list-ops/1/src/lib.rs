use std::iter;

/// Yields each item of a and then each item of b
pub fn append<I, J>(mut a: I, mut b: J) -> impl Iterator<Item = I::Item>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    iter::from_fn(move || match a.next() {
        Some(x) => Some(x),
        None => b.next(),
    })
}

/// Combines all items in all nested iterators inside into one flattened iterator
pub fn concat<I>(
    mut outer_iterator: I,
) -> impl Iterator<Item = <I::Item as Iterator>::Item>
where
    I: Iterator,
    I::Item: Iterator,
{
    let mut outer_item = outer_iterator.next();
    iter::from_fn(move || {
        loop {
            match outer_item {
                None => return None,
                Some(ref mut inner_iterator) => match inner_iterator.next() {
                    Some(inner_item) => return Some(inner_item),
                    None => outer_item = outer_iterator.next(),
                },
            }
        }
    })
}

/// Returns an iterator of all items in iter for which `predicate(item)` is true
pub fn filter<I, F>(mut i: I, p: F) -> impl Iterator<Item = I::Item>
where
    I: Iterator,
    F: Fn(&I::Item) -> bool,
{
    iter::from_fn(move || {
        loop {
            match i.next() {
                None => return None,
                Some(x) => {
                    if p(&x) {
                        return Some(x);
                    }
                }
            }
        }
    })
}

pub fn length<I: Iterator>(i: I) -> usize {
    let mut n = 0;
    for _ in i {
        n += 1
    }
    n
}

/// Returns an iterator of the results of applying `function(item)` on all iter items
pub fn map<I, F, U>(mut i: I, f: F) -> impl Iterator<Item = U>
where
    I: Iterator,
    F: Fn(I::Item) -> U,
{
    iter::from_fn(move || i.next().map(&f))
}

pub fn foldl<I, F, U>(i: I, mut z: U, f: F) -> U
where
    I: Iterator,
    F: Fn(U, I::Item) -> U,
{
    for x in i {
        z = f(z, x)
    }
    z
}

pub fn foldr<I, F, U>(i: I, mut z: U, f: F) -> U
where
    I: DoubleEndedIterator,
    F: Fn(U, I::Item) -> U,
{
    for x in i.rev() {
        z = f(z, x)
    }
    z
}

/// Returns an iterator with all the original items, but in reverse order
pub fn reverse<I: DoubleEndedIterator>(
    mut i: I,
) -> impl Iterator<Item = I::Item> {
    iter::from_fn(move || i.next_back())
}
