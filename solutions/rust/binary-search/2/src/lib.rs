pub fn find<I: Ord, A: AsRef<[I]>>(array: A, key: I) -> Option<usize> {
    find_ref(array.as_ref(), key)
}

pub fn find_ref<I: Ord>(array: &[I], key: I) -> Option<usize> {
    if array.is_empty() {
        None
    } else {
        let m = array.len() / 2;
        use std::cmp::Ordering::*;
        match key.cmp(&array[m]) {
            Less => find_ref(&array[..m], key),
            Equal => Some(m),
            Greater => find_ref(&array[m + 1..], key).map(|x| m + 1 + x),
        }
    }
}
