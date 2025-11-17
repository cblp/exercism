pub fn find(array: &[i32], key: i32) -> Option<usize> {
    if array.is_empty() {
        None
    } else {
        let m = array.len() / 2;
        use std::cmp::Ordering::*;
        match key.cmp(&array[m]) {
            Less => find(&array[..m], key),
            Equal => Some(m),
            Greater => find(&array[m + 1..], key).map(|x| m + 1 + x),
        }
    }
}
