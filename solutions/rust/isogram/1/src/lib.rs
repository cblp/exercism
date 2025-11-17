use std::collections::HashSet;

pub fn check(candidate: &str) -> bool {
    let mut counter = HashSet::new();
    for b in candidate.bytes() {
        if !b.is_ascii_alphabetic() {
            continue;
        }
        let b = b.to_ascii_lowercase();
        match counter.get(&b) {
            Some(_) => return false,
            None => counter.insert(b),
        };
    }
    true
}
