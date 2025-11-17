use std::iter::zip;

/// Return the Hamming distance between the strings,
/// or None if the lengths are mismatched.
pub fn hamming_distance(s1: &str, s2: &str) -> Option<usize> {
    (s1.len() == s2.len()).then_some(
        zip(s1.bytes(), s2.bytes())
            .map(|(a, b)| if a == b { 0 } else { 1 })
            .sum(),
    )
}
