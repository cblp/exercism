/// Return the Hamming distance between the strings,
/// or None if the lengths are mismatched.
pub fn hamming_distance(s1: &str, s2: &str) -> Option<usize> {
    (s1.len() == s2.len()).then_some(
        s1.bytes()
            .zip(s2.bytes())
            .map(|(a, b)| if a == b { 0 } else { 1 })
            .sum(),
    )
}
