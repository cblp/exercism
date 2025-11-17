use std::collections::HashSet;

/// Determine whether a sentence is a pangram.
pub fn is_pangram(sentence: &str) -> bool {
    let mut alphabet = HashSet::<u8>::from_iter(b'a'..=b'z');
    for c in sentence.bytes() {
        if c.is_ascii_alphabetic() {
            alphabet.remove(&c.to_ascii_lowercase());
        }
    }
    alphabet.is_empty()
}
