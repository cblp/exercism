pub fn series(digits: &str, n: usize) -> Vec<String> {
    if n > digits.len() {
        vec![]
    } else {
        (0..=digits.len() - n)
            .map(|i| String::from(&digits[i..i + n]))
            .collect()
    }
}
