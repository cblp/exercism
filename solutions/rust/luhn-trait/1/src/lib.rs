pub trait Luhn {
    fn valid_luhn(&self) -> bool;
}

/// Here is the example of how to implement custom Luhn trait
/// for the &str type. Naturally, you can implement this trait
/// by hand for every other type presented in the test suite,
/// but your solution will fail if a new type is presented.
/// Perhaps there exists a better solution for this problem?
impl<T: ToString> Luhn for T {
    fn valid_luhn(&self) -> bool {
        if !self
            .to_string()
            .bytes()
            .all(|b| b.is_ascii_digit() || b.is_ascii_whitespace())
        {
            return false;
        }
        let digits: Vec<_> = self
            .to_string()
            .bytes()
            .filter(|&b| b.is_ascii_digit())
            .collect();
        if digits == [b'0'] {
            return false;
        }
        digits
            .iter()
            .rev()
            .enumerate()
            .map(|(i, b)| {
                let mut d = (b - b'0') * (1 + i as u8 % 2);
                if d > 9 {
                    d -= 9
                }
                d as usize
            })
            .sum::<usize>()
            % 10
            == 0
    }
}
