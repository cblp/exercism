/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    if !code
        .bytes()
        .all(|b| b.is_ascii_digit() || b.is_ascii_whitespace())
    {
        return false;
    }
    let digits: Vec<_> = code.bytes().filter(|&b| b.is_ascii_digit()).collect();
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
