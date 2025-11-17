pub fn is_armstrong_number(num: u32) -> bool {
    let s = num.to_string();
    s.bytes()
        .map(|c| ((c - b'0') as u32).pow(s.len() as u32))
        .sum::<u32>()
        == num
}
