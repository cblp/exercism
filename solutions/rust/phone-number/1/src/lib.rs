// use regex;

pub fn number(user_number: &str) -> Option<String> {
    let digits = user_number
        .chars()
        .filter(char::is_ascii_digit)
        .collect::<Vec<_>>();
    let digits = match digits.len() {
        10 => digits,
        11 if digits[0] == '1' => digits[1..].to_vec(),
        _ => return None,
    };
    if digits[0] < '2' || digits[3] < '2' {
        return None;
    }
    Some(String::from_iter(digits))
}
