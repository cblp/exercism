fn to_digits(mut n: u64) -> Vec<u8> {
    let mut result = vec![];
    while n != 0 {
        result.push((n % 10) as u8);
        n /= 10;
    }
    result
}

const ONES: [&str; 9] = [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
];

const TENS: [&str; 9] = [
    "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
    "ninety",
];

const TEENS: [&str; 9] = [
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
];

fn say_group(digits: Vec<u8>) -> Vec<String> {
    let mut result = vec![];
    let digit0 = digits[0];
    let digit1 = if digits.len() > 1 { digits[1] } else { 0 };
    let digit2 = if digits.len() > 2 { digits[2] } else { 0 };
    if digit0 != 0 {
        if digit1 == 1 {
            result.push(TEENS[(digit0 - 1) as usize].to_string());
        } else if digit1 != 0 {
            result.push(
                TENS[(digits[1] - 1) as usize].to_string()
                    + "-"
                    + ONES[(digits[0] - 1) as usize],
            );
        } else {
            result.push(ONES[(digits[0] - 1) as usize].to_string())
        }
    } else if digit1 != 0 {
        result.push(TENS[(digits[1] - 1) as usize].to_string())
    }
    if digit2 != 0 {
        result.push("hundred".to_string());
        result.push(ONES[(digits[2] - 1) as usize].to_string());
    }
    result
}

fn say_parts(digits: &[u8]) -> Vec<String> {
    let mut result = vec![];
    result.append(&mut say_group(Vec::from_iter(
        digits.iter().take(3).copied(),
    )));
    if digits.len() > 3 && digits.iter().skip(3).take(3).any(|d| *d != 0) {
        result.push("thousand".to_string());
        result.append(&mut say_group(Vec::from_iter(
            digits.iter().skip(3).take(3).copied(),
        )));
    }
    if digits.len() > 6 && digits.iter().skip(6).take(3).any(|d| *d != 0) {
        result.push("million".to_string());
        result.append(&mut say_group(Vec::from_iter(
            digits.iter().skip(6).take(3).copied(),
        )));
    }
    if digits.len() > 9 && digits.iter().skip(9).take(3).any(|d| *d != 0) {
        result.push("billion".to_string());
        result.append(&mut say_group(Vec::from_iter(
            digits.iter().skip(9).take(3).copied(),
        )));
    }
    if digits.len() > 12 && digits.iter().skip(12).take(3).any(|d| *d != 0) {
        result.push("trillion".to_string());
        result.append(&mut say_group(Vec::from_iter(
            digits.iter().skip(12).take(3).copied(),
        )));
    }
    if digits.len() > 15 && digits.iter().skip(15).take(3).any(|d| *d != 0) {
        result.push("quadrillion".to_string());
        result.append(&mut say_group(Vec::from_iter(
            digits.iter().skip(15).take(3).copied(),
        )));
    }
    if digits.len() > 18 && digits.iter().skip(18).take(3).any(|d| *d != 0) {
        result.push("quintillion".to_string());
        result.append(&mut say_group(Vec::from_iter(
            digits.iter().skip(18).take(3).copied(),
        )));
    }
    result
}

pub fn encode(n: u64) -> String {
    match n {
        0 => "zero".to_string(),
        _ => {
            let mut parts = say_parts(&to_digits(n));
            parts.reverse();
            parts.join(" ")
        }
    }
}
