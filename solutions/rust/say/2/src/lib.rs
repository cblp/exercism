fn to_digits(mut n: u64) -> Vec<u8> {
    let mut r = vec![];
    while n != 0 {
        r.push((n % 10) as u8);
        n /= 10;
    }
    r
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

fn str<const N: usize>(names: [&str; N], digit: u8) -> String {
    names[(digit - 1) as usize].to_string()
}

fn push_str(v: &mut Vec<String>, s: &str) {
    v.push(s.to_string());
}

fn push_str_<const N: usize>(v: &mut Vec<String>, names: [&str; N], digit: u8) {
    v.push(names[(digit - 1) as usize].to_string());
}

use std::ops::Range;

fn islice(slice: &[u8], r: Range<usize>) -> &[u8] {
    &slice[r.start.min(slice.len())..r.end.min(slice.len())]
}

fn say_group(digits: &[u8]) -> Vec<String> {
    let mut r = vec![];
    let digits: Vec<u8> = {
        let mut digits_tmp = digits.to_vec();
        while digits_tmp.len() < 3 {
            digits_tmp.push(0);
        }
        digits_tmp
    };
    match (digits[0], digits[1]) {
        (0, 0) => {}
        (0, d1) => push_str_(&mut r, TENS, d1),
        (d0, 0) => push_str_(&mut r, ONES, d0),
        (d0, 1) => push_str_(&mut r, TEENS, d0),
        (d0, d1) => r.push(str(TENS, d1) + "-" + &str(ONES, d0)),
    }
    if digits[2] != 0 {
        push_str(&mut r, "hundred");
        push_str_(&mut r, ONES, digits[2]);
    }
    r
}

fn say_parts(digits: &[u8]) -> Vec<String> {
    let mut r = vec![];
    r.append(&mut say_group(islice(digits, 0..3)));
    for (n, title) in [
        (3, "thousand"),
        (6, "million"),
        (9, "billion"),
        (12, "trillion"),
        (15, "quadrillion"),
        (18, "quintillion"),
    ] {
        let group = say_group(islice(digits, n..n + 3));
        if !group.is_empty() {
            push_str(&mut r, title);
            r.append(&mut { group });
        }
    }
    r
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
