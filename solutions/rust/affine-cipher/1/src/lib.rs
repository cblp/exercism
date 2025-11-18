#[derive(Debug, Eq, PartialEq)]
pub enum AffineCipherError {
    NotCoprime(i32),
}

use AffineCipherError::*;

fn is_coprime_with_26(a: i32) -> bool {
    [1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25].contains(&a)
}

pub fn encode(
    plaintext: &str,
    a: i32,
    b: i32,
) -> Result<String, AffineCipherError> {
    if !is_coprime_with_26(a) {
        return Err(NotCoprime(a));
    }

    let mut s = String::new();
    let mut n: usize = 0;
    let mut push = |c| {
        if n != 0 && n.is_multiple_of(5) {
            s.push(' ');
        }
        s.push(c);
        n += 1;
    };

    for x in plaintext.bytes() {
        if x.is_ascii_alphabetic() {
            let i = (x.to_ascii_lowercase() - b'a') as i32;
            let e = (a * i + b) % 26;
            push((b'a' + e as u8) as char);
        } else if x.is_ascii_digit() {
            push(x as char);
        }
    }
    Ok(s)
}

fn inv(a: i32) -> i32 {
    match a {
        1 => 1,
        3 => 9,
        5 => 21,
        7 => 15,
        9 => 3,
        11 => 19,
        15 => 7,
        17 => 23,
        19 => 11,
        21 => 5,
        23 => 17,
        25 => 25,
        _ => panic!("inv({a})"),
    }
}

pub fn decode(
    ciphertext: &str,
    a: i32,
    b: i32,
) -> Result<String, AffineCipherError> {
    if !is_coprime_with_26(a) {
        return Err(NotCoprime(a));
    }

    let a_inv = inv(a);

    let mut s = String::new();
    for x in ciphertext.bytes() {
        if x.is_ascii_lowercase() {
            let y = (x - b'a') as i32;
            let d = (a_inv * (y - b)).rem_euclid(26);
            s.push((b'a' + d as u8) as char);
        } else if x.is_ascii_digit() {
            s.push(x as char);
        }
    }
    Ok(s)
}
