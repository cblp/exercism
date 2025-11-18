#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    IncompleteNumber,
}
use Error::*;

fn to_base_128(mut n: u32) -> Vec<u8> {
    if n == 0 {
        return vec![0];
    }
    let mut r = vec![];
    while n != 0 {
        let digit = n % 128;
        n /= 128;
        r.push(digit as u8);
    }
    r.reverse();
    r
}

/// Convert a list of numbers to a stream of bytes encoded with variable length
/// encoding.
pub fn to_bytes(values: &[u32]) -> Vec<u8> {
    let mut r = vec![];
    for value in values {
        let digits = to_base_128(*value);
        for (i, digit) in digits.iter().enumerate() {
            r.push((if i != digits.len() - 1 { 0x80 } else { 0 }) | digit);
        }
    }
    r
}

/// Given a stream of bytes, extract all numbers which are encoded in there.
pub fn from_bytes(bytes: &[u8]) -> Result<Vec<u32>, Error> {
    let mut r = vec![];
    let mut n: u32 = 0;
    let mut in_sequence = false;
    for byte in bytes {
        n = n * 128 + (byte & 0x7F) as u32;
        in_sequence = byte & 0x80 != 0;
        if !in_sequence {
            r.push(n);
            n = 0;
        }
    }
    if in_sequence {
        return Err(IncompleteNumber);
    }
    Ok(r)
}
