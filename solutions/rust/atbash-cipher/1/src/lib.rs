use itertools::Itertools;

fn mirror(c: u8) -> Option<char> {
    c   .is_ascii_alphanumeric()
        .then(
            ||
            if c.is_ascii_alphabetic() {219 - c.to_ascii_lowercase()} else {c}
            as char
        )
}

/// "Encipher" with the Atbash cipher.
pub fn encode(plain: &str) -> String {
    plain
        .bytes()
        .filter_map(mirror)
        .chunks(5)
        .into_iter()
        .map(|s| s.collect::<String>())
        .join(" ")
}

/// "Decipher" with the Atbash cipher.
pub fn decode(cipher: &str) -> String {
    cipher.bytes().filter_map(mirror).collect()
}
