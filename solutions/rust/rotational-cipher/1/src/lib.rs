pub fn rotate(input: &str, key: u8) -> String {
    input
        .bytes()
        .map(|mut c| {
            if c.is_ascii_lowercase() {
                c += key;
                if c > b'z' {
                    c -= 26
                }
            } else if c.is_ascii_uppercase() {
                c += key;
                if c > b'Z' {
                    c -= 26
                }
            }
            c as char
        })
        .collect()
}
