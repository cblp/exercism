pub fn rotate(input: &str, key: u8) -> String {
    let rot = |mut c, z| {
        c += key;
        if c > z {
            c -= 26
        }
        c
    };
    input
        .bytes()
        .map(|c| {
            (if c.is_ascii_lowercase() {
                rot(c, b'z')
            } else if c.is_ascii_uppercase() {
                rot(c, b'Z')
            } else {
                c
            }) as char
        })
        .collect()
}
