pub fn abbreviate(phrase: &str) -> String {
    if phrase.is_empty() {
        return String::new();
    }

    let mut res = String::new();
    let mut prev = phrase.as_bytes()[0];
    if prev.is_ascii_alphabetic() {
        res.push(prev.to_ascii_uppercase() as char);
    }
    for cur in phrase.bytes().skip(1) {
        if (!(prev.is_ascii_alphabetic() || prev == b'\'')
            && cur.is_ascii_alphabetic())
            || (prev.is_ascii_lowercase() && cur.is_ascii_uppercase())
        {
            res.push(cur.to_ascii_uppercase() as char);
        }
        prev = cur;
    }
    res
}
