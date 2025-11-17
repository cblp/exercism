pub fn encode(source: &str) -> String {
    let mut current_char;
    let mut current_count;
    let mut res = String::new();

    let mut chars = source.chars();
    if let Some(c) = chars.next() {
        current_char = c;
        current_count = 1;
    } else {
        return String::new();
    }

    let mut flush = |current_char, current_count: usize| {
        if current_count != 1 {
            res.push_str(&current_count.to_string());
        };
        res.push(current_char);
    };

    for c in chars {
        if c == current_char {
            current_count += 1;
        } else {
            flush(current_char, current_count);
            current_char = c;
            current_count = 1;
        }
    }
    if current_count != 0 {
        flush(current_char, current_count);
    }

    res
}

pub fn decode(source: &str) -> String {
    let mut current_count: usize = 0;
    let mut res = String::new();

    for c in source.chars() {
        if c.is_ascii_digit() {
            current_count = current_count * 10 + (c as u8 - b'0') as usize;
        } else {
            if current_count == 0 {
                res.push(c);
            } else {
                res.extend([c].repeat(current_count));
            }
            current_count = 0;
        }
    }

    res
}
