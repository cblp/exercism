struct Encoder {
    current_char: char,
    current_count: usize,
    res: String,
}

impl Encoder {
    fn new(current_char: char) -> Self {
        Self {
            current_char,
            current_count: 1,
            res: String::new(),
        }
    }

    fn flush(&mut self) {
        if self.current_count != 1 {
            self.res.push_str(&self.current_count.to_string());
        };
        self.res.push(self.current_char);
        self.current_count = 1;
    }
}

pub fn encode(source: &str) -> String {
    let mut chars = source.chars();
    let mut encoder = if let Some(c) = chars.next() {
        Encoder::new(c)
    } else {
        return String::new();
    };

    for c in chars {
        if c == encoder.current_char {
            encoder.current_count += 1;
        } else {
            encoder.flush();
            encoder.current_char = c;
        }
    }
    if encoder.current_count != 0 {
        encoder.flush();
    }

    encoder.res
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
