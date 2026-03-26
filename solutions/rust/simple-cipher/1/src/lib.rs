use rand::distr::SampleString;
use std::iter::zip;

fn rotate(key: &str, s: &str, add: bool) -> Option<String> {
    if key.is_empty() {
        return None;
    }
    let key: Vec<u8> = key
        .bytes()
        .map(|k| k.is_ascii_lowercase().then(|| k - b'a'))
        .collect::<Option<_>>()?;
    Some(
        zip(s.bytes(), key.into_iter().cycle())
            .map(|(mut i, k)| {
                if add {
                    i += k;
                    if i > b'z' {
                        i -= 26
                    }
                } else {
                    i -= k;
                    if i < b'a' {
                        i += 26
                    }
                }
                i as char
            })
            .collect(),
    )
}

pub fn encode(key: &str, s: &str) -> Option<String> {
    rotate(key, s, true)
}

pub fn decode(key: &str, s: &str) -> Option<String> {
    rotate(key, s, false)
}

pub fn encode_random(s: &str) -> (String, String) {
    let key = rand::distr::Uniform::new_inclusive('a', 'z')
        .unwrap()
        .sample_string(&mut rand::rng(), 100);
    let cipher = encode(&key, s).unwrap();
    (key, cipher)
}
