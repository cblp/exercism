use itertools::Itertools;

fn transpose(a: &Vec<String>) -> Vec<String> {
    let n = a[0].len();
    let mut r = vec!["".to_string(); n];
    for row in a {
        for (i, c) in row.chars().enumerate() {
            r[i].push(c);
        }
    }
    r
}

pub fn encrypt(input: &str) -> String {
    let message = input
        .bytes()
        .filter_map(|c| {
            c.is_ascii_alphanumeric().then_some(c.to_ascii_lowercase())
        })
        .map(|c| c as char)
        .collect::<String>();

    let mut c = 0;
    let mut r = 0;
    while c * r < message.len() {
        if r < c { r += 1 } else { c += 1 }
    }
    if c == 0 {
        return "".to_string();
    }
    let c = c; // unmut

    let mut square: Vec<String> = message
        .chars()
        .chunks(c)
        .into_iter()
        .map(|c| c.collect())
        .collect();
    {
        let last_line = &mut square[r - 1];
        if last_line.len() < c {
            last_line.push_str(" ".repeat(c - last_line.len()).as_str());
        }
    }

    transpose(&square).join(" ")
}
