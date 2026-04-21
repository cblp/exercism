#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
}

const DIGITS: [&str; 10] = [
    "_| ||", // 0
    "   | ", // 1
    "_ _||", // 2
    "_ _| ", // 3
    " |_| ", // 4
    "_|_  ", // 5
    "_|_ |", // 6
    "_  | ", // 7
    "_|_||", // 8
    "_|_| ", // 9
];

fn recognize_digit(pattern: &str) -> char {
    DIGITS
        .iter()
        .position(|&d| d == pattern)
        .map_or('?', |n| (b'0' + n as u8) as char)
}

fn slice_col(row: &str, start: usize) -> &str {
    let end = start + 3;
    if end <= row.len() {
        &row[start..end]
    } else if start < row.len() {
        &row[start..]
    } else {
        ""
    }
}

fn digits_in_group<'a>(
    group: &[&'a str; 4],
) -> impl Iterator<Item = String> + 'a {
    let max_width = group.iter().map(|r| r.len()).max().unwrap_or(0);
    let num_digits = max_width / 3;
    let [r0, r1, r2, _] = *group;
    (0..num_digits).map(move |i| {
        let start = i * 3;
        let s0 = slice_col(r0, start);
        let s1 = slice_col(r1, start);
        let s2 = slice_col(r2, start);
        format!(
            "{}{}{}",
            s0.get(1..2).unwrap_or(" "),
            s1,
            s2.get(..1).unwrap_or(" "),
        )
    })
}

pub fn convert(input: &str) -> Result<String, Error> {
    let rows: Vec<&str> = input.split('\n').collect();
    let row_count = rows.len();

    if !row_count.is_multiple_of(4) {
        return Err(Error::InvalidRowCount(row_count));
    }

    if let Some(n) = rows.iter().map(|r| r.len()).find(|n| !n.is_multiple_of(3))
    {
        return Err(Error::InvalidColumnCount(n));
    }

    let result = rows
        .chunks_exact(4)
        .map(|chunk| {
            let group: &[&str; 4] = chunk.try_into().unwrap();
            digits_in_group(group)
                .map(|p| recognize_digit(&p))
                .collect::<String>()
        })
        .collect::<Vec<_>>()
        .join(",");

    Ok(result)
}
