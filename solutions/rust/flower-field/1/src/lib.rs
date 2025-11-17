fn mark(garden: &[&str], i: usize, j: usize) -> char {
    let rows = garden.len();
    let mut count = 0;
    let cols = if rows > 0 { garden[i].len() } else { 0 };
    let top = if i == 0 { 0 } else { i - 1 };
    let bottom = if i + 1 >= rows { rows - 1 } else { i + 1 };
    let left = if j == 0 { 0 } else { j - 1 };
    let right = if j + 1 >= cols { cols - 1 } else { j + 1 };
    for ni in top..=bottom {
        for nj in left..=right {
            if ni == i && nj == j {
                continue;
            }
            if garden[ni].as_bytes()[nj] == b'*' {
                count += 1;
            }
        }
    }
    if count == 0 {
        ' '
    } else {
        (b'0' + count) as char
    }
}

pub fn annotate(garden: &[&str]) -> Vec<String> {
    garden
        .iter()
        .enumerate()
        .map(|(i, row)| {
            row.bytes()
                .enumerate()
                .map(|(j, cell)| {
                    if cell == b'*' {
                        '*'
                    } else {
                        mark(garden, i, j)
                    }
                })
                .collect()
        })
        .collect()
}
