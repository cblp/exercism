pub fn count(rows: &[&str]) -> u32 {
    let mut count = 0;
    for (row_start, row) in rows.iter().enumerate() {
        for (col_start, c) in row.chars().enumerate() {
            if c == '+' {
                count += num_rects_from(
                    &rows.iter().map(|row| row.as_bytes()).collect::<Vec<_>>(),
                    row_start,
                    col_start,
                );
            }
        }
    }
    count
}

fn num_rects_from(rows: &[&[u8]], row_start: usize, col_start: usize) -> u32 {
    let mut n = 0;
    for col_end in col_start + 1..rows[row_start].len() {
        match rows[row_start][col_end] {
            b'+' => {
                n += num_rects_from2(rows, row_start, col_start, col_end);
            }
            b'-' => continue,
            _ => break,
        }
    }
    n
}

fn num_rects_from2(
    rows: &[&[u8]],
    row_start: usize,
    col_start: usize,
    col_end: usize,
) -> u32 {
    let mut n = 0;
    for row_end in row_start + 1..rows.len() {
        match rows[row_end][col_start] {
            b'+' => {
                if is_square(rows, row_start, row_end, col_start, col_end) {
                    n += 1;
                }
            }
            b'|' => continue,
            _ => break,
        }
    }
    n
}

fn is_square(
    rows: &[&[u8]],
    row_start: usize,
    row_end: usize,
    col_start: usize,
    col_end: usize,
) -> bool {
    rows[row_start][col_start] == b'+'
        && rows[row_start][col_end] == b'+'
        && rows[row_end][col_start] == b'+'
        && rows[row_end][col_end] == b'+'
        && (col_start + 1..col_end).all(|col| {
            (rows[row_start][col] == b'-' || rows[row_start][col] == b'+')
                && (rows[row_end][col] == b'-' || rows[row_end][col] == b'+')
        })
        && (row_start + 1..row_end).all(|row| {
            (rows[row][col_start] == b'|' || rows[row][col_start] == b'+')
                && (rows[row][col_end] == b'|' || rows[row][col_end] == b'+')
        })
}
