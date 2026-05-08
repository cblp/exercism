pub fn count(rows: &[&str]) -> u32 {
    let rows: Vec<&[u8]> = rows.iter().map(|row| row.as_bytes()).collect();
    let rows = rows.as_slice();
    rows.iter()
        .enumerate()
        .flat_map(|(row_start, row)| {
            row.iter().enumerate().filter(|&(_, &c)| c == b'+').map(
                move |(col_start, _)| {
                    num_rects_from(rows, row_start, col_start)
                },
            )
        })
        .sum()
}

fn num_rects_from(rows: &[&[u8]], row_start: usize, col_start: usize) -> u32 {
    (col_start + 1..rows[row_start].len())
        .take_while(|&col| matches!(rows[row_start][col], b'+' | b'-'))
        .filter(|&col| rows[row_start][col] == b'+')
        .map(|col_end| num_rects_from2(rows, row_start, col_start, col_end))
        .sum()
}

fn num_rects_from2(
    rows: &[&[u8]],
    row_start: usize,
    col_start: usize,
    col_end: usize,
) -> u32 {
    (row_start + 1..rows.len())
        .take_while(|&row| matches!(rows[row][col_start], b'+' | b'|'))
        .filter(|&row| rows[row][col_start] == b'+')
        .map(|row_end| {
            is_rect(rows, row_start, row_end, col_start, col_end) as u32
        })
        .sum()
}

fn is_rect(
    rows: &[&[u8]],
    row_start: usize,
    row_end: usize,
    col_start: usize,
    col_end: usize,
) -> bool {
    rows[row_end][col_end] == b'+'
        && (col_start + 1..col_end).all(|col| {
            matches!(rows[row_start][col], b'-' | b'+')
                && matches!(rows[row_end][col], b'-' | b'+')
        })
        && (row_start + 1..row_end).all(|row| {
            matches!(rows[row][col_start], b'|' | b'+')
                && matches!(rows[row][col_end], b'|' | b'+')
        })
}
