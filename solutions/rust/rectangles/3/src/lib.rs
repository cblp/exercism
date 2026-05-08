pub fn count(rows: &[&str]) -> u32 {
    let rows: Vec<&[u8]> = rows.iter().map(|r| r.as_bytes()).collect();
    let h = h_runs(&rows);
    let v = v_runs(&rows);
    let rows = rows.as_slice();
    let h = h.as_slice();
    let v = v.as_slice();

    rows.iter()
        .enumerate()
        .flat_map(move |(r0, row)| {
            row.iter()
                .enumerate()
                .filter(|&(_, &c)| c == b'+')
                .flat_map(move |(c0, _)| {
                    (c0 + 1..c0 + h[r0][c0])
                        .filter(move |&c1| rows[r0][c1] == b'+')
                        .flat_map(move |c1| {
                            (r0 + 1..r0 + v[r0][c0]).filter(move |&r1| {
                                rows[r1][c0] == b'+'
                                    && rows[r1][c1] == b'+'
                                    && h[r1][c0] > c1 - c0
                                    && v[r0][c1] > r1 - r0
                            })
                        })
                })
        })
        .count() as u32
}

// h[r][c] = длина прогона '+'|'-' вправо от (r, c)
fn h_runs(rows: &[&[u8]]) -> Vec<Vec<usize>> {
    rows.iter()
        .map(|row| {
            let mut h = vec![0usize; row.len()];
            let mut run = 0usize;
            for (c, &b) in row.iter().enumerate().rev() {
                run = if matches!(b, b'+' | b'-') { run + 1 } else { 0 };
                h[c] = run;
            }
            h
        })
        .collect()
}

// v[r][c] = длина прогона '+'|'|' вниз от (r, c)
fn v_runs(rows: &[&[u8]]) -> Vec<Vec<usize>> {
    let ncols = rows.iter().map(|r| r.len()).max().unwrap_or(0);
    let mut v: Vec<Vec<usize>> =
        rows.iter().map(|r| vec![0usize; r.len()]).collect();
    let mut run = vec![0usize; ncols];
    for (r, row) in rows.iter().enumerate().rev() {
        for (c, &b) in row.iter().enumerate() {
            run[c] = if matches!(b, b'+' | b'|') {
                run[c] + 1
            } else {
                0
            };
            v[r][c] = run[c];
        }
    }
    v
}
