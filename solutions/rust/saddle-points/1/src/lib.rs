fn find_max_indexes(row: &[u64]) -> Vec<usize> {
    if row.is_empty() {
        return vec![];
    }
    let mut res = vec![0];
    for j in 1..row.len() {
        use std::cmp::Ordering::*;
        match row[res[0]].cmp(&row[j]) {
            Less => res = vec![j],
            Equal => res.push(j),
            Greater => {}
        }
    }
    res
}

fn is_min_in_column(input: &[Vec<u64>], i: usize, j: usize) -> bool {
    let value = input[i][j];
    input.iter().all(|row| row[j] >= value)
}

pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<(usize, usize)> {
    let mut res = vec![];
    for (i, row) in input.iter().enumerate() {
        for j in find_max_indexes(row) {
            let point_is_the_only_min_in_column = is_min_in_column(input, i, j);
            if point_is_the_only_min_in_column {
                res.push((i, j));
            }
        }
    }
    res
}
