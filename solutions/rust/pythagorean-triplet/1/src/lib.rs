use std::{cmp::min, collections::HashSet};

pub fn find(sum: u32) -> HashSet<[u32; 3]> {
    (1..sum / 3)
        .flat_map(|a| {
            (a + 1..min(sum - a, 2 * sum / 3)).filter_map(move |b| {
                let c = sum - a - b;
                (b < c && a * a + b * b == c * c).then_some([a, b, c])
            })
        })
        .collect()
}
