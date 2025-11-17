use itertools::iproduct;
use std::cmp::{max, min};

pub fn annotate(field: &[&str]) -> Vec<String> {
    let neighbours = |x, size| max(x, 1) - 1..=min(x + 1, size - 1);

    let mines_around = |i, j| {
        iproduct!(neighbours(i, field.len()), neighbours(j, field[0].len()))
            .filter(|&(i, j)| field[i].as_bytes()[j] == b'*')
            .count()
    };

    (0..field.len())
        .map(|i| {
            (0..field[0].len())
                .map(|j| {
                    let mines = mines_around(i, j);
                    (match field[i].as_bytes()[j] {
                        b' ' if mines > 0 => b'0' + mines as u8,
                        c => c,
                    }) as char
                })
                .collect()
        })
        .collect()
}
