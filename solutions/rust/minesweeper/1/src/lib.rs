use std::cmp::{max, min};

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    if minefield.is_empty() {
        return vec![];
    }

    let height = minefield.len();
    let width = minefield[0].len();

    let mines_at = |i: usize, j| (minefield[i].as_bytes()[j] == b'*') as u8;

    fn neighbours(x: usize, size: usize) -> impl Iterator<Item = usize> {
        max(x, 1) - 1..=min(x + 1, size - 1)
    }

    let neighbours2 = |i, j| {
        neighbours(i, height)
            .flat_map(move |i_| neighbours(j, width).map(move |j_| (i_, j_)))
    };

    let mines_around = |i, j| -> u8 {
        neighbours2(i, j).map(|(i_, j_)| mines_at(i_, j_)).sum()
    };

    minefield
        .iter()
        .copied()
        .enumerate()
        .map(|(i, line)| {
            line.chars()
                .enumerate()
                .map(|(j, c)| {
                    if c == ' ' {
                        let mines = mines_around(i, j);
                        if mines > 0 {
                            (b'0' + mines) as char
                        } else {
                            c
                        }
                    } else {
                        c
                    }
                })
                .collect()
        })
        .collect()
}
