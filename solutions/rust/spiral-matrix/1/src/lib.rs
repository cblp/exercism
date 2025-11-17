fn at<A>(matrix: &[Vec<A>], pos: (usize, usize)) -> &A {
    &matrix[pos.0][pos.1]
}

fn at_put<A>(matrix: &mut [Vec<A>], pos: (usize, usize), value: A) {
    matrix[pos.0][pos.1] = value
}

fn turn_right(dir: u8) -> u8 {
    (dir + 1) % 4
}

fn forward(
    dir: u8,
    pos: (usize, usize),
    size: usize,
) -> Option<(usize, usize)> {
    match dir {
        0 if pos.1 + 1 < size => Some((pos.0, pos.1 + 1)),
        1 if pos.0 + 1 < size => Some((pos.0 + 1, pos.1)),
        2 if pos.1 > 0 => Some((pos.0, pos.1 - 1)),
        3 if pos.0 > 0 => Some((pos.0 - 1, pos.1)),
        _ => None,
    }
}

pub fn spiral_matrix(size: u32) -> Vec<Vec<u32>> {
    if size == 0 {
        return vec![];
    }
    if size == 1 {
        return vec![vec![1]];
    }
    let size = size as usize;
    let mut r = vec![vec![0; size]; size];
    let mut pos = (0, 0);
    let mut dir = 0;
    for x in 1..=size * size {
        at_put(&mut r, pos, x as u32);
        match forward(dir, pos, size) {
            Some(next_pos) if *at(&r, next_pos) == 0 => {
                pos = next_pos;
            }
            _ => {
                dir = turn_right(dir);
                pos = forward(dir, pos, size).unwrap();
            }
        }
    }
    r
}
