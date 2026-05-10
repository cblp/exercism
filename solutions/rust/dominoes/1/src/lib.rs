pub fn chain(input: &[(u8, u8)]) -> Option<Vec<(u8, u8)>> {
    match input {
        [] => Some(vec![]),
        [(head, start_b), dominoes @ ..] => {
            go(vec![(*start_b, *head)], dominoes)
        }
    }
}

fn go(chained: Vec<(u8, u8)>, free: &[(u8, u8)]) -> Option<Vec<(u8, u8)>> {
    let last = chained.last()?.1;
    match free {
        [] => (chained.first()?.0 == chained.last()?.1).then_some(chained),
        free => free.iter().find_map(|&(a, b)| {
            let tile = if a == last {
                (a, b)
            } else if b == last {
                (b, a)
            } else {
                return None;
            };
            let mut free = free.to_vec();
            if let Some(p) = free.iter().position(|&d| d == (a, b)) {
                free.swap_remove(p);
            }
            let mut chained = chained.clone();
            chained.push(tile);
            go(chained, &free)
        }),
    }
}
