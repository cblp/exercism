use std::{
    cmp::Reverse,
    collections::{BTreeSet, HashMap},
};

pub fn lowest_price(books: &[u32]) -> u32 {
    Total::new().memoized(&normalize(count(books.iter().copied())))
}

fn count(books: impl IntoIterator<Item = u32>) -> Vec<u32> {
    let mut counter: HashMap<u32, u32> = HashMap::new();
    for b in books {
        *counter.entry(b).or_default() += 1
    }
    counter.values().copied().collect()
}

fn normalize(basket: impl IntoIterator<Item = u32>) -> Vec<u32> {
    let mut basket: Vec<u32> = basket.into_iter().collect();
    basket.sort_by_key(|x| Reverse(*x));
    chop(basket)
}

const COST_1: u32 = 8_00;
const COST_2: u32 = 15_20;
const COST_3: u32 = 21_60;
const COST_4: u32 = 25_60;
const COST_5: u32 = 30_00;

struct Total {
    cache: HashMap<Vec<u32>, u32>,
}

impl Total {
    fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    fn memoized(&mut self, basket: &[u32]) -> u32 {
        match self.cache.get(basket) {
            Some(v) => *v,
            None => {
                let v = self.implementation(basket);
                self.cache.insert(basket.to_vec(), v);
                v
            }
        }
    }

    fn implementation(&mut self, basket: &[u32]) -> u32 {
        if basket.is_empty() {
            return 0;
        }
        if basket.len() == 1 {
            return COST_1 * basket[0];
        }

        let mut costs = vec![];

        for without_5 in take_some(5, basket) {
            costs.push(COST_5 + self.memoized(&without_5))
        }

        for without_4 in take_some(4, basket) {
            costs.push(COST_4 + self.memoized(&without_4))
        }

        for without_3 in take_some(3, basket) {
            costs.push(COST_3 + self.memoized(&without_3))
        }

        for without_2 in take_some(2, basket) {
            costs.push(COST_2 + self.memoized(&without_2))
        }

        assert!(!costs.is_empty());
        costs.into_iter().min().unwrap()
    }
}

fn take_some(n: usize, basket: &[u32]) -> Vec<Vec<u32>> {
    if n > basket.len() {
        return vec![];
    }

    if n == basket.len() {
        assert!(basket.iter().all(|x| *x >= 1), "{basket:?}");
        return vec![chop(basket.iter().map(|x| x - 1))];
    }

    // n < len(basket)
    let basket_variants: Vec<Vec<u32>> = match (n, basket) {
        (2, [b1, b2, b3]) => vec![
            vec![b1 - 1, b2 - 1, *b3],
            vec![b1 - 1, *b2, b3 - 1],
            vec![*b1, b2 - 1, b3 - 1],
        ],
        (2, [b1, b2, b3, b4]) => vec![
            vec![b1 - 1, b2 - 1, *b3, *b4],
            vec![b1 - 1, *b2, b3 - 1, *b4],
            vec![b1 - 1, *b2, *b3, b4 - 1],
            vec![*b1, b2 - 1, b3 - 1, *b4],
            vec![*b1, b2 - 1, *b3, b4 - 1],
            vec![*b1, *b2, b3 - 1, b4 - 1],
        ],
        (3, [b1, b2, b3, b4]) => vec![
            vec![b1 - 1, b2 - 1, b3 - 1, *b4],
            vec![b1 - 1, b2 - 1, *b3, b4 - 1],
            vec![b1 - 1, *b2, b3 - 1, b4 - 1],
            vec![*b1, b2 - 1, b3 - 1, b4 - 1],
        ],
        (2, [b1, b2, b3, b4, b5]) => vec![
            vec![b1 - 1, b2 - 1, *b3, *b4, *b5],
            vec![b1 - 1, *b2, b3 - 1, *b4, *b5],
            vec![b1 - 1, *b2, *b3, b4 - 1, *b5],
            vec![b1 - 1, *b2, *b3, *b4, b5 - 1],
            vec![*b1, b2 - 1, b3 - 1, *b4, *b5],
            vec![*b1, b2 - 1, *b3, b4 - 1, *b5],
            vec![*b1, b2 - 1, *b3, *b4, b5 - 1],
            vec![*b1, *b2, b3 - 1, b4 - 1, *b5],
            vec![*b1, *b2, b3 - 1, *b4, b5 - 1],
            vec![*b1, *b2, *b3, b4 - 1, b5 - 1],
        ],
        (3, [b1, b2, b3, b4, b5]) => vec![
            vec![b1 - 1, b2 - 1, b3 - 1, *b4, *b5],
            vec![b1 - 1, b2 - 1, *b3, b4 - 1, *b5],
            vec![b1 - 1, b2 - 1, *b3, *b4, b5 - 1],
            vec![b1 - 1, *b2, b3 - 1, b4 - 1, *b5],
            vec![b1 - 1, *b2, b3 - 1, *b4, b5 - 1],
            vec![b1 - 1, *b2, *b3, b4 - 1, b5 - 1],
            vec![*b1, b2 - 1, b3 - 1, b4 - 1, *b5],
            vec![*b1, b2 - 1, b3 - 1, *b4, b5 - 1],
            vec![*b1, b2 - 1, *b3, b4 - 1, b5 - 1],
            vec![*b1, *b2, b3 - 1, b4 - 1, b5 - 1],
        ],
        (4, [b1, b2, b3, b4, b5]) => vec![
            vec![b1 - 1, b2 - 1, b3 - 1, b4 - 1, *b5],
            vec![b1 - 1, b2 - 1, b3 - 1, *b4, b5 - 1],
            vec![b1 - 1, b2 - 1, *b3, b4 - 1, b5 - 1],
            vec![b1 - 1, *b2, b3 - 1, b4 - 1, b5 - 1],
            vec![*b1, b2 - 1, b3 - 1, b4 - 1, b5 - 1],
        ],
        _ => todo!("{n}, {basket:?}"),
    };

    Vec::from_iter(BTreeSet::from_iter(
        basket_variants
            .iter()
            .map(|bv| normalize(bv.iter().copied())),
    ))
    // return list(set(normalize(bv) for bv in basket_variants))
}

fn chop(basket: impl IntoIterator<Item = u32>) -> Vec<u32> {
    basket.into_iter().filter(|x| *x != 0).collect()
}
