use std::{cmp::Ordering, collections::HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Palindrome {
    value: u64,
    factors: HashSet<(u64, u64)>,
}

impl Palindrome {
    pub fn value(&self) -> u64 {
        self.value
    }

    pub fn into_factors(self) -> HashSet<(u64, u64)> {
        self.factors
    }
}

fn is_palindrome(x: u64) -> bool {
    if x.is_multiple_of(10) {
        return false;
    }
    let s = format!("{}", x);
    let bs = s.as_bytes();
    for i in 0..(bs.len() / 2) {
        if bs[i] != bs[bs.len() - 1 - i] {
            return false;
        }
    }
    true
}

enum MinMax {
    Min,
    Max,
}
use MinMax::*;

impl Palindrome {
    fn merge(&mut self, m: MinMax, p: Self) {
        match (m, self.value.cmp(&p.value)) {
            (Min, Ordering::Less) | (Max, Ordering::Greater) => {}
            (Min, Ordering::Greater) | (Max, Ordering::Less) => *self = p,
            (_, Ordering::Equal) => self.factors.extend(p.factors),
        }
    }
}

fn update(
    r: Option<(Palindrome, Palindrome)>,
    p: Palindrome,
) -> (Palindrome, Palindrome) {
    if let Some(mut r) = r {
        r.0.merge(Min, p.clone());
        r.1.merge(Max, p);
        r
    } else {
        (p.clone(), p)
    }
}

pub fn palindrome_products(
    min: u64,
    max: u64,
) -> Option<(Palindrome, Palindrome)> {
    let mut r = None;
    for a in min..=max {
        for b in a..=max {
            let value = a * b;
            if !is_palindrome(value) {
                continue;
            }
            r = Some(update(
                r,
                Palindrome {
                    value,
                    factors: HashSet::from([(a, b)]),
                },
            ));
        }
    }
    r
}
