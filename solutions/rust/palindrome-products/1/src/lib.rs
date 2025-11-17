use std::collections::HashSet;

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

struct PalindromeVar(Option<Palindrome>, MinMax);

impl PalindromeVar {
    fn new(m: MinMax) -> Self {
        Self(None, m)
    }

    fn update(&mut self, value: u64, a: u64, b: u64) {
        let reset = Some(Palindrome {
            value,
            factors: HashSet::from([(a, b)]),
        });
        if let Some(p) = &mut self.0 {
            let reset_condition = match self.1 {
                Min => value < p.value,
                Max => value > p.value,
            };
            if reset_condition {
                self.0 = reset;
            } else if value == p.value {
                p.factors.insert((a, b));
            }
        } else {
            self.0 = reset;
        }
    }
}

pub fn palindrome_products(
    min: u64,
    max: u64,
) -> Option<(Palindrome, Palindrome)> {
    let mut min_palindrome = PalindromeVar::new(Min);
    let mut max_palindrome = PalindromeVar::new(Max);
    for a in min..=max {
        for b in a..=max {
            let p = a * b;
            if !is_palindrome(p) {
                continue;
            }
            min_palindrome.update(p, a, b);
            max_palindrome.update(p, a, b);
        }
    }
    min_palindrome.0.zip(max_palindrome.0)
}
