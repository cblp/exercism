#[derive(Debug, PartialEq, Eq)]
pub enum Classification {
    Abundant,
    Perfect,
    Deficient,
}

fn aliquot_sum(n: u64) -> u64 {
    let mut r = 0;
    for x in (2..).take_while(|x| x * x <= n) {
        if n.is_multiple_of(x) {
            r += x;
            if x * x < n {
                r += n / x;
            }
        }
    }
    r + 1
}

pub fn classify(num: u64) -> Option<Classification> {
    if num == 0 {
        return None;
    }
    if num == 1 {
        return Some(Deficient);
    }

    use std::cmp::Ordering::*;
    use Classification::*;
    Some(match aliquot_sum(num).cmp(&num) {
        Less => Deficient,
        Equal => Perfect,
        Greater => Abundant,
    })
}
