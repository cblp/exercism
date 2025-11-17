pub fn primes_up_to(upper_bound: u64) -> Vec<u64> {
    if upper_bound < 2 {
        return vec![];
    }
    if upper_bound == 2 {
        return vec![2];
    }
    let mut primes = vec![2, 3];
    let mut x = 5;
    while x <= upper_bound {
        if primes.iter().all(|&p| !x.is_multiple_of(p)) {
            primes.push(x);
        }
        x += 2;
    }
    primes
}
