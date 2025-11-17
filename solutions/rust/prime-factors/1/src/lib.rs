pub fn factors(mut n: u64) -> Vec<u64> {
    let mut known_primes: Vec<u64> = vec![];
    let mut factors = vec![];
    while n != 1 {
        // find the next prime between 2 and √n
        let p = (2..)
            .take_while(|t| t * t <= n)
            .find(|t| known_primes.iter().all(|k| t % k != 0))
            .unwrap_or(n);
        known_primes.push(p);

        // divide n by p as much as possible
        while n % p == 0 {
            factors.push(p);
            n /= p;
        }
    }
    factors
}
