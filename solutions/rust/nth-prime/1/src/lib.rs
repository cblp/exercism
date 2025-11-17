struct Primes {
    cache: Vec<u32>,
}

fn primes() -> Primes {
    Primes { cache: vec![2] }
}

impl Primes {
    fn nth(mut self, n: u32) -> u32 {
        if (n as usize) < self.cache.len() {
            return self.cache[n as usize];
        }
        let last_known = self.cache.last().unwrap();
        for x in last_known + 1.. {
            if self
                .cache
                .iter()
                .take_while(|&p| p * p <= x)
                .all(|p| x % p != 0)
            {
                self.cache.push(x);
                if self.cache.len() == n as usize + 1 {
                    return x;
                }
            }
        }
        0
    }
}

pub fn nth(n: u32) -> u32 {
    primes().nth(n)
}
