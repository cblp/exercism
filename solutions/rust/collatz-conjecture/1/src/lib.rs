pub fn collatz(mut n: u64) -> Option<u64> {
    let mut c = 0;
    loop {
        match n {
            0 => break None,
            1 => break Some(c),
            _ if n % 2 == 0 => {
                n /= 2;
                c += 1;
            }
            // _ if n >= 6_148_914_691_236_517_205 => break None,
            _ => {
                n = n * 3 + 1;
                c += 1;
            }
        }
    }
}
