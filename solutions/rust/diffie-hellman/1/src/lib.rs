struct Mod(u64);

impl Mod {
    fn mul(&self, x: u64, y: u64) -> u64 {
        (x * y) % self.0
    }

    fn square(&self, x: u64) -> u64 {
        self.mul(x, x)
    }

    fn pow(&self, base: u64, power: u64) -> u64 {
        match power {
            0 => 1,
            1 => base,
            2 => self.square(base),
            _ => {
                let x = self.square(self.pow(base, power / 2));
                if power % 2 == 0 {
                    x
                } else {
                    self.mul(x, base)
                }
            }
        }
    }
}

pub fn private_key(p: u64) -> u64 {
    p / 2
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    Mod(p).pow(g, a)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    Mod(p).pow(b_pub, a)
}
