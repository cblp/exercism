use {
    num_bigint::BigInt,
    num_integer::Integer,
    std::{
        cmp::Ordering,
        ops::{Add, Mul, Neg, Sub},
    },
};

/// Type implementing arbitrary-precision decimal arithmetic
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Decimal {
    significand: BigInt,
    exponent: i64,
}

impl Decimal {
    pub fn new(mut significand: BigInt, mut exponent: i64) -> Self {
        // for 0, exponent must be 0
        if significand == BigInt::ZERO {
            return Decimal {
                significand,
                exponent: 0,
            };
        }
        // normalize
        while significand.is_multiple_of(&BigInt::from(10)) {
            significand /= 10;
            exponent += 1;
        }
        Self {
            significand,
            exponent,
        }
    }

    pub fn try_from(input: &str) -> Option<Decimal> {
        let mut significand = BigInt::ZERO;
        let mut exponent = 0;
        let mut in_fractional_part = false;
        let mut is_negative = false;
        for c in input.bytes() {
            match c {
                _ if c.is_ascii_digit() => {
                    significand = significand * 10 + BigInt::from(c - b'0');
                    if in_fractional_part {
                        exponent -= 1;
                    }
                }
                b'.' => {
                    if in_fractional_part {
                        return None;
                    } else {
                        in_fractional_part = true;
                    }
                }
                b'-' => is_negative = true,
                b'+' => (),
                _ => todo!("{:?}", c as char),
            }
        }
        Some(Self::new(
            if is_negative {
                -significand
            } else {
                significand
            },
            exponent,
        ))
    }
}

impl Neg for Decimal {
    type Output = Decimal;
    fn neg(self) -> Self::Output {
        Self {
            significand: -self.significand,
            exponent: self.exponent,
        }
    }
}

impl Add for Decimal {
    type Output = Decimal;
    fn add(self, rhs: Self) -> Self::Output {
        let (self_sig, rhs_sig, exponent) = unify(self.clone(), rhs.clone());
        Self::new(self_sig + rhs_sig, exponent)
    }
}

impl Sub for Decimal {
    type Output = Decimal;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::add(self, -rhs)
    }
}

impl Mul for Decimal {
    type Output = Decimal;
    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(
            self.significand * rhs.significand,
            self.exponent + rhs.exponent,
        )
    }
}

impl PartialOrd for Decimal {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Decimal {
    fn cmp(&self, other: &Self) -> Ordering {
        let (self_sig, other_sig, _) = unify(self.clone(), other.clone());
        self_sig.cmp(&other_sig)
    }
}

/// denormalized unified significands and common exponent
fn unify(a: Decimal, b: Decimal) -> (BigInt, BigInt, i64) {
    match a.exponent.cmp(&b.exponent) {
        Ordering::Equal => (a.significand, b.significand, a.exponent),
        Ordering::Less => (
            a.significand,
            b.significand
                * BigInt::from(10)
                    .pow((b.exponent - a.exponent).try_into().unwrap()),
            a.exponent,
        ),

        Ordering::Greater => (
            a.significand
                * BigInt::from(10)
                    .pow((a.exponent - b.exponent).try_into().unwrap()),
            b.significand,
            b.exponent,
        ),
    }
}
