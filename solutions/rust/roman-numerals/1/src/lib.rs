use std::fmt::{Display, Formatter, Result};

pub struct Roman(String);

impl Display for Roman {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.fmt(f)
    }
}

const ROMAN_DIGITS: [(u32, &str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

impl From<u32> for Roman {
    fn from(mut num: u32) -> Self {
        let mut r = String::new();
        for (value, symbol) in ROMAN_DIGITS {
            while num >= value {
                r += symbol;
                num -= value;
            }
        }
        Roman(r)
    }
}
