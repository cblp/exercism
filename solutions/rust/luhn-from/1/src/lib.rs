use std::iter;

pub struct Luhn(Vec<u8>);

// 0   0
// 1   2
// 2   4
// 3   6
// 4   8
// 5   1
// 6   3
// 7   5
// 8   7
// 9   9

impl Luhn {
    pub fn is_valid(&self) -> bool {
        self.0.len() >= 2
            && self
                .0
                .iter()
                .enumerate()
                .map(|(i, d)| {
                    (if !i.is_multiple_of(2) {
                        2 * d - if *d > 4 { 9 } else { 0 }
                    } else {
                        *d
                    }) as u64
                })
                .sum::<u64>()
                % 10
                == 0
    }
}

/// Here is the example of how the From trait could be implemented
/// for the &str type. Naturally, you can implement this trait
/// by hand for every other type presented in the test suite,
/// but your solution will fail if a new type is presented.
/// Perhaps there exists a better solution for this problem?
impl From<&str> for Luhn {
    fn from(input: &str) -> Self {
        Self(if input.bytes().all(|c| c.is_ascii_digit() || c == b' ') {
            input
                .bytes()
                .rev()
                .filter(u8::is_ascii_digit)
                .map(|c| c - b'0')
                .collect()
        } else {
            vec![]
        })
    }
}

impl From<String> for Luhn {
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}

impl From<usize> for Luhn {
    fn from(mut value: usize) -> Self {
        Self(
            iter::from_fn(|| match value {
                0 => None,
                _ => {
                    let d = (value % 10) as u8;
                    value /= 10;
                    Some(d)
                }
            })
            .collect(),
        )
    }
}

impl From<u64> for Luhn {
    fn from(value: u64) -> Self {
        Self::from(value as usize)
    }
}

impl From<u32> for Luhn {
    fn from(value: u32) -> Self {
        Self::from(value as usize)
    }
}

impl From<u16> for Luhn {
    fn from(value: u16) -> Self {
        Self::from(value as usize)
    }
}

impl From<u8> for Luhn {
    fn from(value: u8) -> Self {
        Self::from(value as usize)
    }
}
