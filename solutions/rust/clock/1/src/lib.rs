use num::integer::div_rem;
use std::fmt::{Display, Error, Formatter};
use std::ops::Rem;

#[derive(Debug, PartialEq)]
pub struct Clock {
    minutes: i32,
}

impl Display for Clock {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let (hours, m) = div_rem(self.minutes, 60);
        let h = hours.rem_euclid(24);
        write!(f, "{h:02}:{m:02}")
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self {
            minutes: (hours * 60 + minutes).rem_euclid(24 * 60),
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Self {
            minutes: (self.minutes + minutes).rem_euclid(24 * 60),
        }
    }
}
