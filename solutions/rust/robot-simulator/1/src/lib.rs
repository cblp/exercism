// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(PartialEq, Eq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

use Direction::*;

pub struct Robot {
    x: i32,
    y: i32,
    d: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Self { x, y, d }
    }

    #[must_use]
    pub fn turn_right(self) -> Self {
        Self {
            d: match self.d {
                North => East,
                East => South,
                South => West,
                West => North,
            },
            ..self
        }
    }

    #[must_use]
    pub fn turn_left(self) -> Self {
        Self {
            d: match self.d {
                North => West,
                West => South,
                South => East,
                East => North,
            },
            ..self
        }
    }

    #[must_use]
    pub fn advance(self) -> Self {
        let (x, y) = match self.d {
            North => (self.x, self.y + 1),
            South => (self.x, self.y - 1),
            East => (self.x + 1, self.y),
            West => (self.x - 1, self.y),
        };
        Self { x, y, ..self }
    }

    #[must_use]
    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |r, i| match i {
            'L' => r.turn_left(),
            'R' => r.turn_right(),
            'A' => r.advance(),
            _ => todo!("instruction={i}"),
        })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
