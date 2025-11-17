#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

use Error::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Bonus {
    NoBonus,
    Spare,
    Strike,
}

fn pretty_bonus(x: Bonus) -> char {
    match x {
        NoBonus => ' ',
        Spare => '/',
        Strike => 'X',
    }
}

use Bonus::*;

pub struct BowlingGame {
    frames: u8,

    /// Throws made in the current frame
    throws: u8,

    pins_up: u16,

    score: u16,

    last_bonuses: (Bonus, Bonus),

    ended: bool,
}

fn bool_to_int(b: bool) -> u16 {
    if b {
        1
    } else {
        0
    }
}

impl BowlingGame {
    pub fn new() -> Self {
        BowlingGame {
            frames: 0,
            throws: 0,
            pins_up: 10,
            score: 0,
            last_bonuses: (NoBonus, NoBonus),
            ended: false,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if self.ended {
            println!("GameComplete");
            return Err(GameComplete);
        }

        let frame = self.frames + 1;
        let throw = self.throws + 1;

        if pins > self.pins_up {
            return Err(NotEnoughPinsLeft);
        }
        self.pins_up -= pins;

        self.score += {
            pins * (bool_to_int(frame <= 10)
                + bool_to_int(matches!(self.last_bonuses, (_, Spare | Strike)))
                + bool_to_int(self.last_bonuses.0 == Strike))
        };

        // next bonus state
        let bonus = if self.frames >= 10 || self.pins_up != 0 {
            NoBonus
        } else if self.throws == 0 {
            Strike
        } else {
            Spare
        };

        println!(
            "Frame {:2}, Throw {}, Taken {:2}, Left {:2}, {} Score {:2}",
            frame,
            throw,
            pins,
            self.pins_up,
            pretty_bonus(bonus),
            self.score,
        );

        if (frame == 10 && throw == 2 && self.pins_up > 0)
            || (frame == 11
                && ((self.last_bonuses.0 == Strike && throw == 2)
                    || (self.last_bonuses.1 == Spare && throw == 1)))
            || frame == 12
        {
            self.ended = true;
        }

        // next frame if needed
        if self.pins_up == 0 || throw == 2 {
            self.frames += 1;
            self.throws = 0;
            self.pins_up = 10;
        } else {
            self.throws += 1;
        }
        self.last_bonuses = (self.last_bonuses.1, bonus);

        Ok(())
    }

    pub fn score(&self) -> Option<u16> {
        self.ended.then_some(self.score)
    }
}
