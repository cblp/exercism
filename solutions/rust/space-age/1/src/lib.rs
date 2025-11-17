// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(Debug)]
pub struct Duration {
    seconds: f64,
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Duration { seconds: s as f64 }
    }
}

pub trait Planet {
    const YEAR_IN_SECONDS: f64;
    fn years_during(d: &Duration) -> f64 {
        d.seconds / Self::YEAR_IN_SECONDS
    }
}

pub struct Mercury;
pub struct Venus;
pub struct Earth;
pub struct Mars;
pub struct Jupiter;
pub struct Saturn;
pub struct Uranus;
pub struct Neptune;

impl Planet for Mercury {
    const YEAR_IN_SECONDS: f64 = 0.2408467 * Earth::YEAR_IN_SECONDS;
}
impl Planet for Venus {
    const YEAR_IN_SECONDS: f64 = 0.61519726 * Earth::YEAR_IN_SECONDS;
}
impl Planet for Earth {
    const YEAR_IN_SECONDS: f64 = 31_557_600.0;
}
impl Planet for Mars {
    const YEAR_IN_SECONDS: f64 = 1.8808158 * Earth::YEAR_IN_SECONDS;
}
impl Planet for Jupiter {
    const YEAR_IN_SECONDS: f64 = 11.862615 * Earth::YEAR_IN_SECONDS;
}
impl Planet for Saturn {
    const YEAR_IN_SECONDS: f64 = 29.447498 * Earth::YEAR_IN_SECONDS;
}
impl Planet for Uranus {
    const YEAR_IN_SECONDS: f64 = 84.016846 * Earth::YEAR_IN_SECONDS;
}
impl Planet for Neptune {
    const YEAR_IN_SECONDS: f64 = 164.79132 * Earth::YEAR_IN_SECONDS;
}
