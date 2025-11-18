use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug)]
pub struct Robot {
    name: String,
}

fn digit(n: u64) -> char {
    (b'0' + n as u8) as char
}

fn letter(n: u64) -> char {
    (b'A' + n as u8) as char
}

static COUNTER: AtomicU64 = const { AtomicU64::new(0) };

fn new_name() -> String {
    let counter = COUNTER.fetch_add(1, Ordering::SeqCst);
    [
        'A',
        letter(counter % 26),
        digit((counter / 10 / 10) % 10),
        digit((counter / 10) % 10),
        digit(counter % 10),
    ]
    .iter()
    .collect()
}

impl Robot {
    pub fn new() -> Self {
        Self { name: new_name() }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn reset_name(&mut self) {
        self.name = new_name();
    }
}
