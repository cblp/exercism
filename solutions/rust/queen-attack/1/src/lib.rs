#[derive(Debug)]
pub struct ChessPosition {
    x: i32,
    y: i32,
}

#[derive(Debug)]
pub struct Queen(ChessPosition);

impl ChessPosition {
    pub fn new(x: i32, y: i32) -> Option<Self> {
        ((0..8).contains(&x) && (0..8).contains(&y))
            .then_some(ChessPosition { x, y })
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Queen(position)
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        let a = &self.0;
        let b = &other.0;

        a.x == b.x
            || a.y == b.y
            || a.x + a.y == b.x + b.y
            || a.x - a.y == b.x - b.y
    }
}
