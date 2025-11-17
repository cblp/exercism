pub struct PascalsTriangle(Vec<Vec<u32>>);

fn next_row(prev: Vec<u32>) -> Vec<u32> {
    let mut row = vec![1];
    for i in 1..prev.len() {
        row.push(prev[i - 1] + prev[i]);
    }
    row.push(1);
    row
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        let mut row = vec![1];
        let mut rows = vec![];
        for _ in 0..row_count {
            rows.push(row.clone());
            row = next_row(row);
        }
        Self(rows)
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        self.0.clone()
    }
}
