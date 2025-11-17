pub struct Triangle([u64; 3]);

impl Triangle {
    pub fn build(mut sides: [u64; 3]) -> Option<Triangle> {
        sides.sort();
        let [a, b, c] = sides;
        (a > 0 && b > 0 && c > 0 && a + b >= c).then_some(Triangle(sides))
    }

    pub fn is_equilateral(&self) -> bool {
        let Triangle([a, b, c]) = self;
        a == b && b == c
    }

    pub fn is_scalene(&self) -> bool {
        !self.is_isosceles()
    }

    pub fn is_isosceles(&self) -> bool {
        let Triangle([a, b, c]) = self;
        a == b || b == c
    }
}
