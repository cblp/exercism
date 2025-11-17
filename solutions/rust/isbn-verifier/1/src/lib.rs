fn guard(b: bool) -> Option<()> {
    b.then_some(())
}

fn replicate<A>(
    n: usize,
    mut action: impl FnMut() -> Option<A>,
) -> Option<Vec<A>> {
    let mut r = vec![];
    for _ in 0..n {
        match action() {
            None => return None,
            Some(x) => r.push(x),
        }
    }
    Some(r)
}

#[derive(Debug)]
struct Parser<'a> {
    input: &'a [u8],
}

impl Parser<'_> {
    fn parse(&mut self, p: impl Fn(u8) -> bool) -> Option<u8> {
        if self.input.is_empty() {
            return None;
        }
        let i0 = self.input[0];
        if !p(i0) {
            return None;
        }
        self.input = &self.input[1..];
        Some(i0)
    }

    fn digit(&mut self) -> Option<u8> {
        self.parse(|c| c.is_ascii_digit()).map(|c| c - b'0')
    }

    fn check_digit(&mut self) -> Option<u8> {
        self.parse(|c| c.is_ascii_digit() || c == b'X')
            .map(|c| match c {
                b'X' => 10,
                _ => c - b'0',
            })
    }

    fn optional_char(&mut self, liter: u8) {
        self.parse(|c| c == liter);
    }

    fn end(&self) -> Option<()> {
        guard(self.input.is_empty())
    }
}

fn parse_isbn(s: &str) -> Option<[u8; 10]> {
    let mut p = Parser {
        input: s.as_bytes(),
    };
    let r1 = p.digit()?;
    p.optional_char(b'-');
    let r2_4 = replicate(3, || p.digit())?;
    p.optional_char(b'-');
    let r5_9 = replicate(5, || p.digit())?;
    p.optional_char(b'-');
    let r10 = p.check_digit()?;
    p.end()?;
    [vec![r1], r2_4, r5_9, vec![r10]].concat().try_into().ok()
}

/// Determines whether the supplied string is a valid ISBN number
pub fn is_valid_isbn(s: &str) -> bool {
    parse_isbn(s).is_some_and(|isbn| {
        isbn.into_iter()
            .zip((1..=10).rev())
            .map(|(d, x)| d as u16 * x)
            .sum::<u16>()
            % 11
            == 0
    })
}
