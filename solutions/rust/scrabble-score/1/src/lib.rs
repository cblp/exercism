/// Compute the Scrabble score for a word.
pub fn score(word: &str) -> u64 {
    word.bytes()
        .map(|c| match c.to_ascii_uppercase() {
            b'A' | b'E' | b'I' | b'O' | b'U' | b'L' | b'N' | b'R' | b'S'
            | b'T' => 1,
            b'D' | b'G' => 2,
            b'B' | b'C' | b'M' | b'P' => 3,
            b'F' | b'H' | b'V' | b'W' | b'Y' => 4,
            b'K' => 5,
            b'J' | b'X' => 8,
            b'Q' | b'Z' => 10,
            _ => 0,
        })
        .sum()
}
