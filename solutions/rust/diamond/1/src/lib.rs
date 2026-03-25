pub fn get_diamond(c: char) -> Vec<String> {
    let size = c as usize - b'A' as usize;
    (0..=size)
        .chain((0..size).rev())
        .map(|i| {
            let letter = (b'A' + i as u8) as char;
            " ".repeat(size - i)
                + &if i == 0 {
                    String::from(letter)
                } else {
                    String::from(letter)
                        + &" ".repeat(2 * i - 1)
                        + &String::from(letter)
                }
                + &" ".repeat(size - i)
        })
        .collect()
}
