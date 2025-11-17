pub fn plants(diagram: &'static str, student: &str) -> Vec<&'static str> {
    let index = (student.as_bytes()[0] - b'A') as usize;
    let offset = index * 2;
    diagram
        .split('\n')
        .flat_map(|row| {
            row[offset..offset + 2].chars().map(|c| match c {
                'C' => "clover",
                'G' => "grass",
                'R' => "radishes",
                'V' => "violets",
                _ => unreachable!(),
            })
        })
        .collect()
}
