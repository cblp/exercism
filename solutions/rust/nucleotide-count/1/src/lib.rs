use std::collections::HashMap;

fn is_nucleotide(x: char) -> bool {
    matches!(x, 'A' | 'C' | 'T' | 'G')
}

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    if !is_nucleotide(nucleotide) {
        return Err(nucleotide);
    }
    let mut cnt = 0;
    for x in dna.chars() {
        if !is_nucleotide(x) {
            return Err(x);
        }
        if x == nucleotide {
            cnt += 1;
        }
    }
    Ok(cnt)
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    let mut cnt = HashMap::from([('A', 0), ('C', 0), ('T', 0), ('G', 0)]);
    for x in dna.chars() {
        if let Some(c) = cnt.get_mut(&x) {
            *c += 1;
        } else {
            return Err(x);
        }
    }
    Ok(cnt)
}
