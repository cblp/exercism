fn is_dna_nucleotide(n: char) -> bool {
    matches!(n, 'A' | 'C' | 'T' | 'G')
}

fn is_rna_nucleotide(n: char) -> bool {
    matches!(n, 'A' | 'C' | 'U' | 'G')
}

#[derive(Debug, PartialEq, Eq)]
pub struct Dna(String);

#[derive(Debug, PartialEq, Eq)]
pub struct Rna(String);

impl Dna {
    pub fn new(dna: &str) -> Result<Dna, usize> {
        if let Some(n) = dna.find(|c| !is_dna_nucleotide(c)) {
            Err(n)
        } else {
            Ok(Dna(dna.to_string()))
        }
    }

    pub fn into_rna(self) -> Rna {
        Rna(self
            .0
            .chars()
            .map(|n| match n {
                'G' => 'C',
                'C' => 'G',
                'T' => 'A',
                'A' => 'U',
                _ => unimplemented!("impossible"),
            })
            .collect())
    }
}

impl Rna {
    pub fn new(rna: &str) -> Result<Rna, usize> {
        if let Some(n) = rna.find(|c| !is_rna_nucleotide(c)) {
            Err(n)
        } else {
            Ok(Rna(rna.to_string()))
        }
    }
}
