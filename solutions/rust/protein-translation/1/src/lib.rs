enum ProteinResult {
    Protein(&'static str),
    Stop,
    Unknown,
}
use ProteinResult::*;

fn protein(codon: &str) -> ProteinResult {
    match codon {
        "AUG" => Protein("Methionine"),
        "UUU" | "UUC" => Protein("Phenylalanine"),
        "UUA" | "UUG" => Protein("Leucine"),
        "UCU" | "UCC" | "UCA" | "UCG" => Protein("Serine"),
        "UAU" | "UAC" => Protein("Tyrosine"),
        "UGU" | "UGC" => Protein("Cysteine"),
        "UGG" => Protein("Tryptophan"),
        "UAA" | "UAG" | "UGA" => Stop,
        _ => Unknown,
    }
}

pub fn translate(rna: &str) -> Option<Vec<&str>> {
    let mut r = vec![];
    for i in (0..rna.len()).step_by(3) {
        if i + 3 > rna.len() {
            return None;
        }
        let codon = &rna[i..i + 3];
        match protein(codon) {
            Protein(p) => r.push(p),
            Stop => break,
            Unknown => return None,
        }
    }
    Some(r)
}
