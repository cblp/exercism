def to_rna(dna_strand: str) -> str:
    return "".join(
        {"A": "U", "C": "G", "G": "C", "T": "A"}[nucleotide]
        for nucleotide in dna_strand
    )
