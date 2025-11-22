package strand

func ToRNA(dna string) (rna string) {
	for _, nucleotide := range dna {
		switch nucleotide {
		case 'C':
			rna += "G"
		case 'G':
			rna += "C"
		case 'T':
			rna += "A"
		case 'A':
			rna += "U"
		}
	}
	return
}
