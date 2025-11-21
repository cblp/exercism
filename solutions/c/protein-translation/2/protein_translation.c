#include "protein_translation.h"

#include <string.h>

typedef struct {
    char nucleotides[3];
    amino_acid_t value;
} codon_t;

codon_t const CODONS[] = {
    {"AUG", Methionine}, {"UUU", Phenylalanine}, {"UUC", Phenylalanine},
    {"UUA", Leucine},    {"UUG", Leucine},       {"UCU", Serine},
    {"UCC", Serine},     {"UCA", Serine},        {"UCG", Serine},
    {"UAU", Tyrosine},   {"UAC", Tyrosine},      {"UGU", Cysteine},
    {"UGC", Cysteine},   {"UGG", Tryptophan},    {"UAA", STOP},
    {"UAG", STOP},       {"UGA", STOP},
};

size_t const CODONS_COUNT = sizeof(CODONS) / sizeof(codon_t);

protein_t protein(const char* const rna) {
    protein_t r = {0};
    for (size_t i = 0; rna[i]; i += 3) {
        if (!rna[i + 1] || !rna[i + 2]) {
            return r;
        }
        bool codon_is_found = false;
        for (size_t c = 0; c < CODONS_COUNT; ++c) {
            if (strncmp(rna + i, CODONS[c].nucleotides, 3) == 0) {
                if (CODONS[c].value == STOP) {
                    goto stop;
                }
                r.amino_acids[r.count++] = CODONS[c].value;
                codon_is_found = true;
                break;
            }
        }
        if (!codon_is_found) {
            return r;
        }
    }
stop:
    r.valid = true;
    return r;
}
