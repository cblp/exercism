#include "rna_transcription.h"

#include <stdlib.h>
#include <string.h>

char* to_rna(const char* dna) {
    size_t len = strlen(dna);
    char* rna = malloc(len + 1);
    for (size_t i = 0; i <= len; ++i) {
        char const n = dna[i];
        rna[i] = n == 'A'   ? 'U'
                 : n == 'C' ? 'G'
                 : n == 'G' ? 'C'
                 : n == 'T' ? 'A'
                            : n;
    }
    return rna;
}
