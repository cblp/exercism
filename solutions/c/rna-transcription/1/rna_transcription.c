#include "rna_transcription.h"

#include <string.h>

char* to_rna(const char* dna) {
    char* rna = strdup(dna);
    for (char* c = rna; *c; ++c) {
        *c = *c == 'A'   ? 'U'
             : *c == 'C' ? 'G'
             : *c == 'G' ? 'C'
             : *c == 'T' ? 'A'
                         : *c;
    }
    return rna;
}
