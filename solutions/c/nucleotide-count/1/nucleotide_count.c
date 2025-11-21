#include "nucleotide_count.h"

#include <stdio.h>
#include <stdlib.h>

char* count(const char* dna_strand) {
    size_t const OUT_SIZE = 20;
    char* out = malloc(OUT_SIZE);
    out[0] = 0;

    size_t A = 0, C = 0, G = 0, T = 0;
    for (const char* nuc = dna_strand; *nuc; ++nuc) {
        switch (*nuc) {
            case 'A':
                ++A;
                break;

            case 'C':
                ++C;
                break;

            case 'G':
                ++G;
                break;

            case 'T':
                ++T;
                break;

            default:
                return out;
        }
    }

    snprintf(out, OUT_SIZE, "A:%zu C:%zu G:%zu T:%zu", A, C, G, T);
    return out;
}
