#include "sum_of_multiples.h"

#include <stdbool.h>

unsigned sum(const unsigned* const factors, size_t const number_of_factors,
             unsigned const limit) {
    bool flags[limit];
    for (size_t i = 0; i < limit; ++i) {
        flags[i] = false;
    }

    for (size_t fi = 0; fi < number_of_factors; ++fi) {
        if (factors[fi] < 1) continue;
        for (unsigned multiple = factors[fi]; multiple < limit;
             multiple += factors[fi]) {
            flags[multiple] = true;
        }
    }

    unsigned s = 0;
    for (size_t multiple = 1; multiple < limit; ++multiple) {
        if (flags[multiple]) {
            s += multiple;
        }
    }
    return s;
}
