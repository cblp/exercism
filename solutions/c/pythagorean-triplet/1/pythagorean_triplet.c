#include "pythagorean_triplet.h"

#include <stdlib.h>

triplets_t* triplets_with_sum(uint16_t sum) {
    (void)sum;
    triplets_t* triplets = calloc(1, sizeof(triplets_t));
    for (uint16_t a = 1; a < sum / 3; ++a) {
        for (uint16_t b = a + 1; b < sum - a && b < 2 * sum / 3; ++b) {
            uint16_t const c = sum - a - b;
            if (c > b && a * a + b * b == c * c) {
                triplets->triplets[triplets->count++] = (triplet_t){a, b, c};
            }
        }
    }
    return triplets;
}

void free_triplets(triplets_t* t) { free(t); }
