#ifndef PYTHAGOREAN_TRIPLET_H
#define PYTHAGOREAN_TRIPLET_H

#include <stdint.h>

#define MAX_TRIPLETS 10

typedef struct {
    uint16_t a, b, c;
} triplet_t;

typedef struct {
    uint16_t count;
    triplet_t triplets[MAX_TRIPLETS];
} triplets_t;

triplets_t* triplets_with_sum(uint16_t sum);

void free_triplets(triplets_t*);

#endif
