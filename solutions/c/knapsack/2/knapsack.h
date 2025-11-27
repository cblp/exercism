#ifndef KNAPSACK_H
#define KNAPSACK_H

#include <stdlib.h>

typedef struct {
    unsigned int weight;
    unsigned int value;
} item_t;

unsigned maximum_value(unsigned maximum_weight, const item_t* items,
                       size_t item_count);

#endif
