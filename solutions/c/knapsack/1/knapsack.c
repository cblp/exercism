#include "knapsack.h"

#include <assert.h>

typedef uint16_t bitset_t;

static item_t sum_items(const item_t* const items, size_t const item_count,
                        bitset_t bitset) {
    item_t t = {0};
    for (unsigned i = 0; bitset && i < item_count; i++, bitset >>= 1) {
        if (bitset & 1) {
            t.value += items[i].value;
            t.weight += items[i].weight;
        }
    }
    return t;
}

unsigned maximum_value(unsigned const maximum_weight, const item_t* const items,
                       size_t const item_count) {
    assert(item_count <= 15);
    if (item_count == 0) {
        return 0;
    }
    assert(items);
    unsigned maxval = 0;
    for (bitset_t item_bits = 1; item_bits < (1 << item_count); item_bits++) {
        item_t total = sum_items(items, item_count, item_bits);
        if (total.weight <= maximum_weight && total.value > maxval) {
            maxval = total.value;
        }
    }
    return maxval;
}
