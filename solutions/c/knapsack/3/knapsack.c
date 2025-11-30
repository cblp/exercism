#include "knapsack.h"

#include <assert.h>

#define MAX_DICT_SIZE 1000

typedef struct dict_t {
    item_t items[MAX_DICT_SIZE];
    size_t size;
} dict_t;

static const item_t* cbegin(const dict_t* const dict) { return dict->items; }

static const item_t* cend(const dict_t* const dict) {
    return dict->items + dict->size;
}

static void insert_or_update(dict_t* const dict, unsigned const new_weight,
                             unsigned const new_value) {
    for (item_t* it = dict->items; it < dict->items + dict->size; it++) {
        if (it->weight == new_weight) {
            it->value = new_value;
            return;
        }
    }
    assert(dict->size < MAX_DICT_SIZE);
    dict->items[dict->size] =
        (item_t){.weight = new_weight, .value = new_value};
    dict->size++;
}

static void insert_or_update_if_gt(dict_t* const dict,
                                   unsigned const new_weight,
                                   unsigned const new_value) {
    for (item_t* it = dict->items; it < dict->items + dict->size; it++) {
        if (it->weight == new_weight) {
            if (new_value > it->value) {
                it->value = new_value;
            }
            return;
        }
    }
    assert(dict->size < MAX_DICT_SIZE);
    dict->items[dict->size] =
        (item_t){.weight = new_weight, .value = new_value};
    dict->size++;
}

unsigned maximum_value(unsigned const maximum_weight, const item_t* const items,
                       size_t const item_count) {
    if (item_count == 0) {
        return 0;
    }

    dict_t best = {0};
    insert_or_update(&best, 0, 0);
    unsigned maximum_value = 0;
    for (const item_t* item = items; item < items + item_count; item++) {
        dict_t const best_before_this_item = best;
        for (const item_t* best_iter = cbegin(&best_before_this_item);
             best_iter < cend(&best_before_this_item); best_iter++) {
            unsigned const new_weight = best_iter->weight + item->weight;
            if (new_weight > maximum_weight) {
                continue;
            }
            unsigned const new_value = best_iter->value + item->value;
            insert_or_update_if_gt(&best, new_weight, new_value);
            if (new_value > maximum_value) {
                maximum_value = new_value;
            }
        }
    }

    return maximum_value;
}
