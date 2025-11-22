#include "sublist.h"

#include <assert.h>
#include <stdbool.h>
#include <string.h>

bool is_sublist(int* list_smaller, size_t list_smaller_len, int* list_bigger,
                size_t list_bigger_len);
bool is_sublist(int* list_smaller, size_t list_smaller_len, int* list_bigger,
                size_t list_bigger_len) {
    assert(list_bigger);
    assert(list_smaller_len <= list_bigger_len);

    if (!list_smaller || !list_smaller_len) return true;

    for (size_t i = 0; i < list_bigger_len - list_smaller_len + 1; ++i) {
        if (memcmp(list_bigger + i, list_smaller,
                   sizeof(int) * list_smaller_len) == 0) {
            return true;
        }
    }
    return false;
}

comparison_result_t check_lists(int* list_one, int* list_two,
                                size_t list_one_len, size_t list_two_len) {
    assert(list_one || !list_one_len);
    assert(list_two || !list_two_len);

    return list_one_len == list_two_len
               ? (memcmp(list_one, list_two, sizeof(int) * list_one_len) == 0
                      ? EQUAL
                      : UNEQUAL)
           : list_one_len < list_two_len
               ? (is_sublist(list_one, list_one_len, list_two, list_two_len)
                      ? SUBLIST
                      : UNEQUAL)
           : is_sublist(list_two, list_two_len, list_one, list_one_len)
               ? SUPERLIST
               : UNEQUAL;
}
