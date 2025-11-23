#include "etl.h"

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int (*comparator_t)(const void*, const void*);

int convert(const legacy_map* input, const size_t input_len, new_map** output) {
    if (input == NULL || output == NULL || input_len == 0) {
        return -1;  // Indicate error for invalid input
    }

    size_t keys_count = 0;
    int keys[26] = {0};  // Assuming only lowercase a-z
    for (size_t i = 0; i < input_len; ++i) {
        const char* key_input = input[i].keys;
        while (*key_input) {
            if (isalpha(*key_input)) {
                char const key = tolower(*key_input);
                int index = key - 'a';
                if (keys[index] == 0) {
                    keys[index] = input[i].value;
                    keys_count++;
                }
            }
            key_input++;
        }
    }

    *output = malloc(keys_count * sizeof(new_map));
    for (size_t i = 0, j = 0; i < 26; ++i) {
        if (keys[i] != 0) {
            (*output)[j].key = 'a' + i;
            (*output)[j].value = keys[i];
            j++;
        }
    }

    return keys_count;
}
