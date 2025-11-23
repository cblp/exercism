#include "acronym.h"

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#define MAX_ACRONYM_LENGTH 100

char* abbreviate(const char* phrase) {
    if (!phrase || !*phrase) {
        return NULL;
    }

    bool in_word = false;
    char* result = malloc(MAX_ACRONYM_LENGTH);
    size_t result_len = 0;
    for (const char* p = phrase; *p; ++p) {
        if (isalpha(*p) || *p == '\'') {
            if (!in_word) {
                assert(result_len < MAX_ACRONYM_LENGTH - 1);
                result[result_len++] = toupper(*p);
                in_word = true;
            }
        } else {
            in_word = false;
        }
    }
    result[result_len] = 0;
    return result;
}
