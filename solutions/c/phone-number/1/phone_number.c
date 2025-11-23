#include "phone_number.h"

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

char* phone_number_clean(const char* input) {
    char* const out = malloc(11);
    memset(out, '0', 10);
    out[10] = '\0';

    size_t out_len = 0;
    for (size_t i = 0; input[i]; ++i) {
        if (!isdigit(input[i])) {
            continue;
        }
        if (out_len == 0) {
            if (input[i] == '0') {
                return out;
            } else if (input[i] == '1') {
                // skip
            } else {
                out[out_len++] = input[i];
            }
        } else if (out_len == 3) {
            if (input[i] == '0' || input[i] == '1') {
                memset(out, '0', 10);
                return out;
            } else {
                out[out_len++] = input[i];
            }
        } else if (out_len < 10) {
            out[out_len++] = input[i];
        } else {
            memset(out, '0', 10);
            return out;
        }
    }
    if (out_len != 10) {
        memset(out, '0', 10);
    }
    return out;
}
