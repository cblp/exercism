#include "rail_fence_cipher.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static char* subst(const char* in, size_t rails, bool decode) {
    size_t const len = strlen(in);
    size_t const max_step = 2 * (rails - 1);

    char* out = calloc(1, len + 1);
    size_t cipher_index = 0;
    for (size_t r = 0; r < rails; ++r) {
        size_t step = 2 * r;
        for (size_t plain_index = r; plain_index < len; plain_index += step) {
            if (decode) {
                out[plain_index] = in[cipher_index];
            } else {
                out[cipher_index] = in[plain_index];
            }
            ++cipher_index;
            if (step != max_step) step = max_step - step;
        }
    }

    return out;
}

char* encode(const char* plain, size_t rails) {
    return subst(plain, rails, false);
}

char* decode(const char* cipher, size_t rails) {
    return subst(cipher, rails, true);
}
