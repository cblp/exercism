#include "rail_fence_cipher.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* encode(const char* plain, size_t rails) {
    size_t const len = strlen(plain);
    char* cipher = calloc(1, len + 1);
    size_t cipher_index = 0;
    for (size_t rail = 0; rail < rails; ++rail) {
        if (rail == 0 || rail == rails - 1) {
            for (size_t plain_index = rail; plain_index < len;
                 plain_index += 2 * (rails - 1)) {
                cipher[cipher_index] = plain[plain_index];
                ++cipher_index;
            }
        } else {
            size_t step = 2 * (rails - rail - 1);
            for (size_t plain_index = rail; plain_index < len;
                 plain_index += step, step = 2 * (rails - 1) - step) {
                cipher[cipher_index] = plain[plain_index];
                ++cipher_index;
            }
        }
    }

    return cipher;
}

char* decode(const char* cipher, size_t rails) {
    size_t const len = strlen(cipher);
    char* plain = calloc(1, len + 1);
    size_t cipher_index = 0;
    for (size_t rail = 0; rail < rails; ++rail) {
        if (rail == 0 || rail == rails - 1) {
            for (size_t plain_index = rail; plain_index < len;
                 plain_index += 2 * (rails - 1)) {
                plain[plain_index] = cipher[cipher_index];
                ++cipher_index;
            }
        } else {
            size_t step = 2 * (rails - rail - 1);
            for (size_t plain_index = rail; plain_index < len;
                 plain_index += step, step = 2 * (rails - 1) - step) {
                plain[plain_index] = cipher[cipher_index];
                ++cipher_index;
            }
        }
    }

    return plain;
}
