#include "crypto_square.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define MAX_OUT_SIZE 64

char* ciphertext(const char* in) {
    // calculate message len
    size_t msg_len = 0;
    for (size_t i = 0; in[i]; i++) {
        if (isalnum(in[i])) {
            msg_len++;
        }
    }

    if (msg_len == 0) {
        return calloc(1, 1);
    }

    // calculate square sizes c, r
    size_t c = 0;
    size_t r = 0;
    while (c * r < msg_len)
        if (r < c)
            r++;
        else
            c++;

    // make the square
    size_t const ciphertext_len = c * (r + 1) - 1;
    char* out = malloc(ciphertext_len + 1);
    memset(out, ' ', ciphertext_len);
    out[ciphertext_len] = 0;
    for (size_t input_index = 0, message_index = 0; in[input_index];
         input_index++) {
        if (isalnum(in[input_index])) {
            out[(message_index * (r + 1)) % ciphertext_len] =
                tolower(in[input_index]);
            message_index++;
        }
    }
    return out;
}
