#include "atbash_cipher.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static char mirror_char(char c) { return 219 - c; }

char* atbash_encode(const char* input) {
    char* out = malloc((strlen(input) / 5 + 1) * 6 + 1);
    size_t out_len = 0;
    size_t count = 0;
    for (const char* c = input; *c; ++c) {
        if (!isalnum(*c)) continue;
        if (count && count % 5 == 0) out[out_len++] = ' ';
        out[out_len++] = isalpha(*c) ? mirror_char(tolower(*c)) : *c;
        count++;
    }
    out[out_len] = 0;
    return out;
}

char* atbash_decode(const char* input) {
    char* out = malloc(strlen(input) + 1);
    size_t out_len = 0;
    for (const char* c = input; *c; ++c)
        if (isalnum(*c))
            out[out_len++] = isalpha(*c) ? mirror_char(tolower(*c)) : *c;
    out[out_len] = 0;
    return out;
}
