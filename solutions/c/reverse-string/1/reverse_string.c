#include "reverse_string.h"

#include <stdlib.h>
#include <string.h>

char* reverse(const char* in) {
    size_t const size = strlen(in);
    char * const out = malloc(size + 1);
    for (size_t i = 0; i < size; ++i) {
        out[i] = in[size - 1 - i];
    }
    out[size] = 0;
    return out;
}
