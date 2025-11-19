#include "binary.h"

int convert(const char* input) {
    int r = 0;
    for (const char* i = input; *i; ++i) {
        if ('0' <= *i && *i <= '1')
            r = r * 2 + (*i - '0');
        else
            return -1;
    }
    return r;
}
