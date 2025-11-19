#include "hamming.h"

int compute(const char* lhs, const char* rhs) {
    int r = 0;
    while (*lhs || *rhs) {
        if (!*lhs || !*rhs) return -1;
        if (*lhs != *rhs) ++r;
        ++lhs;
        ++rhs;
    }
    return r;
}
