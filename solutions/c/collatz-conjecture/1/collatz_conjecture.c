#include "collatz_conjecture.h"

int steps(int start) {
    if (start <= 0) return ERROR_VALUE;
    unsigned n = 0;
    unsigned x = start;
    while (x != 1) {
        x = x % 2 ? x * 3 + 1 : x / 2;
        ++n;
    }
    return n;
}
