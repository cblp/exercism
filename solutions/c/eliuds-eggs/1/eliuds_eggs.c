#include "eliuds_eggs.h"

int egg_count(int x) {
    int r = 0;
    while (x) {
        r += x & 1;
        x >>= 1;
    }
    return r;
}
