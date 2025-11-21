#include "square_root.h"

unsigned square_root(unsigned n) {
    unsigned x = 0;
    while ((x + 1) * (x + 1) <= n) x++;
    return x;
}
