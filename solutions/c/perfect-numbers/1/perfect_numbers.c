#include "perfect_numbers.h"

int aliquot_sum(int);
int aliquot_sum(int num) {
    if (num < 2) return 0;
    int r = 0;
    for (int x = 2; x * x <= num; ++x) {
        if (num % x == 0) {
            r += x;
            if (x * x < num) r += num / x;
        }
    }
    return r + 1;
}

kind classify_number(int num) {
    if (num <= 0) return ERROR;
    int const d = aliquot_sum(num) - num;
    return d == 0 ? PERFECT_NUMBER : d < 0 ? DEFICIENT_NUMBER : ABUNDANT_NUMBER;
}
