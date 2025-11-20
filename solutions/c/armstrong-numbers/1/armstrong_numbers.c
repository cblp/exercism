#include "armstrong_numbers.h"

#include <assert.h>

int pow2(int n);
int pow2(int n) { return n * n; }

int pow3(int n);
int pow3(int n) { return n * n * n; }

int pow4(int n);
int pow4(int n) { return pow2(pow2(n)); }

int pow5(int n);
int pow5(int n) { return pow4(n) * n; }

int pow6(int n);
int pow6(int n) { return pow4(n) * pow2(n); }

int pow7(int n);
int pow7(int n) { return pow4(n) * pow3(n); }

int armstrong_sum(int n);
int armstrong_sum(int n) {
    if (n < 10) return n;
    if (n < 100) return pow2(n % 10) + pow2(n / 10);
    if (n < 1000) return pow3(n % 10) + pow3(n / 10 % 10) + pow3(n / 100);
    if (n < 10 * 1000)
        return pow4(n % 10) + pow4(n / 10 % 10) + pow4(n / 100 % 10) +
               pow4(n / 1000);
    if (n < 100 * 1000)
        return pow5(n % 10) + pow5(n / 10 % 10) + pow5(n / 100 % 10) +
               pow5(n / 1000 % 10) + pow5(n / (10 * 1000));
    if (n < 1000 * 1000)
        return pow6(n % 10) + pow6(n / 10 % 10) + pow6(n / 100 % 10) +
               pow6(n / 1000 % 10) + pow6(n / (10 * 1000) % 10) +
               pow6(n / (100 * 1000));
    if (n < 10 * 1000 * 1000)
        return pow7(n % 10) + pow7(n / 10 % 10) + pow7(n / 100 % 10) +
               pow7(n / 1000 % 10) + pow7(n / (10 * 1000) % 10) +
               pow7(n / (100 * 1000) % 10) + pow7(n / (1000 * 1000));
    assert(0);
}

bool is_armstrong_number(int candidate) {
    return armstrong_sum(candidate) == candidate;
}
