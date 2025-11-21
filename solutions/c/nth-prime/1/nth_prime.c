#include "nth_prime.h"

#include <stdbool.h>
#include <stdlib.h>

uint32_t PRIMES[20000] = {2, 3};
uint32_t PRIMES_COUNT = 2;

bool is_multiple_of_a_known_prime(uint32_t x) {
    for (size_t i = 0; i < PRIMES_COUNT; ++i) {
        if (x % PRIMES[i] == 0) {
            return true;
        }
    }
    return false;
}

uint32_t nth(uint32_t const n) {
    if (n < 1) {
        return 0;
    }
    while (n > PRIMES_COUNT) {
        for (uint32_t x = PRIMES[PRIMES_COUNT - 1] + 2;; x += 2) {
            if (!is_multiple_of_a_known_prime(x)) {
                PRIMES[PRIMES_COUNT++] = x;
                break;
            }
        }
    }
    return PRIMES[n - 1];
}
