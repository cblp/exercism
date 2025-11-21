#include "prime_factors.h"

uint64_t primes[1000] = {2, 3};
size_t primes_count = 2;

size_t find_factors(uint64_t n, uint64_t factors[static MAXFACTORS]) {
    size_t factors_count = 0;

    // check known primes first
    for (size_t i = 0; i < primes_count; ++i) {
        uint64_t const p = primes[i];
        if (p > n) break;
        while (n % p == 0) {
            factors[factors_count++] = p;
            n /= p;
        }
    }

    // find more primes
    uint64_t x = primes[primes_count - 1] + 2;
    while (n != 1) {
        for (size_t i = 0; i < primes_count; ++i) {
            if (x % primes[i] == 0) {
                goto x_is_not_prime;
            }
        }
        while (n % x == 0) {
            factors[factors_count++] = x;
            n /= x;
        }
    x_is_not_prime:
        x += 2;
    }
    return factors_count;
}
