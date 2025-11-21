#include "sieve.h"

#include <stdbool.h>

uint32_t sieve(uint32_t limit, uint32_t* primes, size_t max_primes) {
    if (limit < 2) return 0;

    size_t primes_count = 0;
    primes[primes_count++] = 2;
    for (uint32_t x = 3; x <= limit && primes_count < max_primes; x += 2) {
        bool x_is_prime = true;
        for (size_t prime_index = 0; prime_index < primes_count;
             ++prime_index) {
            if (x % primes[prime_index] == 0) {
                x_is_prime = false;
                break;
            }
        }
        if (x_is_prime) {
            primes[primes_count++] = x;
        }
    }
    return primes_count;
}
