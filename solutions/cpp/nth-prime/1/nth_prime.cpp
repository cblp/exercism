#include "nth_prime.h"

#include <cstdlib>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std;

namespace nth_prime {

vector<unsigned> KnownPrimes = {2, 3};

bool is_multiple_of_a_known_prime(unsigned x) {
    for (const auto p : KnownPrimes) {
        if (x % p == 0) {
            return true;
        }
    }
    return false;
}

unsigned nth(unsigned const n) {
    if (n < 1) {
        throw domain_error{to_string(n)};
    }
    while (n > KnownPrimes.size()) {
        for (unsigned x = KnownPrimes.back() + 2;; x += 2) {
            if (!is_multiple_of_a_known_prime(x)) {
                KnownPrimes.push_back(x);
                break;
            }
        }
    }
    return KnownPrimes[n - 1];
}

}  // namespace nth_prime
