#pragma once

#include <vector>

namespace sieve {

using namespace std;

template <class SinglePassRange, class UnaryPredicate>
inline bool any(SinglePassRange& rng, UnaryPredicate pred) {
    return find_if(begin(rng), end(rng), pred) != end(rng);
}

inline vector<int> primes(int limit) {
    if (limit < 2) return {};

    vector<int> primes{2};
    for (int x = 3; x <= limit; x += 2) {
        if (not any(primes, [x](const auto p) { return x % p == 0; })) {
            primes.push_back(x);
        }
    }
    return primes;
}

}  // namespace sieve
