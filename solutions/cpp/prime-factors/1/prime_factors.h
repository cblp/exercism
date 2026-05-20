#pragma once

#include <vector>

namespace prime_factors {

using namespace std;

using number = long long;

inline vector<number> of(number n) {
    vector<number> factors;
    number x = 2;
    while (n > 1) {
        if (n % x == 0) {
            factors.push_back(x);
            n = n / x;
        } else {
            x++;
        }
    }
    return factors;
}

}  // namespace prime_factors
