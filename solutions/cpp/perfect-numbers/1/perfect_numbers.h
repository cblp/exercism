#pragma once

#include <stdexcept>

namespace perfect_numbers {

enum classification { deficient, perfect, abundant };

inline classification classify(int n) {
    if (n < 1) {
        throw std::domain_error{""};
    }
    int a = 0;
    for (int f = 1; f <= n / 2; f++) {
        if (n % f == 0) {
            a += f;
        }
    }
    return a < n ? deficient : a == n ? perfect : abundant;
}

}  // namespace perfect_numbers
