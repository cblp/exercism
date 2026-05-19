#pragma once

#include <stdexcept>
#include <string>

namespace collatz_conjecture {

using namespace std;

inline int steps(int n) {
    if (n < 1) {
        throw domain_error{to_string(n)};
    }
    if (n == 1) {
        return 0;
    }
    if (n % 2 == 0) {
        return 1 + steps(n / 2);
    }
    return 1 + steps(3 * n + 1);
}

}  // namespace collatz_conjecture
