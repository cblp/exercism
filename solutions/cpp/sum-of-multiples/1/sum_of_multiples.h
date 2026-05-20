#pragma once

#include <set>

namespace sum_of_multiples {

using namespace std;

inline bool is_multiple_of_any(unsigned i, set<unsigned> ms) {
    for (const auto& m : ms) {
        if (i % m == 0) {
            return true;
        }
    }
    return false;
}

inline unsigned to(set<unsigned> ms, unsigned n) {
    unsigned s = 0;
    for (unsigned i = 1; i < n; i++) {
        if (is_multiple_of_any(i, ms)) {
            s += i;
        }
    }
    return s;
}

}  // namespace sum_of_multiples
