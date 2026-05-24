#pragma once

#include <vector>

namespace all_your_base {

using namespace std;

inline vector<unsigned> convert(unsigned in_base, vector<unsigned> digits,
                                unsigned out_base) {
    if (in_base < 2) {
        throw invalid_argument{"in_base"};
    }
    if (out_base < 2) {
        throw invalid_argument{"out_base"};
    }
    unsigned n = 0;
    for (unsigned digit : digits) {
        if (digit >= in_base) {
            throw invalid_argument{"in_digit"};
        }
        n = n * in_base + digit;
    }
    vector<unsigned> out;
    while (n) {
        out.push_back(n % out_base);
        n /= out_base;
    }
    reverse(out.begin(), out.end());
    return out;
}

}  // namespace all_your_base
