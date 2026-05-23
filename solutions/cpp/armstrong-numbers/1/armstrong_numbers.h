#pragma once

#include <boost/range/adaptors.hpp>
#include <boost/range/numeric.hpp>
#include <string>

namespace armstrong_numbers {

using namespace boost;
using namespace boost::range;
using namespace boost::adaptors;
using namespace std;

inline unsigned ipow(unsigned x, unsigned n) {
    switch (n) {
        case 1:
            return x;
        case 2:
            return x * x;
        case 3:
            return ipow(x, 2) * x;
        case 4:
            return ipow(ipow(x, 2), 2);
        case 7:
            return ipow(ipow(x, 2), 3) * x;
        default:
            throw to_string(n);
    }
}

inline bool is_armstrong_number(unsigned n) {
    if (n < 10) {
        return true;
    }
    const auto s = to_string(n);
    const auto armstrong = accumulate(
        s | transformed([n = s.size()](char c) { return ipow(c - '0', n); }),
        unsigned(0));
    return n == armstrong;
}

}  // namespace armstrong_numbers
