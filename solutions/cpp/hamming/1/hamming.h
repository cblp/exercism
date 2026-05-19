#pragma once

#include <boost/range/algorithm.hpp>
#include <boost/range/combine.hpp>
#include <stdexcept>
#include <string>

namespace hamming {

using namespace boost;
using namespace std;

inline int compute(string xs, string ys) {
    if (xs.size() != ys.size()) {
        throw domain_error{"bad args"};
    }
    return count_if(combine(xs, ys), [](const auto xy) {
        const auto [x, y] = xy;
        return x != y;
    });
}

}  // namespace hamming
