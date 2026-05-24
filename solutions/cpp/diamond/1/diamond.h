#pragma once

#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/join.hpp>
#include <string>
#include <vector>

namespace diamond {

using namespace boost;
using namespace boost::adaptors;
using namespace std;

inline string row(int size, int step) {
    const char letter = 'A' + step;
    return string(size - step, ' ')
         + letter
         + (step == 0 ? "" : string(2 * step - 1, ' ') + letter)
         + string(size - step, ' ');
}

inline vector<string> rows(char letter) {
    const auto size = letter - 'A';
    return copy_range<vector<string>>(
        transform(join(irange(size + 1), irange(size - 1, -1, -1)),
                  [=](int i) { return row(size, i); }));
}

}  // namespace diamond
