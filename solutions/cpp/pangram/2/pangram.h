#pragma once

#include <boost/range/irange.hpp>
#include <set>

namespace pangram {

using namespace boost;
using namespace std;

inline bool is_pangram(string s) {
    auto alphabet = copy_range<set<char>>(irange('a', 'z'));
    for (auto c : s) {
        c = tolower(c);
        if (isalpha(c)) {
            alphabet.erase(c);
        }
    }
    return alphabet.empty();
}

}  // namespace pangram
