#pragma once

#include <map>
#include <string_view>
#include <vector>

namespace parallel_letter_frequency {

using namespace std;

using Result = map<char, unsigned>;

inline Result merge(Result a, const Result& b) {
    for (const auto& [c, n] : b) a[c] += n;
    return a;
}

Result frequency(vector<string_view> const& texts);

}  // namespace parallel_letter_frequency
