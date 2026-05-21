#pragma once

#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <cctype>
#include <map>
#include <tuple>
#include <vector>

namespace etl {

using namespace boost;
using namespace boost::adaptors;
using namespace std;

template <typename Range, typename F>
auto flat_map2(const Range& rng, F f) {
    auto pairs =
        rng | transformed([f](const auto& e) { return f(e.first, e.second); });
    using Inner = typename range_value<decltype(pairs)>::type;
    using Value = typename range_value<Inner>::type;
    vector<Value> result;
    for_each(pairs, [&](auto inner) { copy(inner, back_inserter(result)); });
    return result;
}

inline map<char, int> transform(map<int, vector<char>> score_to_letters) {
    return copy_range<map<char, int>>(
        flat_map2(score_to_letters, [](int score, const auto& letters) {
            return letters | transformed([score](char c) {
                       return pair{char(tolower(c)), score};
                   });
        }));
}

}  // namespace etl
