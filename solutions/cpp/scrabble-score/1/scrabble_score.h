#pragma once

#include <boost/lambda2.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/numeric.hpp>
#include <string>

namespace scrabble_score {

using namespace boost::adaptors;

const unsigned scores[]{1, 3, 3, 2,  1, 4, 2, 4, 1, 8, 5, 1, 3,
                        1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10};

inline unsigned score(std::string word) {
    return accumulate(
        transform(word, [](char c) { return scores[tolower(c) - 'a']; }), 0);
}

}  // namespace scrabble_score
