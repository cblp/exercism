#pragma once

#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/lambda2.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/numeric.hpp>
#include <string>
#include <vector>

namespace yacht {

using namespace boost;
using namespace boost::adaptors;
using namespace boost::algorithm;
using namespace boost::lambda2;
using namespace std;

inline int score(vector<int> dice, string category_name) {
    sort(dice);

    const auto sum_eq = [&](int d) {
        return accumulate(filter(dice, _1 == d), 0);
    };

    if (category_name == "ones") {
        return sum_eq(1);
    }
    if (category_name == "twos") {
        return sum_eq(2);
    }
    if (category_name == "threes") {
        return sum_eq(3);
    }
    if (category_name == "fours") {
        return sum_eq(4);
    }
    if (category_name == "fives") {
        return sum_eq(5);
    }
    if (category_name == "sixes") {
        return sum_eq(6);
    }
    if (category_name == "full house") {
        return dice[0] == dice[1]
                   and (dice[1] == dice[2] or dice[2] == dice[3])
                   and dice[3] == dice[4]
                   and dice[0] != dice[4]
                 ? accumulate(dice, 0)
                 : 0;
    }
    if (category_name == "four of a kind") {
        return dice[0] == dice[1]
                 ? (dice[1] == dice[2] and dice[2] == dice[3] ? dice[0] * 4 : 0)
             : dice[1] == dice[2] and dice[2] == dice[3] and dice[3] == dice[4]
                 ? dice[1] * 4
                 : 0;
    }
    if (category_name == "little straight") {
        return dice == vector{1, 2, 3, 4, 5} ? 30 : 0;
    }
    if (category_name == "big straight") {
        return dice == vector{2, 3, 4, 5, 6} ? 30 : 0;
    }
    if (category_name == "choice") {
        return accumulate(dice, 0);
    }
    if (category_name == "yacht") {
        return all_of_equal(dice, dice.front()) ? 50 : 0;
    }
    throw invalid_argument{category_name};
}

}  // namespace yacht
