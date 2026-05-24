#pragma once

#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/lambda2.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/numeric.hpp>
#include <map>
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

    return map<string, int>{
        {"ones", sum_eq(1)},
        {"twos", sum_eq(2)},
        {"threes", sum_eq(3)},
        {"fours", sum_eq(4)},
        {"fives", sum_eq(5)},
        {"sixes", sum_eq(6)},
        {"full house", dice[0] == dice[1]
                               and (dice[1] == dice[2] or dice[2] == dice[3])
                               and dice[3] == dice[4]
                               and dice[0] != dice[4]
                           ? accumulate(dice, 0)
                           : 0},
        {"four of a kind",
         dice[0] == dice[1]
             ? (dice[1] == dice[2] and dice[2] == dice[3] ? dice[0] * 4 : 0)
         : dice[1] == dice[2] and dice[2] == dice[3] and dice[3] == dice[4]
             ? dice[1] * 4
             : 0},
        {"little straight", dice == vector{1, 2, 3, 4, 5} ? 30 : 0},
        {"big straight", dice == vector{2, 3, 4, 5, 6} ? 30 : 0},
        {"choice", accumulate(dice, 0)},
        {"yacht", all_of_equal(dice, dice.front()) ? 50 : 0},
    }[category_name];
}

}  // namespace yacht
