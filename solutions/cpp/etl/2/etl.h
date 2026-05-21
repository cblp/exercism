#pragma once

#include <map>
#include <vector>
#include <cctype>

namespace etl {

using namespace std;

inline map<char, int> transform(map<int, vector<char>> score_to_letters) {
    map<char, int> letter_to_score;
    for (const auto& [score, letters] : score_to_letters) {
        for (const auto letter : letters) {
            letter_to_score[tolower(letter)] = score;
        }
    }
    return letter_to_score;
}

}  // namespace etl
