#pragma once

#include <set>
#include <string>

namespace isogram {

using namespace std;

inline bool is_isogram(string input) {
    set<char> used;
    for (char c : input) {
        if (not isalpha(c)) {
            continue;
        }
        c = tolower(c);
        if (used.count(c)) {
            return false;
        }
        used.insert(c);
    }
    return true;
}

}  // namespace isogram
