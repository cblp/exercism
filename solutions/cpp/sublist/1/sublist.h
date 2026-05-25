#pragma once

#include <vector>

namespace sublist {

using namespace std;

enum class List_comparison { sublist, superlist, equal, unequal };

inline bool is_sublist(const vector<int>& first, const vector<int>& second) {
    for (size_t i = 0; i <= second.size() - first.size(); ++i) {
        if (equal(first.begin(), first.end(), second.begin() + i)) {
            return true;
        }
    }
    return false;
}

inline List_comparison sublist(const vector<int>& first,
                               const vector<int>& second) {
    return first.size() == second.size()
             ? (first == second ? List_comparison::equal
                                : List_comparison::unequal)
         : first.size() < second.size()
             ? (is_sublist(first, second) ? List_comparison::sublist
                                          : List_comparison::unequal)
             : (is_sublist(second, first) ? List_comparison::superlist
                                          : List_comparison::unequal);
}

}  // namespace sublist
