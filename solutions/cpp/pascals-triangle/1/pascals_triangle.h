#pragma once

#include <vector>

namespace pascals_triangle {

using namespace std;

inline vector<vector<int>> generate_rows(unsigned n) {
    if (n == 0) {
        return {};
    }
    vector<int> prev_row;
    vector<vector<int>> rows{{1}};
    for (unsigned i = 0; i + 1 < n; i++) {
        vector<int> next_row = {1};
        for (size_t j = 0; j + 1 < prev_row.size(); j++) {
            next_row.push_back(prev_row[j] + prev_row[j + 1]);
        }
        next_row.push_back(1);
        rows.push_back(next_row);
        prev_row = next_row;
    }
    return rows;
}

}  // namespace pascals_triangle
